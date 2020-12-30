// Copyright 2020 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// Returns true if s starts with the given prefix.
static bool startswith(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

// Returns true if s ends with the given suffix.
static bool endswith(const char *s, const char *suffix) {
    int n = strlen(s);
    int m = strlen(suffix);
    return n >= m && strncmp(s + (n - m), suffix, m) == 0;
}

// Concatenates two strings into a newly allocated string.
static char *concat(const char *s1, const char *s2) {
    int n1 = strlen(s1);
    int n2 = strlen(s2);
    char *dest = malloc(n1 + n2 + 1);
    memcpy(dest, s1, n1);
    memcpy(dest + n1, s2, n2);
    dest[n1 + n2] = '\0';
    return dest;
}

// A pointer-and-length string. Not necessarily null terminated.
struct Span {
    char *data;
    int len;
};

// An absent span. Distinct from SPAN("").
#define NULL_SPAN ((struct Span){NULL, 0})

// Creates a span from a string literal or array.
#define SPAN(s) ((struct Span){s, sizeof s - 1})

// Converts s to lowercase using the given buffer.
static struct Span tolower_s(struct Span s, char *buf, int cap) {
    assert(cap >= s.len);
    for (int i = 0; i < s.len; i++) {
        buf[i] = tolower(s.data[i]);
    }
    return (struct Span){buf, s.len};
}

// Name of the pandoc executable.
static const char PANDOC[] = "pandoc";

// Pandoc options that differ between invocations.
struct PandocOpts {
    // Path to the input file.
    const char *input;
    // Path to the output file.
    const char *output;
    // Contents of <title>...</title>.
    const char *title;
    // String that identifies the active tab. Allowed values: "index", "text",
    // "lecture", and "exercise".
    const char *active;
    // Relative path to the root of the website, e.g. "", "../", "../../", etc.
    const char *root;
    // Links to up/prev/next page. If any are set, up must be set.
    const char *up;
    const char *prev;
    const char *next;
};

// Invokes pandoc, printing the command before executing it. Normally does not
// return since it replaces the current process. Returns false on error.
static bool pandoc(const struct PandocOpts opts) {
    const int LEN =
        1     // pandoc
        + 3   // -o output -dconfig
        + 6   // -M title -M active -M root
        + 6   // -M prev -M up -M next
        + 1   // input
        + 1;  // NULL
    const char *argv[LEN];
    int i = 0;
    argv[i++] = PANDOC;
    argv[i++] = "-o";
    argv[i++] = opts.output;
    argv[i++] = "-dnotes/pandoc/config.yml";
    // Note: We don't need to free memory allocated by concat because it will
    // all disappear when execvp replaces the process image.
    argv[i++] = "-M";
    const int title_idx = i;
    argv[i++] = concat("title=", opts.title);
    argv[i++] = "-M";
    argv[i++] = opts.active;
    argv[i++] = "-M";
    argv[i++] = concat("root=", opts.root);
    if (opts.up) {
        argv[i++] = "-M";
        argv[i++] = concat("up=", opts.up);
        if (opts.prev) {
            argv[i++] = "-M";
            argv[i++] = concat("prev=", opts.prev);
        }
        if (opts.next) {
            argv[i++] = "-M";
            argv[i++] = concat("next=", opts.next);
        }
    }
    argv[i++] = opts.input;
    argv[i++] = NULL;
    assert(i <= LEN);
    for (int j = 0; j < i - 1; j++) {
        // Quote the title argument since it has spaces.
        const char *quote = j == title_idx ? "'" : "";
        const char *space = j == i - 2 ? "" : " ";
        printf("%s%s%s%s", quote, argv[j], quote, space);
    }
    putchar('\n');
    // It is safe to cast to (char **) because execvp will not modify argv
    // except as a consequence of replacing the process image.
    execvp(PANDOC, (char **)argv);
    perror(PANDOC);
    return false;
}

// Represents a child process executing pandoc.
struct PandocProc {
    // PID of the child process.
    pid_t pid;
    // Stream for writing to the child's stdin.
    FILE *in;
};

// Runs pandoc with opts in a child process, storing its information in proc.
// The caller should call wait_pandoc after. Returns true on success.
static bool fork_pandoc(struct PandocProc *proc, struct PandocOpts opts) {
    // Fork the process, using a pipe for the child's stdin.
    int fd[2];
    if (pipe(fd) == -1) {
        perror("pipe");
        return false;
    }
    if ((proc->pid = fork()) == -1) {
        perror("fork");
        close(fd[0]);
        close(fd[1]);
        return false;
    }
    if (proc->pid == 0) {
        // Close the pipe's write-side.
        close(fd[1]);
        // Move the pipe's read-side to file descriptor 0 (stdin).
        dup2(fd[0], 0);
        close(fd[0]);
        // Replace this process with pandoc.
        return pandoc(opts);
    }
    // Close the pipe's read-side and store the write-side.
    close(fd[0]);
    proc->in = fdopen(fd[1], "w");
    if (!proc->in) {
        perror("fdopen");
        close(fd[1]);
        return false;
    }
    return true;
}

// Closes proc's pipe and waits for it to finish. Returns true on success.
static bool wait_pandoc(struct PandocProc *proc) {
    fclose(proc->in);
    if (waitpid(proc->pid, NULL, 0) == -1) {
        perror("waitpid");
        return false;
    };
    proc->pid = -1;
    proc->in = NULL;
    return true;
}

// A Markdown sector is represented by an integer s, where:
//
//     (s >> 0*MS_BITS) & MS_MASK  gives the h1 index,
//     (s >> 1*MS_BITS) & MS_MASK  gives the h2 index,
//     ...
//
// An index of 0 means that heading has not been encountered yet.
//
// Example:
//
//     Some text.      ms=0x0000
//                     ms=0x0000
//     # First         ms=0x0001
//                     ms=0x0001
//     ## A            ms=0x0101
//     ## B            ms=0x0201
//     # Second        ms=0x0002
//
typedef uint64_t MarkdownSector;
#define MS_MASK UINT64_C(0xff)
#define MS_BITS UINT64_C(8)
#define MS_INDEX(s, h) ((int)(((s) >> (h - 1)*MS_BITS) & MS_MASK))
#define MS_MASK_UPTO(h) ((UINT64_C(1) << h*MS_BITS) - 1)
#define MS_INCREMENT(h) (UINT64_C(1) << (h - 1)*MS_BITS)
#define MS_NEXT(s, h) ((s & MS_MASK_UPTO(h)) + MS_INCREMENT(h))

// Writes a dotted string representation of sector in buf. Writes no more than
// size characters (including the null terminator). For example, the sector
// 0x030201 becomes "1.2.3".
static void write_dotted_section(char *buf, int size, MarkdownSector section) {
    assert(size >= 2);
    assert(section != 0);
    int i = 0;
    i += snprintf(buf + i, size - i, "%d", (int)(section & MS_MASK));
    section >>= MS_BITS;
    while (section && i < size) {
        i += snprintf(buf + i, size - i, ".%d", (int)(section & MS_MASK));
        section >>= MS_BITS;
    }
}

// Markdown line scanner state.
struct MarkdownState {
    // The Markdown file.
    FILE *file;
    // Current line and its capacity.
    struct Span line;
    size_t cap;
    // True if we are inside a fenced code block.
    bool code;
    // Current sector within the document.
    MarkdownSector sector;
    // If this line is a heading, 1/2/... for h1/h2/..., otherwise 0.
    int heading;
};

// Initializes a Markdown line scanner. Returns true on success.
static bool init_md(struct MarkdownState *state, const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return false;
    }
    state->file = file;
    state->line = NULL_SPAN;
    state->code = false;
    state->sector = 0;
    state->heading = 0;
    return true;
}

// Advances a Markdown line scanner to the next line. Returns true on successs,
// and false on failure or EOF. Takes care of closing the file.
static bool scan_md(struct MarkdownState *state) {
    if (!state->file) {
        return false;
    }
    const bool prev_blank = state->line.len <= 1;
    state->line.len = getline(&state->line.data, &state->cap, state->file);
    state->heading = 0;
    if (state->line.len == -1) {
        fclose(state->file);
        state->file = NULL;
        return false;
    }
    if (startswith(state->line.data, "```")) {
        state->code = !state->code;
        return true;
    }
    if (!state->code && prev_blank) {
        int h = 0;
        while (h < state->line.len && state->line.data[h] == '#') h++;
        if (h > 0 && h < state->line.len && state->line.data[h] == ' ') {
            state->sector = MS_NEXT(state->sector, h);
            state->heading = h;
        }
    }
    return true;
}

// Copies the current line in state to out.
static void copy_md(struct MarkdownState *state, FILE *out) {
    fwrite(state->line.data, state->line.len, 1, out);
}

// A parsed Markdown heading.
struct MarkdownHeading {
    // If the heading looks like, "# 1A: Foo bar", this is "1A". Otherwise NULL.
    struct Span label;
    // If the heading looks like "# 1A: Foo bar", this is "Foo bar". If there is
    // no label, it is the whole heading with only the leading "# " removed.
    struct Span title;
};

// Creates a Markdown heading with title only, from a string literal.
#define TITLE_HEADING(t) \
    ((struct MarkdownHeading){.label = NULL_SPAN, .title = SPAN(t)})

// Parses a Markdown heading.
static struct MarkdownHeading parse_md_heading(struct Span s) {
    char *const p = s.data;
    const int n = s.len;
    assert(n >= 2 && p[0] == '#'  && p[n-1] == '\n');
    int i = 1;
    while (i < n && p[i] == '#') i++;
    assert(p[i++] == ' ' && i < n);
    if (p[i] >= '1' && p[i] <= '9') {
        int j = i;
        while (j < n && p[j] != ':') j++;
        if (j + 2 < n && p[j+1] == ' ') {
            return (struct MarkdownHeading){
                .label = {p + i, j - i},
                .title = {p + j + 2, n - j - 3},
            };
        }
    }
    return (struct MarkdownHeading){
        .label = NULL_SPAN,
        .title = {p + i, n - i - 1},
    };
}

// Buffer sizes for rendering various things.
#define SZ_HREF 256
#define SZ_RELATIVE 32
#define SZ_HEADING 64
#define SZ_LABEL 8

// Renders a heading to out. The level determines h1/h2/etc. If id is present,
// uses it and renders a "#" link. If heading.label is present, renders a
// .number span. If href_fmt is present, renders heading.title as a link using
// href_fmt and the printf-style arguments that follow.
static void render_heading(FILE *out, int level, struct Span id,
        struct MarkdownHeading heading, const char *href_fmt, ...) {
    if (id.data) {
        fprintf(out,
            "<h%d id=\"%.*s\" class=\"anchor\">"
            "<a class=\"anchor__link link\" href=\"#%.*s\""
            " aria-hidden=\"true\">#</a>",
            level, id.len, id.data, id.len, id.data);
    } else {
        fprintf(out, "<h%d>", level);
    }
    if (heading.label.data) {
        fprintf(out, "<span class=\"number\">%.*s</span> ",
            heading.label.len, heading.label.data);
    }
    if (href_fmt) {
        char href[SZ_HREF];
        va_list args;
        va_start(args, href_fmt);
        vsnprintf(href, sizeof href, href_fmt, args);
        va_end(args);
        fprintf(out,
            // &#65279; is the "zero width no-break space" entity. We put this
            // at the start of the span to prevent the external icon from
            // wrapping onto the next line by itself.
            "<a class=\"link\" href=\"%s\">%.*s<span class=\"nowrap\">&#65279;"
            "<svg alt=\"\" class=\"external\" width=\"24\" height=\"24\">"
            "<use xlink:href=\"#external\"/>"
            "</svg></span></a>",
            href, heading.title.len, heading.title.data); 
    } else {
        fwrite(heading.title.data, heading.title.len, 1, out);
    }
    fprintf(out, "</h%d>\n", level);
}

// State to keep track of while rendering a table of contents.
struct TocRenderer {
    // Nesting depth of <ul> tags.
    int depth;
};

// Creates a new table-of-contents renderer.
static struct TocRenderer new_toc_renderer(void) {
    return (struct TocRenderer){.depth = 0};
}

// Renders the start of a table of contents to out.
static void render_toc_start(struct TocRenderer *tr, FILE *out) {
    assert(tr->depth == 0);
    render_heading(out, 2, SPAN("contents"), TITLE_HEADING("Contents"), NULL);
    fputs("<nav aria-labelledby=\"contents\">", out);
}

// Renders the end of a table of contents to out.
static void render_toc_end(struct TocRenderer *tr, FILE *out) {
    for (; tr->depth > 1; tr->depth--) {
        fputs("</ul></li>", out);
    }
    assert(--tr->depth == 0);
    fputs("</ul></nav>\n", out);
}

// Renders an item in a table of contents to out. The depth can be at most one
// greater than the previous item. The link's href is given in printf style.
static void render_toc_item(
        struct TocRenderer *tr, FILE *out, int depth,
        struct MarkdownHeading heading, const char *href_fmt, ...) {
    char href[SZ_RELATIVE];
    va_list args;
    va_start(args, href_fmt);
    vsnprintf(href, sizeof href, href_fmt, args);
    va_end(args);
    switch (depth - tr->depth) {
    case 0:
        fputs("</li>", out);
        break;
    case 1:
        tr->depth++;
        fputs("<ul class=\"toc\">", out);
        break;
    default:
        assert(tr->depth > depth);
        for (; tr->depth > depth; tr->depth--) {
            fputs("</ul></li>", out);
        }
        break;
    }
    assert(tr->depth == depth);
    fprintf(out,
        "<li class=\"toc__item\">"
        // Put a space between <span> and <a> so that they don't run together
        // in alternative stylesheets like Safari Reader.
        "<span class=\"toc__label\">%.*s</span> "
        "<a href=\"%s\">%.*s</a>",
        heading.label.len, heading.label.data, href,
        heading.title.len, heading.title.data);
}

// Returns the Markdown sector for the given textbook chapter/section.
static MarkdownSector make_sector(int chapter, int section) {
    // Add 1 since "# Frontmatter" is the first h1.
    return (MarkdownSector)chapter + 1 | ((MarkdownSector)section << MS_BITS);
}

// Number of chapters in the textbook.
#define NUM_CHAPTERS 5

// Returns the number of sections in the given 1-based chapter.
static int num_sections(int chapter) {
    assert(chapter >= 1 && chapter <= 5);
    return (int[]){3, 5, 5, 4, 5}[chapter-1];
}

// Base URL for the online SICP textbook.
static const char TEXT_URL_BASE[] =
    "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H";

// Base URL for the online SICP video lectures.
static const char LECTURE_URL_BASE[] =
    "https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures";

// Returns the page number to use in the online SICP textbook URL for a given
// sector in text.md. Only takes into account the chapter and section.
static int text_page_num(MarkdownSector s) {
    int h1 = MS_INDEX(s, 1);
    int h2 = MS_INDEX(s, 2);
    assert(h1 > 0);
    if (h1 == 1) {
        assert(h2 >= 1 && h2 <= 3);
        return (int[]){3, 5, 7}[h2-1];
    }
    int chapter = h1 - 1;
    int section = h2;
    return 8 + chapter + (int[]){0, 3, 8, 13, 17}[chapter-1] + section;
}

// Writes into buf the page name to use in the online SICP video lecture URL for
// a given lecture heading.
static void lecture_page_name(
        struct MarkdownHeading heading, char *buf, int size) {
    assert(size > 0);
    int i = 0;
    for (int j = 0; j < heading.label.len; j++) {
        char c = tolower(heading.label.data[i]);
        buf[i++] = c;
        if (i == size) goto overflow;
    }
    buf[i++] = '-';
    if (i == size) goto overflow;
    for (int j = 0; j < heading.title.len; j++) {
        char c = tolower(heading.title.data[j]);
        if (isalpha(c) || isdigit(c)) {
            buf[i++] = c;
        } else if (buf[i-1] != '-') {
            buf[i++] = '-';
        }
        if (i == size) goto overflow;
    }
    buf[i++] = '\0';
    return;
overflow:
    assert(false);
}

// String constants, used to guard against misspellings.
#define INDEX "index"
#define TEXT "text"
#define LECTURE "lecture"
#define EXERCISE "exercise"
#define HIGHLIGHT "highlight"
#define FRONT "front"

// Helpers to construct input/output paths.
#define INPUT(f) ("notes/" f ".md")
#define OUTPUT(f) ("docs/" f ".html")
#define OUTPUT_DIR(d) ("docs/" d "/")

// Helpers to construct link targets.
#define PARENT "../"
#define HREF(p) (p ".html")

// Generates docs/index.html.
static bool gen_index(void) {
    return pandoc((struct PandocOpts){
        .input = INPUT(INDEX),
        .output = OUTPUT(INDEX),
        .title = "SICP Study",
        .active = INDEX,
        .root = "",
        .prev = NULL,
        .up = NULL,
        .next = NULL,
    });
}

// Generates docs/text/index.html.
static bool gen_text_index(void) {
    struct MarkdownState state;
    if (!init_md(&state, INPUT(TEXT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = OUTPUT(TEXT "/" INDEX),
        .title = "SICP Notes",
        .active = TEXT,
        .root = PARENT,
        .prev = NULL,
        .up = HREF(PARENT INDEX),
        .next = HREF(HIGHLIGHT),
    })) {
        return false;
    }
    render_heading(proc.in,
        1, NULL_SPAN, TITLE_HEADING("Textbook Notes"), NULL);
    while (scan_md(&state) && state.sector == 0) {
        copy_md(&state, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    render_toc_item(&tr, proc.in,
        1, TITLE_HEADING("Highlights"), HREF(HIGHLIGHT));
    do {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_toc_item(&tr, proc.in, 1, h,
                FRONT ".html#%.*s", id.len, id.data);
        }
    } while (scan_md(&state) && MS_INDEX(state.sector, 1) <= 1);
    do {
        if (state.heading == 1) {
            render_toc_item(&tr, proc.in, 1, parse_md_heading(state.line),
                "%d/" INDEX ".html", MS_INDEX(state.sector, 1) - 1);
        } else if (state.heading == 2) {
            render_toc_item(&tr, proc.in, 2, parse_md_heading(state.line),
                "%d/%d.html",
                MS_INDEX(state.sector, 1) - 1,
                MS_INDEX(state.sector, 2));
        }
    } while (scan_md(&state));
    render_toc_end(&tr, proc.in);
    return wait_pandoc(&proc);
}

// Generates docs/text/highlight.html.
static bool gen_text_highlight(void) {
    struct MarkdownState state;
    if (!init_md(&state, INPUT(HIGHLIGHT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = OUTPUT(TEXT "/" HIGHLIGHT),
        .title = "SICP Highlights",
        .active = TEXT,
        .root = PARENT,
        .prev = HREF(INDEX),
        .up = HREF(INDEX),
        .next = HREF(FRONT),
    })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Highlights"), NULL);
    while (scan_md(&state) && state.sector != 1);
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            if (h.label.data) {
                render_heading(proc.in, 2, h.label, h,
                    "%.*s/" INDEX ".html", h.label.len, h.label.data);
            } else {
                char buf[SZ_HEADING];
                struct Span id = tolower_s(h.title, buf, sizeof buf);
                render_heading(proc.in, 2, id, h,
                    FRONT ".html#%.*s", id.len, id.data);
            }
        } else {
            copy_md(&state, proc.in);
        }
    }
    return wait_pandoc(&proc);
}

// Generates docs/text/front.html.
static bool gen_text_front(void) {
    struct MarkdownState state;
    if (!init_md(&state, INPUT(TEXT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = OUTPUT(TEXT "/" FRONT),
        .title = "SICP Frontmatter Notes",
        .active = TEXT,
        .root = PARENT,
        .prev = HREF(HIGHLIGHT),
        .up = HREF(INDEX),
        .next = HREF("1/" INDEX),
    })) {
        return false;
    }
    while (scan_md(&state) && state.sector != 1);
    do {
        if (state.heading > 1) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_heading(proc.in, state.heading, id, h,
                "%s-%d.html", TEXT_URL_BASE, text_page_num(state.sector));
        } else {
            copy_md(&state, proc.in);
        }
    } while (scan_md(&state) && state.heading != 1);
    return wait_pandoc(&proc);
}

// Generates docs/text/*/index.html.
static bool gen_text_chapter(const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash > output && strcmp(slash, "/" INDEX ".html") == 0)) {
        goto invalid;
    }
    const int chapter = slash[-1] - '0';
    if (!(chapter >= 1 && chapter <= NUM_CHAPTERS)) {
        goto invalid;
    }
    struct MarkdownState state;
    if (!init_md(&state, INPUT(TEXT))) {
        return false;
    }
    char title[] = "SICP Chapter _ Notes";
    title[13] = '0' + chapter;
    const char *prev;
    char prev_buf[] = "../_/_.html";
    if (chapter == 1) {
        prev = "../" FRONT ".html";
    } else {
        prev_buf[3] = '0' + chapter - 1;
        prev_buf[5] = '0' + num_sections(chapter - 1);
        prev = prev_buf;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = TEXT,
        .root = PARENT PARENT,
        .prev = prev,
        .up = HREF(PARENT INDEX),
        .next = HREF("1"),
    })) {
        return false;
    }
    const MarkdownSector target_sector = make_sector(chapter, 0);
    while (scan_md(&state) && state.sector != target_sector);
    assert(state.sector == target_sector);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h,
        "%s-%d.html", TEXT_URL_BASE, text_page_num(target_sector));
    while (scan_md(&state) && state.heading == 0) {
        copy_md(&state, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    do {
        if (state.heading == 2) {
            int section = MS_INDEX(state.sector, 2);
            render_toc_item(&tr, proc.in, 1, parse_md_heading(state.line),
                "%d.html", section);
        } else if (state.heading == 3) {
            int section = MS_INDEX(state.sector, 2);
            int subsection = MS_INDEX(state.sector, 3);
            render_toc_item(&tr, proc.in, 2, parse_md_heading(state.line),
                "%d.html#%d.%d.%d", section, chapter, section, subsection);
        }
    } while (scan_md(&state) && state.heading != 1);
    render_toc_end(&tr, proc.in);
    return wait_pandoc(&proc);
invalid:
    fprintf(stderr, "%s: invalid text chapter\n", output);
    return false;
}

// Generates docs/text/*/*.html.
static bool gen_text_section(const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash > output && slash[1])) {
        goto invalid;
    }
    const int chapter = slash[-1] - '0';
    const int section = slash[1] - '0';
    const int last_section = num_sections(chapter);
    if (!(chapter >= 1 && chapter <= 5
            && section >= 1 && section <= last_section)) {
        goto invalid;
    }
    struct MarkdownState state;
    if (!init_md(&state, INPUT(TEXT))) {
        return false;
    }
    char title[] = "SICP Section _._ Notes";
    title[13] = '0' + chapter;
    title[15] = '0' + section;
    const char *prev;
    char prev_buf[] = "_.html";
    if (section == 1) {
        prev = HREF(INDEX);
    } else {
        prev_buf[0] = '0' + section - 1;
        prev = prev_buf;
    }
    char *next;
    char next_buf[] = "../_/" INDEX ".html";
    if (chapter == NUM_CHAPTERS && section == last_section) {
        next = NULL;
    } else if (section == last_section) {
        next_buf[3] = '0' + chapter + 1;
        next = next_buf;
    } else {
        snprintf(next_buf, sizeof next_buf, "%d.html", section + 1);
        next = next_buf;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = TEXT,
        .root = PARENT PARENT,
        .prev = prev,
        .up = HREF(INDEX),
        .next = next,
    })) {
        return false;
    }
    const MarkdownSector target_sector = make_sector(chapter, section);
    while (scan_md(&state) && state.sector != target_sector);
    assert(state.sector == target_sector);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    const int page_num = text_page_num(target_sector);
    render_heading(proc.in, 1, NULL_SPAN, h,
        "%s-%d.html", TEXT_URL_BASE, page_num);
    while (scan_md(&state) && state.heading != 1 && state.heading != 2) {
        if (state.heading >= 3) {
            h = parse_md_heading(state.line);
            if (h.label.data) {
                assert(state.heading == 3);
                render_heading(proc.in, 2, h.label, h,
                    "%s-%d.html#%%_sec_%d.%d.%d", TEXT_URL_BASE, page_num,
                    chapter, section, MS_INDEX(state.sector, 3));
            } else {
                assert(state.heading == 4);
                char id[SZ_LABEL];
                snprintf(id, sizeof id, "%d.%d.%d.%d", chapter, section,
                    MS_INDEX(state.sector, 3), MS_INDEX(state.sector, 4));
                render_heading(proc.in, 3, SPAN(id), h, NULL);
            }
        } else {
            copy_md(&state, proc.in);
        }
    }
    return wait_pandoc(&proc);
invalid:
    fprintf(stderr, "%s: invalid text section\n", output);
    return false;
}

// Generates docs/lecture/index.html.
static bool gen_lecture_index(void) {
    struct MarkdownState state;
    if (!init_md(&state, INPUT(LECTURE))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = OUTPUT(LECTURE "/" INDEX),
        .title = "SICP Lecture Notes",
        .active = LECTURE,
        .root = PARENT,
        .prev = NULL,
        .up = HREF(PARENT INDEX),
        .next = HREF(HIGHLIGHT),
    })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Lecture Notes"), NULL);
    while (scan_md(&state) && state.sector == 0) {
        copy_md(&state, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    render_toc_item(&tr, proc.in,
        1, TITLE_HEADING("Highlights"), HREF(HIGHLIGHT));
    do {
        if (state.heading == 1) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            char buf[SZ_LABEL];
            struct Span href = tolower_s(h.label, buf, sizeof buf);
            render_toc_item(&tr, proc.in, 1, h, 
                "%.*s.html", href.len, href.data);
        }
    } while (scan_md(&state));
    render_toc_end(&tr, proc.in);
    return wait_pandoc(&proc);
}

// Generates docs/lecture/highlight.html.
static bool gen_lecture_highlight(void) {
    struct MarkdownState state;
    if (!init_md(&state, INPUT(HIGHLIGHT))) {
        return false;
    }
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = OUTPUT(LECTURE "/" HIGHLIGHT),
        .title = "SICP Lecture Highlights",
        .active = LECTURE,
        .root = PARENT,
        .prev = HREF(INDEX),
        .up = HREF(INDEX),
        .next = HREF("1a"),
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Highlights"), NULL);
    while (scan_md(&state) && state.sector != 2);
    while (scan_md(&state)) {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(h.label.data);
            char buf[SZ_LABEL];
            struct Span id = tolower_s(h.label, buf, sizeof buf);
            render_heading(proc.in, 2, id, h, "%.*s.html", id.len, id.data);
        } else {
            copy_md(&state, proc.in);
        }
    }
    return wait_pandoc(&proc);
}

// Generates docs/lecture/*.html.
static bool gen_lecture_page(const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash[1] && slash[2] && slash[3])) {
        goto invalid;
    }
    const char *endptr;
    const int number = strtol(slash + 1, (char **)&endptr, 10);
    if (number == 0 || !*endptr) {
        goto invalid;
    }
    const char a_or_b = *endptr;
    if (a_or_b != 'a' && a_or_b != 'b') {
        goto invalid;
    }
    struct MarkdownState state;
    if (!init_md(&state, INPUT(LECTURE))) {
        return false;
    }
    char title[SZ_HEADING];
    snprintf(title, sizeof title, "SICP Lecture %d%c Notes",
        number, toupper(a_or_b));
    const char *prev;
    char prev_buf[SZ_RELATIVE];
    if (number == 1 && a_or_b == 'a') {
        prev = HREF(HIGHLIGHT);
    } else {
        int ab = a_or_b - 'a';
        snprintf(prev_buf, sizeof prev_buf, "%d%c.html",
            number + ab - 1, 'b' - ab);
        prev = prev_buf;
    }
    const char *next;
    char next_buf[SZ_RELATIVE];
    if (number == 10 && a_or_b == 'b') {
        next = NULL;
    } else {
        int ab = a_or_b - 'a';
        snprintf(next_buf, sizeof next_buf, "%d%c.html",
            number + ab, 'b' - ab);
        next = next_buf;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = LECTURE,
        .root = PARENT,
        .prev = prev,
        .up = HREF(INDEX),
        .next = next,
    })) {
        return false;
    }
    const MarkdownSector target_sector = 1 + (number - 1) * 2 + a_or_b - 'a';
    while (scan_md(&state) && state.sector != target_sector);
    assert(state.sector == target_sector);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    char lecture_page[SZ_HEADING];
    lecture_page_name(h, lecture_page, sizeof lecture_page);
    render_heading(proc.in, 1, NULL_SPAN, h,
        "%s/%s", LECTURE_URL_BASE, lecture_page);
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading > 1) {
            h = parse_md_heading(state.line);
            assert(!h.label.data);
            char id[SZ_LABEL];
            write_dotted_section(id, sizeof id, state.sector >> MS_BITS);
            render_heading(proc.in, state.heading, SPAN(id), h, NULL);
        } else {
            copy_md(&state, proc.in);
        }
    }
    return wait_pandoc(&proc);
invalid:
    fprintf(stderr, "%s: invalid lecture\n", output);
    return false;
}

// Generates docs/exercise/index.html.
static bool gen_exercise_index(void) {
    return true;
}

// Generates docs/exercise/*/index.html.
static bool gen_exercise_chapter(const char *output) {
    return true;
}

// Generates docs/exercise/*/*.html.
static bool gen_exercise_section(const char *output) {
    return true;
}

// Generates the given output file.
static bool gen(const char *output) {
    if (strcmp(output, OUTPUT(INDEX)) == 0) {
        return gen_index();
    }
    if (startswith(output, OUTPUT_DIR(TEXT))) {
        if (strcmp(output, OUTPUT(TEXT "/" INDEX)) == 0) {
            return gen_text_index();
        }
        if (strcmp(output, OUTPUT(TEXT "/" HIGHLIGHT)) == 0) {
            return gen_text_highlight();
        }
        if (strcmp(output, OUTPUT(TEXT "/" FRONT)) == 0) {
            return gen_text_front();
        }
        if (endswith(output, "/" INDEX ".html")) {
            return gen_text_chapter(output);
        }
        return gen_text_section(output);
    }
    if (startswith(output, OUTPUT_DIR(LECTURE))) {
        if (strcmp(output, OUTPUT(LECTURE "/" INDEX)) == 0) {
            return gen_lecture_index();
        }
        if (strcmp(output, OUTPUT(LECTURE "/" HIGHLIGHT)) == 0) {
            return gen_lecture_highlight();
        }
        return gen_lecture_page(output);
    }
    if (startswith(output, OUTPUT_DIR(EXERCISE))) {
        if (strcmp(output, OUTPUT(EXERCISE "/" INDEX)) == 0) {
            return gen_exercise_index();
        }
        if (endswith(output, "/" INDEX ".html")) {
            return gen_exercise_chapter(output);
        }
        return gen_exercise_section(output);
    }
    fprintf(stderr, "%s: invalid output file\n", output);
    return false;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s OUT_FILE\n", argv[0]);
        return 1;
    }
    const char *output = argv[1];
    char *parent = strdup(output);
    parent = dirname(parent);
    if (mkdir(parent, 0777) == -1 && errno != EEXIST) {
        perror(parent);
        free(parent);
        return 1;
    }
    free(parent);
    return gen(output) ? 0 : 1;
}
