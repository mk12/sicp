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

// An absent span (distinct from an empty span).
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
static const char *const PANDOC = "pandoc";

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
        + 6   // -M title -V active -V root
        + 6   // -V prev -V up -V next
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
    argv[i++] = "-V";
    argv[i++] = opts.active;
    argv[i++] = "-V";
    argv[i++] = concat("root=", opts.root);
    if (opts.up) {
        argv[i++] = "-V";
        argv[i++] = concat("up=", opts.up);
        if (opts.prev) {
            argv[i++] = "-V";
            argv[i++] = concat("prev=", opts.prev);
        }
        if (opts.next) {
            argv[i++] = "-V";
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

// A Markdown section is represented by an integer s, where:
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
typedef uint64_t MarkdownSection;
#define MS_MASK UINT64_C(0xff)
#define MS_BITS UINT64_C(8)
#define MS_INDEX(s, h) ((int)(((s) >> (h - 1)*MS_BITS) & MS_MASK))
#define MS_MASK_UPTO(h) ((UINT64_C(1) << h*MS_BITS) - 1)
#define MS_INCREMENT(h) (UINT64_C(1) << (h - 1)*MS_BITS)
#define MS_NEXT(s, h) ((s & MS_MASK_UPTO(h)) + MS_INCREMENT(h))

// Writes a dotted string representation of the section in buf. Writes no more
// than size characters (including the null terminator). For example, the
// section 0x030201 becomes "1.2.3".
static void write_dotted_section(char *buf, int size, MarkdownSection section) {
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
    // Current section within the document.
    MarkdownSection section;
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
    state->section = 0;
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
            state->section = MS_NEXT(state->section, h);
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

// Helpers to construct input/output paths.
#define MARKDOWN(f) ("notes/" f ".md")
#define HTML(f) ("docs/" f ".html")
#define SUBDIR(d) ("docs/" d "/")

// Number of chapters in the textbook.
#define NUM_CHAPTERS 5

// Returns the number of sections in the given 1-based chapter.
static int num_sections(int chapter) {
    return (int[]){3, 5, 5, 4, 5}[chapter-1];
}

// Returns the Markdown section for the given textbook chapter/section.
static MarkdownSection text_section(int chapter, int section) {
    // Add 1 since "# Frontmatter" is the first h1.
    return (MarkdownSection)chapter + 1 | ((MarkdownSection)section << MS_BITS);
}

// Buffer sizes for rendering various things.
#define SZ_HREF 64
#define SZ_HEADING 64
#define SZ_LABEL 8

// Renders a heading. Includes a "#" anchor link if id is present, and a .number
// span if heading.label is present.
static void render_heading(FILE *out, int level, struct Span id,
        struct MarkdownHeading heading) {
    if (id.data) {
        fprintf(out,
            "<h%d id=\"%.*s\" class=\"anchor\">"
            "<a class=\"anchor__link link\" href=\"#%.*s\">#</a>",
            level, id.len, id.data, id.len, id.data);
    } else {
        fprintf(out, "<h%d>", level);
    }
    if (heading.label.data) {
        fprintf(out, "<span class=\"number\">%.*s</span> ",
            heading.label.len, heading.label.data);
    }
    fprintf(out, "%.*s</h%d>\n", heading.title.len, heading.title.data, level);
}

// Renders a linked heading for one of of the quote.html pages. The id must be
// present. Includes a .number span if heading.label is present. The link's href
// is given in printf style.
static void render_quote_heading(FILE *out, int level, struct Span id,
        struct MarkdownHeading heading, const char *href_fmt, ...) {
    char href[SZ_HREF];
    va_list args;
    va_start(args, href_fmt);
    vsnprintf(href, sizeof href, href_fmt, args);
    va_end(args);
    fprintf(out,
        "<h%d id=\"%.*s\" class=\"anchor\">"
        "<a class=\"anchor__link link\" href=\"#%.*s\">#</a>",
        level, id.len, id.data, id.len, id.data);
    if (heading.label.data) {
        fprintf(out, "<span class=\"number\">%.*s</span> ",
            heading.label.len, heading.label.data);
    }
    fprintf(out,
        "<a class=\"link\" href=\"%s\">%.*s</a>"
        "</h%d>\n",
        href, heading.title.len, heading.title.data, level);
}

// State to keep track of while rendering a table of contents.
struct TocState {
    // Output stream.
    FILE *out;
    // Nesting depth of <ul> tags.
    int depth;
};

// Renders the start of a table of contents.
static struct TocState render_toc_start(FILE *out) {
    struct MarkdownHeading h = {.label = NULL_SPAN, .title = SPAN("Contents")};
    render_heading(out, 2, SPAN("contents"), h);
    return (struct TocState){.out = out, .depth = 0};
}

// Renders the end of a table of contents.
static void render_toc_end(struct TocState *toc) {
    for (; toc->depth > 1; toc->depth--) {
        fputs("</ul></li>", toc->out);
    }
    assert(--toc->depth == 0);
    fputs("</ul>\n", toc->out);
}

// Renders an item in a table of contents. The depth can be at most one greater
// than the previous item. The link's href is given in printf style.
static void render_toc_item(
        struct TocState *toc, int depth, struct MarkdownHeading heading,
        const char *href_fmt, ...) {
    char href[SZ_HREF];
    va_list args;
    va_start(args, href_fmt);
    vsnprintf(href, sizeof href, href_fmt, args);
    va_end(args);
    switch (depth - toc->depth) {
    case 0:
        fputs("</li>", toc->out);
        break;
    case 1:
        toc->depth++;
        fputs("<ul class=\"toc\">", toc->out);
        break;
    default:
        assert(toc->depth > depth);
        for (; toc->depth > depth; toc->depth--) {
            fputs("</ul></li>", toc->out);
        }
        break;
    }
    assert(toc->depth == depth);
    fprintf(toc->out,
        "<li class=\"toc__item\">"
        // Put a space between <span> and <a> so that they don't run together
        // in alternative stylesheets like Safari Reader.
        "<span class=\"toc__label\">%.*s</span> "
        "<a href=\"%s\">%.*s</a>",
        heading.label.len, heading.label.data, href,
        heading.title.len, heading.title.data);
}

// Generates docs/index.html.
static bool gen_index(void) {
    return pandoc((struct PandocOpts){
        .input = MARKDOWN("index"),
        .output = HTML("index"),
        .title = "SICP Study",
        .active = "index",
        .root = "",
        .prev = NULL,
        .up = NULL,
        .next = NULL,
    });
}

// Generates docs/text/index.html.
static bool gen_text_index(void) {
    struct MarkdownState state;
    if (!init_md(&state, MARKDOWN("text"))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = HTML("text/index"),
        .title = "SICP Notes",
        .active = "text",
        .root = "../",
        .prev = NULL,
        .up = "../index.html",
        .next = "quote.html",
    })) {
        return false;
    }
    fputs("# Textbook Notes\n\n", proc.in);
    while (scan_md(&state) && state.section == 0) {
        copy_md(&state, proc.in);
    }
    struct TocState toc = render_toc_start(proc.in);
    const struct MarkdownHeading highlights =
        {.label = NULL_SPAN, .title = SPAN("Highlights")};
    render_toc_item(&toc, 1, highlights, "quote.html");
    do {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_toc_item(&toc, 1, h, "front.html#%.*s", id.len, id.data);
        }
    } while (scan_md(&state) && MS_INDEX(state.section, 1) <= 1);
    do {
        if (state.heading == 1) {
            render_toc_item(&toc, 1, parse_md_heading(state.line),
                "%d/index.html", MS_INDEX(state.section, 1) - 1);
        } else if (state.heading == 2) {
            render_toc_item(&toc, 2, parse_md_heading(state.line),
                "%d/%d.html",
                MS_INDEX(state.section, 1) - 1,
                MS_INDEX(state.section, 2));
        }
    } while (scan_md(&state));
    render_toc_end(&toc);
    return wait_pandoc(&proc);
}

// Generates docs/text/quote.html.
static bool gen_text_quote(void) {
    struct MarkdownState state;
    if (!init_md(&state, MARKDOWN("quote"))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = HTML("text/quote"),
        .title = "SICP Highlights",
        .active = "text",
        .root = "../",
        .prev = "index.html",
        .up = "index.html",
        .next = "front.html",
    })) {
        return false;
    }
    fprintf(proc.in, "# Highlights\n\n");
    while (scan_md(&state) && state.section != 1);
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            if (h.label.data) {
                render_quote_heading(proc.in, 2, h.label, h,
                    "%.*s/index.html", h.label.len, h.label.data);
            } else {
                char buf[SZ_HEADING];
                struct Span id = tolower_s(h.title, buf, sizeof buf);
                render_quote_heading(proc.in, 2, id, h,
                    "front.html#%.*s", id.len, id.data);
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
    if (!init_md(&state, MARKDOWN("text"))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = HTML("text/front"),
        .title = "SICP Frontmatter Notes",
        .active = "text",
        .root = "../",
        .prev = "quote.html",
        .up = "index.html",
        .next = "1/index.html",
    })) {
        return false;
    }
    while (scan_md(&state) && state.section != 1);
    do {
        if (state.heading > 1) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_heading(proc.in, state.heading, id, h);
        } else {
            copy_md(&state, proc.in);
        }
    } while (scan_md(&state) && state.heading != 1);
    return wait_pandoc(&proc);
}

// Generates docs/text/*/index.html.
static bool gen_text_chapter(const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash > output && strcmp(slash, "/index.html") == 0)) {
        goto invalid;
    }
    const int chapter = slash[-1] - '0';
    if (!(chapter >= 1 && chapter <= NUM_CHAPTERS)) {
        goto invalid;
    }
    struct MarkdownState state;
    if (!init_md(&state, MARKDOWN("text"))) {
        return false;
    }
    char title[] = "SICP Chapter _ Notes";
    title[13] = '0' + chapter;
    const char *prev;
    char prev_buf[] = "../_/_.html";
    if (chapter == 1) {
        prev = "../front.html";
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
        .active = "text",
        .root = "../../",
        .prev = prev,
        .up = "../index.html",
        .next = "1.html",
    })) {
        return false;
    }
    const MarkdownSection target_section = text_section(chapter, 0);
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h);
    while (scan_md(&state) && state.heading == 0) {
        copy_md(&state, proc.in);
    }
    struct TocState toc = render_toc_start(proc.in);
    do {
        if (state.heading == 2) {
            int section = MS_INDEX(state.section, 2);
            render_toc_item(&toc, 1, parse_md_heading(state.line),
                "%d.html", section);
        } else if (state.heading == 3) {
            int section = MS_INDEX(state.section, 2);
            int subsection = MS_INDEX(state.section, 3);
            render_toc_item(&toc, 2, parse_md_heading(state.line),
                "%d.html#%d.%d.%d", section, chapter, section, subsection);
        }
    } while (scan_md(&state) && state.heading != 1);
    render_toc_end(&toc);
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
    if (!init_md(&state, MARKDOWN("text"))) {
        return false;
    }
    char title[] = "SICP Section _._ Notes";
    title[13] = '0' + chapter;
    title[15] = '0' + section;
    const char *prev;
    char prev_buf[] = "_.html";
    if (section == 1) {
        prev = "index.html";
    } else {
        prev_buf[0] = '0' + section - 1;
        prev = prev_buf;
    }
    char *next;
    char next_buf[] = "../_/index.html";
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
        .active = "text",
        .root = "../../",
        .prev = prev,
        .up = "index.html",
        .next = next,
    })) {
        return false;
    }
    const MarkdownSection target_section = text_section(chapter, section);
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h);
    while (scan_md(&state) && state.heading != 1 && state.heading != 2) {
        if (state.heading >= 3) {
            h = parse_md_heading(state.line);
            if (h.label.data) {
                assert(state.heading == 3);
                render_heading(proc.in, 2, h.label, h);
            } else {
                assert(state.heading == 4);
                char id[SZ_LABEL];
                snprintf(id, sizeof id, "%d.%d.%d.%d", chapter, section,
                    MS_INDEX(state.section, 3), MS_INDEX(state.section, 4));
                render_heading(proc.in, 3, SPAN(id), h);
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
    if (!init_md(&state, MARKDOWN("lecture"))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
        .input = "/dev/stdin",
        .output = HTML("lecture/index"),
        .title = "SICP Lecture Notes",
        .active = "lecture",
        .root = "../",
        .prev = NULL,
        .up = "../index.html",
        .next = "quote.html",
    })) {
        return false;
    }
    fprintf(proc.in, "# Lecture Notes\n\n");
    while (scan_md(&state) && state.section == 0) {
        copy_md(&state, proc.in);
    }
    struct TocState toc = render_toc_start(proc.in);
    struct MarkdownHeading h =
        {.label = NULL_SPAN, .title = SPAN("Highlights")};
    render_toc_item(&toc, 1, h, "quote.html");
    do {
        if (state.heading == 1) {
            h = parse_md_heading(state.line);
            char buf[SZ_LABEL];
            struct Span href = tolower_s(h.label, buf, sizeof buf);
            render_toc_item(&toc, 1, h, "%.*s.html", href.len, href.data);
        }
    } while (scan_md(&state));
    render_toc_end(&toc);
    return wait_pandoc(&proc);
}

// Generates docs/lecture/quote.html.
static bool gen_lecture_quote(void) {
    struct MarkdownState state;
    if (!init_md(&state, MARKDOWN("quote"))) {
        return false;
    }
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = HTML("lecture/quote"),
        .title = "SICP Lecture Highlights",
        .active = "lecture",
        .root = "../",
        .prev = "index.html",
        .up = "index.html",
        .next = "1a.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    fprintf(proc.in, "# Highlights\n\n");
    while (scan_md(&state) && state.section != 2);
    while (scan_md(&state)) {
        if (state.heading == 2) {
            struct MarkdownHeading h = parse_md_heading(state.line);
            assert(h.label.data);
            char buf[SZ_LABEL];
            struct Span id = tolower_s(h.label, buf, sizeof buf);
            render_quote_heading(proc.in, 2, id, h,
                "%.*s.html", id.len, id.data);
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
    if (!init_md(&state, MARKDOWN("lecture"))) {
        return false;
    }
    char title[SZ_HEADING];
    snprintf(title, sizeof title, "SICP Lecture %d%c Notes",
        number, toupper(a_or_b));
    const char *prev;
    char prev_buf[SZ_HREF];
    if (number == 1 && a_or_b == 'a') {
        prev = "quote.html";
    } else {
        int ab = a_or_b - 'a';
        snprintf(prev_buf, sizeof prev_buf, "%d%c.html",
            number + ab - 1, 'b' - ab);
        prev = prev_buf;
    }
    const char *next;
    char next_buf[SZ_HREF];
    if (number == 10 && a_or_b == 'b') {
        next = NULL;
    } else {
        int ab = a_or_b - 'a';
        snprintf(next_buf, sizeof next_buf, "%d%c.html",
            number + ab, 'b' - ab);
        next = next_buf;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts ){
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = "lecture",
        .root = "../",
        .prev = prev,
        .up = "index.html",
        .next = next,
    })) {
        return false;
    }
    const MarkdownSection target_section = 1 + (number - 1) * 2 + a_or_b - 'a';
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h);
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading > 1) {
            h = parse_md_heading(state.line);
            assert(!h.label.data);
            char id[SZ_LABEL];
            write_dotted_section(id, sizeof id, state.section >> MS_BITS);
            render_heading(proc.in, state.heading, SPAN(id), h);
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
    if (strcmp(output, HTML("index")) == 0) {
        return gen_index();
    }
    if (startswith(output, SUBDIR("text"))) {
        if (strcmp(output, HTML("text/index")) == 0) {
            return gen_text_index();
        }
        if (strcmp(output, HTML("text/quote")) == 0) {
            return gen_text_quote();
        }
        if (strcmp(output, HTML("text/front")) == 0) {
            return gen_text_front();
        }
        if (endswith(output, "/index.html")) {
            return gen_text_chapter(output);
        }
        return gen_text_section(output);
    }
    if (startswith(output, SUBDIR("lecture"))) {
        if (strcmp(output, HTML("lecture/index")) == 0) {
            return gen_lecture_index();
        }
        if (strcmp(output, HTML("lecture/quote")) == 0) {
            return gen_lecture_quote();
        }
        return gen_lecture_page(output);
    }
    if (startswith(output, SUBDIR("exercise"))) {
        if (strcmp(output, HTML("exercise/index")) == 0) {
            return gen_exercise_index();
        }
        if (endswith(output, "/index.html")) {
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
