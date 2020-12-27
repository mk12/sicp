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

// Returns the length of an array.
#define ARRAYLEN(a) (sizeof a / sizeof a[0])

// Returns true if s starts with the given prefix.
static bool startswith(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

// Returns true if s ends with the given suffix.
static bool endswith(const char *s, const char *suffix) {
    const int n = strlen(s);
    const int m = strlen(suffix);
    return n >= m && strncmp(s + (n - m), suffix, m) == 0;
}

// Concatenates two strings into a newly allocated string.
static char *concat(const char *s1, const char *s2) {
    const int n1 = strlen(s1);
    const int n2 = strlen(s2);
    char *dest = malloc(n1 + n2 + 1);
    memcpy(dest, s1, n1);
    memcpy(dest + n1, s2, n2);
    dest[n1 + n2] = '\0';
    return dest;
}

// Converts s to lowercase in a newly allocated string.
static char *tolower_s(const char *s, int len) {
    char *d = strndup(s, len);
    for (int i = 0; i < len; i++) {
        d[i] = tolower(d[i]);
    }
    return d;
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
    // File descriptor for writing to the child's stdin.
    int in;
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
    proc->in = fd[1];
    return true;
}

// Closes proc's pipe and waits for it to finish. Returns true on success.
static bool wait_pandoc(struct PandocProc *proc) {
    close(proc->in);
    if (waitpid(proc->pid, NULL, 0) == -1) {
        perror("waitpid");
        return false;
    };
    proc->pid = -1;
    proc->in = -1;
    return true;
}

// A Markdown section is represented by an integer s, where:
//
//     (s >> 0*MS_BITS) & MS_MASK  gives the h1 level,
//     (s >> 1*MS_BITS) & MS_MASK  gives the h2 level,
//     ...
//
// A level of 0 means that heading has not been encountered yet.
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
#define MS_LEVEL(s, h) ((int)(((s) >> (h - 1)*MS_BITS) & MS_MASK))
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
    // Current line, capacity, and length (excluding null terminator).
    char *line;
    size_t cap;
    ssize_t len;
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
    state->line = NULL;
    state->len = 1;
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
    const bool prev_blank = state->len == 1;
    state->len = getline(&state->line, &state->cap, state->file);
    state->heading = 0;
    if (state->len == -1) {
        fclose(state->file);
        state->file = NULL;
        return false;
    }
    if (startswith(state->line, "```")) {
        state->code = !state->code;
        return true;
    }
    if (!state->code && prev_blank) {
        int h = 0;
        while (h < state->len && state->line[h] == '#') h++;
        if (h > 0 && h < state->len && state->line[h] == ' ') {
            state->section = MS_NEXT(state->section, h);
            state->heading = h;
        }
    }
    return true;
}

// A parsed Markdown heading.
struct MarkdownHeading {
    // If the heading looks like, "# 1A: Foo bar", this is "1A". Otherwise NULL.
    const char *label;
    int label_len;
    // If the heading looks like "# 1A: Foo bar", this is "Foo bar". If there is
    // no label, it is the whole heading with only the leading "# " removed.
    const char *title;
    int title_len;
};

// Parses a Markdown heading.
static struct MarkdownHeading parse_md_heading(const char *line, int len) {
    assert(len >= 2 && line[0] == '#'  && line[len-1] == '\n');
    int i = 1;
    while (i < len && line[i] == '#') i++;
    assert(line[i++] == ' ' && i < len);
    if (line[i] >= '1' && line[i] <= '9') {
        int j = i;
        while (j < len && line[j] != ':') j++;
        if (j + 2 < len && line[j+1] == ' ') {
            return (struct MarkdownHeading){
                .label = line + i,
                .label_len = j - i,
                .title = line + j + 2,
                .title_len = len - j - 3,
            };
        }
    }
    return (struct MarkdownHeading){
        .label = NULL,
        .label_len = 0,
        .title = line + i,
        .title_len = len - i - 1,
    };
}

// Helpers to construct input/output paths.
#define MARKDOWN(f) ("notes/" f ".md")
#define HTML(f) ("docs/" f ".html")
#define SUBDIR(d) ("docs/" d "/")

// Number of chapters in the textbook.
#define NUM_CHAPTERS 5

// Returns the number of sections in the given chapter.
static int num_sections(int chapter) {
    return (int[]){3, 5, 5, 4, 5}[chapter-1];
}

// Returns the Markdown section for the given textbook chapter/section.
static MarkdownSection text_md_section(int chapter, int section) {
    // Add 1 since "# Frontmatter" is the first h1.
    return (MarkdownSection)chapter + 1 | ((MarkdownSection)section << MS_BITS);
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
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = HTML("text/index"),
        .title = "SICP Notes",
        .active = "text",
        .root = "../",
        .prev = NULL,
        .up = "../index.html",
        .next = "quote.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    dprintf(proc.in, "# Textbook Notes\n\n");
    while (scan_md(&state) && state.section == 0) {
        write(proc.in, state.line, state.len);
    }
    dprintf(proc.in,
        "\n## Contents\n\n"
        "<ul class=\"toc\">"
        "<li class=\"toc__item\">"
        "<span class=\"toc__label\"></span>"
        "<a href=\"quote.html\">Highlights</a>"
        "</li>");
    const char *close = "";
    struct MarkdownHeading h;
    do {
        if (state.heading == 1 && state.section != 1) {
            h = parse_md_heading(state.line, state.len);
            dprintf(proc.in,
                "%s<li class=\"toc__item\">"
                "<span class=\"toc__label\">%.*s</span>"
                "<a href=\"%d/index.html\">%.*s</a>"
                "<ul class=\"toc\">",
                close, h.label_len, h.label, MS_LEVEL(state.section, 1) - 1,
                h.title_len, h.title);
            close = "</ul></li>";
        } else if (state.heading == 2 && MS_LEVEL(state.section, 1) == 1) {
            h = parse_md_heading(state.line, state.len);
            assert(!h.label);
            char *id = tolower_s(h.title, h.title_len);
            dprintf(proc.in,
                "<li class=\"toc__item\">"
                "<span class=\"toc__label\"></span>"
                "<a href=\"front.html#%s\">%.*s</a>"
                "</li>",
                id, h.title_len, h.title);
            free(id);
        } else if (state.heading == 2) {
            h = parse_md_heading(state.line, state.len);
            dprintf(proc.in,
                "<li class=\"toc__item\">"
                "<span class=\"toc__label\">%.*s</span>"
                "<a href=\"%d/%d.html\">%.*s</a>"
                "</li>",
                h.label_len, h.label, MS_LEVEL(state.section, 1) - 1,
                MS_LEVEL(state.section, 2), h.title_len, h.title);
        }
    } while (scan_md(&state));
    dprintf(proc.in, "%s</ul>\n", close);
    return wait_pandoc(&proc);
}

// Generates docs/text/quote.html.
static bool gen_text_quote(void) {
    struct MarkdownState state;
    if (!init_md(&state, MARKDOWN("quote"))) {
        return false;
    }
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = HTML("text/quote"),
        .title = "SICP Highlights",
        .active = "text",
        .root = "../",
        .prev = "index.html",
        .up = "index.html",
        .next = "front.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    dprintf(proc.in, "# Highlights\n\n");
    while (scan_md(&state) && state.section != 1);
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading > 1) {
            struct MarkdownHeading h = parse_md_heading(state.line, state.len);
            if (h.label) {
                dprintf(proc.in,
                    "<h%d id=\"%.*s\" class=\"anchor\">"
                    "<a class=\"anchor__link link\" href=\"#%.*s\">#</a>"
                    "<small class=\"number\">%.*s</small>"
                    "<a class=\"link\" href=\"%.*s/index.html\">%.*s</a>"
                    "</h%d>\n",
                    state.heading, h.label_len, h.label, h.label_len, h.label,
                    h.label_len, h.label, h.label_len, h.label,
                    h.title_len, h.title, state.heading);
            } else {
                char *id = tolower_s(h.title, h.title_len);
                dprintf(proc.in,
                    "<h%d id=\"%s\" class=\"anchor\">"
                    "<a class=\"anchor__link link\" href=\"#%s\">#</a>"
                    "<a class=\"link\" href=\"front.html#%s\">%.*s</a>"
                    "</h%d>\n",
                    state.heading, id, id, id, h.title_len, h.title,
                    state.heading);
                free(id);
            }
        } else {
            write(proc.in, state.line, state.len);
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
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = HTML("text/front"),
        .title = "SICP Frontmatter Notes",
        .active = "text",
        .root = "../",
        .prev = "quote.html",
        .up = "index.html",
        .next = "1/index.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    while (scan_md(&state) && state.section != 1);
    do {
        if (state.heading > 1) {
            struct MarkdownHeading h = parse_md_heading(state.line, state.len);
            assert(!h.label);
            char *id = tolower_s(h.title, h.title_len);
            dprintf(proc.in,
                "<h%d id=\"%s\" class=\"anchor\">"
                "<a class=\"anchor__link link\" href=\"#%s\">#</a>"
                "%.*s"
                "</h%d>\n",
                state.heading, id, id, h.title_len, h.title, state.heading);
            free(id);
        } else {
            write(proc.in, state.line, state.len);
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
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = "text",
        .root = "../../",
        .prev = prev,
        .up = "../index.html",
        .next = "1.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    const MarkdownSection target_section = text_md_section(chapter, 0);
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line, state.len);
    assert(h.label);
    dprintf(proc.in, "<h1><small class=\"number\">%.*s</small>%.*s</h1>\n",
        h.label_len, h.label, h.title_len, h.title);
    while (scan_md(&state) && state.heading == 0) {
        write(proc.in, state.line, state.len);
    }
    dprintf(proc.in,
        "\n## Contents\n\n"
        "<ul class=\"toc\">");
    const char *close = "";
    do {
        if (state.heading == 2) {
            h = parse_md_heading(state.line, state.len);
            const int section = MS_LEVEL(state.section, 2);
            dprintf(proc.in,
                "%s<li class=\"toc__item\">"
                "<span class=\"toc__label\">%.*s</span>"
                "<a href=\"%d.html\">%.*s</a>"
                "<ul class=\"toc\">",
                close, h.label_len, h.label, section, h.title_len, h.title);
            close = "</ul></li>";
        } else if (state.heading == 3) {
            h = parse_md_heading(state.line, state.len);
            const int section = MS_LEVEL(state.section, 2);
            const int subsection = MS_LEVEL(state.section, 3);
            dprintf(proc.in,
                "<li class=\"toc__item\">"
                "<span class=\"toc__label\">%.*s</span>"
                "<a href=\"%d.html#%d.%d.%d\">%.*s</a>"
                "</li>",
                h.label_len, h.label, section, chapter, section, subsection,
                h.title_len, h.title);
        }
    } while (scan_md(&state) && state.heading != 1);
    dprintf(proc.in, "%s</ul>\n", close);
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
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = "text",
        .root = "../../",
        .prev = prev,
        .up = "index.html",
        .next = next,
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    const MarkdownSection target_section = text_md_section(chapter, section);
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line, state.len);
    assert(h.label);
    dprintf(proc.in, "<h1><small class=\"number\">%.*s</small>%.*s</h1>\n",
        h.label_len, h.label, h.title_len, h.title);
    while (scan_md(&state) && state.heading != 1 && state.heading != 2) {
        if (state.heading >= 3) {
            h = parse_md_heading(state.line, state.len);
            if (h.label) {
                assert(state.heading == 3);
                dprintf(proc.in,
                    "<h%d id=\"%.*s\" class=\"anchor\">"
                    "<a class=\"anchor__link link\" href=\"#%.*s\">#</a>"
                    "<small class=\"number\">%.*s</small>"
                    "%.*s"
                    "</h%d>\n",
                    state.heading - 1, h.label_len, h.label,
                    h.label_len, h.label, h.label_len, h.label,
                    h.title_len, h.title, state.heading);
            } else {
                assert(state.heading == 4);
                char id[] = "_._._._";
                snprintf(id, sizeof id, "%d.%d.%d.%d", chapter, section,
                    MS_LEVEL(state.section, 3), MS_LEVEL(state.section, 4));
                dprintf(proc.in,
                    "<h%d id=\"%s\" class=\"anchor\">"
                    "<a class=\"anchor__link link\" href=\"#%s\">#</a>"
                    "%.*s"
                    "</h%d>\n",
                    state.heading - 1, id, id, h.title_len, h.title,
                    state.heading - 1);
            }
        } else {
            write(proc.in, state.line, state.len);
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
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = HTML("lecture/index"),
        .title = "SICP Lecture Notes",
        .active = "lecture",
        .root = "../",
        .prev = NULL,
        .up = "../index.html",
        .next = "quote.html",
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    dprintf(proc.in, "# Lecture Notes\n\n");
    while (scan_md(&state) && state.section == 0) {
        write(proc.in, state.line, state.len);
    }
    dprintf(proc.in,
        "\n## Contents\n\n"
        "<ul class=\"toc\">"
        "<li class=\"toc__item\">"
        "<span class=\"toc__label\"></span>"
        "<a href=\"quote.html\">Highlights</a>"
        "</li>");
    do {
        if (state.heading == 1) {
            struct MarkdownHeading h = parse_md_heading(state.line, state.len);
            char *href = tolower_s(h.label, h.label_len);
            dprintf(proc.in,
                "<li class=\"toc__item\">"
                "<span class=\"toc__label\">%.*s</span>"
                "<a href=\"%s.html\">%.*s</a>"
                "</li>",
                h.label_len, h.label, href, h.title_len, h.title);
            free(href);
        }
    } while (scan_md(&state));
    dprintf(proc.in, "</ul>\n");
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
    dprintf(proc.in, "# Highlights\n\n");
    while (scan_md(&state) && state.section != 2);
    while (scan_md(&state)) {
        if (state.heading > 1) {
            struct MarkdownHeading h = parse_md_heading(state.line, state.len);
            assert(h.label);
            char *id = tolower_s(h.label, h.label_len);
            dprintf(proc.in,
                "<h%d id=\"%s\" class=\"anchor\">"
                "<a class=\"anchor__link link\" href=\"#%s\">#</a>"
                "<small class=\"number\">%.*s</small>"
                "<a class=\"link\" href=\"%s.html\">%.*s</a>"
                "</h%d>\n",
                state.heading, id, id, h.label_len, h.label, id,
                h.title_len, h.title, state.heading);
            free(id);
        } else {
            write(proc.in, state.line, state.len);
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
    char title[] = "SICP Lecture ___ Notes";
    snprintf(title, sizeof title, "SICP Lecture %d%c Notes",
        number, toupper(a_or_b));
    const char *prev;
    char prev_buf[] = "___.html";
    if (number == 1 && a_or_b == 'a') {
        prev = "quote.html";
    } else {
        int ab = a_or_b - 'a';
        snprintf(prev_buf, sizeof prev_buf, "%d%c.html",
            number + ab - 1, 'b' - ab);
        prev = prev_buf;
    }
    const char *next;
    char next_buf[] = "___.html";
    if (number == 10 && a_or_b == 'b') {
        next = NULL;
    } else {
        int ab = a_or_b - 'a';
        snprintf(next_buf, sizeof next_buf, "%d%c.html",
            number + ab, 'b' - ab);
        next = next_buf;
    }
    const struct PandocOpts opts = {
        .input = "/dev/stdin",
        .output = output,
        .title = title,
        .active = "lecture",
        .root = "../",
        .prev = prev,
        .up = "index.html",
        .next = next,
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    const MarkdownSection target_section = 1 + (number - 1) * 2 + a_or_b - 'a';
    while (scan_md(&state) && state.section != target_section);
    assert(state.section == target_section);
    struct MarkdownHeading h = parse_md_heading(state.line, state.len);
    assert(h.label);
    dprintf(proc.in, "<h1><small class=\"number\">%.*s</small>%.*s</h1>\n",
        h.label_len, h.label, h.title_len, h.title);
    char id[10];
    while (scan_md(&state) && state.heading != 1) {
        if (state.heading > 1) {
            h = parse_md_heading(state.line, state.len);
            assert(!h.label);
            write_dotted_section(id, sizeof id, state.section >> MS_BITS);
            dprintf(proc.in,
                "<h%d id=\"%s\" class=\"anchor\">"
                "<a class=\"anchor__link link\" href=\"#%s\">#</a>"
                "%.*s"
                "</h%d>\n",
                state.heading, id, id, h.title_len, h.title, state.heading);
        } else {
            write(proc.in, state.line, state.len);
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
