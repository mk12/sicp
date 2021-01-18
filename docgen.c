// Copyright 2021 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <signal.h>
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
    // Destination path. Usually output is "/dev/stdout" so that docgen can do
    // further post-processing, while dest is the actual HTML path. It is only
    // used for construct the -M id=... parameter; the file is not opened.
    const char *dest;
    // Contents of <title>...</title>.
    const char *title;
    // Links to up/prev/next page. If any are set, up must be set.
    const char *up;
    const char *prev;
    const char *next;
};

// Invokes pandoc, printing the command to stderr before executing it. Normally
// does not return since it replaces the current process. If exec fails, returns
// false (most likely because Pandoc is not installed or not in $PATH).
static bool pandoc(const struct PandocOpts opts) {
    const int LEN = 1   // pandoc
                  + 3   // -o output -dconfig
                  + 4   // -M id -M title
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
    const char *id_arg = concat("id=", strchr(opts.dest, '/') + 1);
    *strrchr(id_arg, '.') = '\0';
    argv[i++] = id_arg;
    argv[i++] = "-M";
    const int title_idx = i;
    argv[i++] = concat("title=", opts.title);
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
        fprintf(stderr, "%s%s%s%s", quote, argv[j], quote, space);
    }
    putc('\n', stderr);
    // It is safe to cast to (char **) because execvp will not modify argv
    // except as a consequence of replacing the process image.
    execvp(PANDOC, (char **)argv);
    perror(PANDOC);
    return false;
}

// PID of the Pandoc process, stored so that signal_handler can kill it.
static volatile pid_t global_pandoc_pid = 0;
// Flag indicating that global_pandoc_pid is set. We need this because pid_t is
// not guaranteed to be written in a single instruction.
static volatile sig_atomic_t global_pandoc_pid_set = 0;

// Kills global_pandoc_pid (if set).
static void kill_pandoc(void) {
    if (global_pandoc_pid_set) {
        kill(global_pandoc_pid, SIGTERM);
    }
}

// Kills global_pandoc_pid (if set) and then runs the default signal handler.
static void signal_handler(int signum) {
    kill_pandoc();
    signal(signum, SIG_DFL);
    raise(signum);
}

// Termination signals that we want to handle.
static const int SIGNUMS[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM};
static const int N_SIGNUMS = sizeof SIGNUMS / sizeof SIGNUMS[0];

// Ensures that global_pandoc_pid (if set) is killed when this process
// terminates normally (returning from main or calling exit) or receives one of
// the signals in SIGNUMS. It is still possible for the pandoc process to be
// orphaned, for example if this process receives SIGKILL.
static bool kill_pandoc_on_termination(void) {
    if (atexit(kill_pandoc) == -1) {
        perror("atexit");
        return false;
    }
    struct sigaction action;
    action.sa_handler = signal_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    for (int i = 0; i < N_SIGNUMS; i++) {
        struct sigaction old;
        if (sigaction(SIGNUMS[i], NULL, &old) == -1) {
            perror("sigaction");
            return false;
        }
        if (old.sa_handler == SIG_IGN) continue;
        if (sigaction(SIGNUMS[i], &action, NULL) == -1) {
            perror("sigaction");
            return false;
        }
    }
    return true;
}

// Represents a child process executing pandoc.
struct PandocProc {
    // PID of the child process.
    pid_t pid;
    // Stream for writing to the child's stdin.
    FILE *in;
    // Stream for reading from the child's stdout.
    FILE *out;
};

// Helper that closes both ends of a pipe.
#define CLOSE2(p)    \
    do {             \
        close(p[0]); \
        close(p[1]); \
    } while (0)

// Runs pandoc with opts in a child process, storing its information in proc.
// Also registers handlers to kill the child process if the parent terminates
// (this is useful if the Lua filter has an infinite loop bug, for example).
// Returns true on success. To take advantage of proc->in and proc->out, set
// opts.input to "/dev/stdin" and opts.output to "/dev/stdout", respectively.
// The caller should invoke wait_pandoc or finish_pandoc later.
static bool fork_pandoc(struct PandocProc *proc, struct PandocOpts opts) {
    if (!kill_pandoc_on_termination()) {
        return false;
    }
    enum { READ, WRITE, RW_N };
    int in[RW_N], out[RW_N];
    if (pipe(in) == -1) {
        perror("pipe");
        return false;
    }
    if (pipe(out) == -1) {
        perror("pipe");
        CLOSE2(in);
        return false;
    }
    if ((proc->pid = fork()) == -1) {
        perror("fork");
        CLOSE2(in);
        CLOSE2(out);
        return false;
    }
    if (proc->pid == 0) {
        // Move the input read-side to file descriptor 0 (stdin).
        close(in[WRITE]);
        dup2(in[READ], 0);
        close(in[READ]);
        // Move the output write-side to file descriptor 1 (stdout).
        close(out[READ]);
        dup2(out[WRITE], 1);
        close(out[WRITE]);
        // Replace this process with pandoc.
        return pandoc(opts);
    }
    global_pandoc_pid = proc->pid;
    global_pandoc_pid_set = 1;
    close(in[READ]);
    close(out[WRITE]);
    proc->in = fdopen(in[WRITE], "w");
    if (!proc->in) {
        perror("fdopen");
        close(in[WRITE]);
        close(out[READ]);
        return false;
    }
    proc->out = fdopen(out[READ], "r");
    if (!proc->out) {
        perror("fdopen");
        fclose(proc->in);
        close(out[READ]);
        return false;
    }
    return true;
}

// Closes streams and blocks until proc finishes. Then cleans up globals set by
// fork_pandoc for signal handlers, and resets all of proc's fields to their
// default values. Returns true on success.
static bool wait_pandoc(struct PandocProc *proc) {
    if (proc->in) {
        fclose(proc->in);
        proc->in = NULL;
    }
    if (proc->out) {
        fclose(proc->out);
        proc->out = NULL;
    }
    bool success;
    assert(proc->pid != 0);
    int proc_status;
    if (waitpid(proc->pid, &proc_status, 0) == -1) {
        perror("waitpid");
        success = false;
    } else {
        success = WIFEXITED(proc_status) && WEXITSTATUS(proc_status) == 0;
    }
    global_pandoc_pid_set = 0;
    global_pandoc_pid = 0;
    proc->pid = 0;
    proc->in = NULL;
    proc->out = NULL;
    return success;
}

// Post-processes HTML, removing unwanted tags and classes in inline code and
// code blocks (there is no option to prevent Pandoc from producing these). Also
// deletes "«" and "»", and converts "‹" and "›" to "<" and ">". The Markdown
// source uses the former for metavariables. The Lua filter uses the latter to
// inject HTML links in code blocks.
static void postprocess_html(FILE *in, FILE *out) {
    char *line = NULL;
    size_t cap = 0;
    ssize_t len;
    while ((len = getline(&line, &cap, in)) > 0) {
        const char *p = line;
        const char *q = line + len - 1;
        const char *suffix = "\n";
        assert(*q == '\n');
        if (startswith(p, "<div class=\"sourceCode\" id=\"cb")) {
            fputs("<pre><code class=\"blockcode\">", out);
            for (int i = 0; i < 3; i++) p = strchr(p + 1, '<');
        }
        if (startswith(p, "<span id=\"cb")) {
            for (int i = 0; i < 3; i++) p = strchr(p + 1, '>');
            p++;
            if (endswith(p, "</code></pre></div>\n")) {
                q -= strlen("</span></code></pre></div>");
                suffix = "</code></pre>\n";
            } else {
                q -= strlen("</span>");
            }
        }
        const char *n;
        if (p == line) {
            // We're not in a code block. Deal with inline code.
            const char *needle = "<code class=\"sourceCode scheme\">";
            while ((n = strstr(p, needle))) {
                fwrite(p, n - p, 1, out);
                fputs("<code>", out);
                p = n + strlen(needle);
                // Deal with metavariables.
                while ((n = strstr(p, "<span class=\"sc\">"))) {
                    fwrite(p, n - p, 1, out);
                    p = n + strlen("<span class=\"sc\">«</span>");
                }
            }
        } else {
            // We're in a code block. Deal with metavariables and pastes. While
            // these two steps really should be interleaved, it doesn't matter
            // because we never use them together.
            while ((n = strstr(p, "<span class=\"sc\">"))) {
                fwrite(p, n - p, 1, out);
                p = n + strlen("<span class=\"sc\">«</span>");
            }
            const int open_len = strlen("‹");
            const int close_len = strlen("›");
            while ((n = strstr(p, "‹"))) {
                fwrite(p, n - p, 1, out);
                putc('<', out);
                p = n + open_len;
                n = strstr(p, "›");
                assert(n);
                fwrite(p, n - p, 1, out);
                putc('>', out);
                p = n + close_len;
            }
        }
        assert(p <= q);
        fwrite(p, q - p, 1, out);
        fputs(suffix, out);
    }
}

// Post-processes the HTML from proc.out and writes it to the given file. Then
// calls wait_pandoc. Returns true on success.
static bool finish_pandoc(struct PandocProc *proc, const char *output) {
    fclose(proc->in);
    proc->in = NULL;
    FILE *out = fopen(output, "w");
    if (!out) {
        perror("fopen");
        return false;
    }
    postprocess_html(proc->out, out);
    fclose(out);
    return wait_pandoc(proc);
}

// A parsed heading from Markdown or Scheme.
struct Heading {
    // Markdown: The "1A" in "# 1A: Foo bar", or NULL_SPAN if there is none.
    // Scheme: The "1.2" in (Section :1.2 "Foo") or (Exercise ?1.2).
    struct Span label;
    // Markdown: The "Foo bar" in "# 1A: Foo bar" or "# Foo bar".
    // Scheme: The "Foo" in (Section :1.2 "Foo"). NULL_SPAN for exercises.
    struct Span title;
};

// Creates a heading with title only, from a string literal.
#define TITLE_HEADING(t) \
    ((struct Heading){.label = NULL_SPAN, .title = SPAN(t)})

// Parses a heading from a line of Markdown.
static struct Heading parse_md_heading(struct Span s) {
    char *const p = s.data;
    const int n = s.len;
    assert(n >= 2 && p[0] == '#' && p[n - 1] == '\n');
    int i = 1;
    while (i < n && p[i] == '#') i++;
    assert(p[i++] == ' ' && i < n);
    if (p[i] >= '1' && p[i] <= '9') {
        int j = i;
        while (j < n && p[j] != ':') j++;
        if (j + 2 < n && p[j + 1] == ' ') {
            return (struct Heading){
                .label = {p + i, j - i},
                .title = {p + j + 2, n - j - 3},
            };
        }
    }
    return (struct Heading){
        .label = NULL_SPAN,
        .title = {p + i, n - i - 1},
    };
}

// Parses a heading from a line of Scheme.
static struct Heading parse_ss_heading(struct Span s) {
    char *const p = s.data;
    const int n = s.len;
    assert(p[0] == '(');
    for (int i = 0; i < n; i++) {
        if (p[i] == '?') {
            int j = ++i;
            while (j < n && p[j] != ')' && p[j] != '\n') j++;
            return (struct Heading){
                .label = {p + i, j - i},
                .title = NULL_SPAN,
            };
        } else if (p[i] == ':') {
            int j = ++i;
            while (j < n && p[j] != ' ') j++;
            int k = j;
            while (k < n && p[k] != '"') k++;
            int l = ++k;
            while (l < n && p[l] != '"') l++;
            return (struct Heading){
                .label = {p + i, j - i},
                .title = {p + k, l - k},
            };
        }
    }
    assert(false);
}

// A document sector is an integer s where
//
//     (s >> 0*DS_BITS) & DS_MASK  gives the h1 index (chapter),
//     (s >> 1*DS_BITS) & DS_MASK  gives the h2 index (section),
//     (s >> 2*DS_BITS) & DS_MASK  gives the h3 index (subsection),
//     (s >> 3*DS_BITS) & DS_MASK  gives the h4 index (subsubsection),
//     (s >> 4*DS_BITS) & DS_MASK  gives the exercise index.
//
// An index of 0 means that heading has not been encountered yet. Exercises are
// always nested directly under chapters (they are not really an h5 index).
//
// Example (Markdown):
//
//     Some text.          s=0x0000
//                         s=0x0000
//     # First             s=0x0001
//                         s=0x0001
//     ## A                s=0x0101
//     ## B                s=0x0201
//     # Second            s=0x0002
//
// Example (Scheme):
//
//     ; ...               s=0x0000000000
//     (Chapter :1)        s=0x0000000001
//     ; ...               s=0x0000000001
//     (Section :1.2)      s=0x0000000201
//     (Section :1.2.3)    s=0x0000030201
//     (Section :1.2.3.4)  s=0x0004030201
//     (Exercise ?3.5)     s=0x0500000003
//
typedef uint64_t Sector;
#define DS_MASK UINT64_C(0xff)
#define DS_BITS UINT64_C(8)
#define DS_INDEX(s, h) ((int)(((s) >> (h - 1) * DS_BITS) & DS_MASK))
#define DS_MASK_UPTO(h) ((UINT64_C(1) << h * DS_BITS) - 1)
#define DS_INCREMENT(h) (UINT64_C(1) << (h - 1) * DS_BITS)
#define DS_NEXT(s, h) ((s & DS_MASK_UPTO(h)) + DS_INCREMENT(h))
#define DS_VALUE(h, i) ((Sector)(i) << (Sector)(h - 1) * DS_BITS)
#define DS_EXERCISE_LEVEL 5

// Writes a dotted string representation of sector in buf. Writes no more than
// size characters (including the null terminator). For example, the sector
// 0x030201 becomes "1.2.3".
static void write_dotted_section(char *buf, int size, Sector sector) {
    assert(size >= 2);
    assert(sector != 0);
    int i = 0;
    i += snprintf(buf + i, size - i, "%d", (int)(sector & DS_MASK));
    sector >>= DS_BITS;
    while (sector && i < size) {
        i += snprintf(buf + i, size - i, ".%d", (int)(sector & DS_MASK));
        sector >>= DS_BITS;
    }
}

// Markdown line scanner.
struct MarkdownScanner {
    // The Markdown file.
    FILE *file;
    // Current line and its capacity.
    struct Span line;
    size_t cap;
    // True if we are inside a fenced code block.
    bool code;
    // Current sector within the document.
    Sector sector;
    // If this line is a heading, 1/2/... for h1/h2/..., otherwise 0.
    int level;
};

// Initializes a Markdown line scanner. Returns true on success.
static bool init_md(struct MarkdownScanner *scan, const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return false;
    }
    scan->file = file;
    scan->line = NULL_SPAN;
    scan->code = false;
    scan->sector = 0;
    scan->level = 0;
    return true;
}

// Closes the Markdown line scanner's file and frees memory for the line.
static void close_md(struct MarkdownScanner *scan) {
    free(scan->line.data);
    scan->line = NULL_SPAN;
    if (scan->file) {
        fclose(scan->file);
        scan->file = NULL;
    }
}

// Advances a Markdown line scanner to the next line. Returns true on successs,
// and false on failure or EOF.
static bool scan_md(struct MarkdownScanner *scan) {
    if (!scan->file) {
        return false;
    }
    const bool prev_blank = scan->line.len <= 1;
    scan->line.len = getline(&scan->line.data, &scan->cap, scan->file);
    scan->level = 0;
    if (scan->line.len == -1) {
        return false;
    }
    if (startswith(scan->line.data, "```")) {
        scan->code = !scan->code;
        return true;
    }
    if (!scan->code && prev_blank) {
        int h = 0;
        while (h < scan->line.len && scan->line.data[h] == '#') h++;
        if (h > 0 && h < scan->line.len && scan->line.data[h] == ' ') {
            scan->sector = DS_NEXT(scan->sector, h);
            scan->level = h;
        }
    }
    return true;
}

// Copies the current line in the scanner to out.
static void copy_md(struct MarkdownScanner *scan, FILE *out) {
    fwrite(scan->line.data, scan->line.len, 1, out);
}

// Buffer sizes for rendering various things.
#define SZ_HREF 256
#define SZ_RELATIVE 32
#define SZ_HEADING 64
#define SZ_LABEL 8

// A Markdown line scanner that picks out "::: highlight" divs.
struct HighlightScanner {
    // The underlying Markdown scanner.
    struct MarkdownScanner md;
    // Current state: START for "::: highlight", END for ":::", INSIDE between
    // the two, and NONE otherwise. START is divided into 1ST (the first
    // highlight block for the current saved heading) and NTH (not the first).
    enum { HL_NONE, HL_START_1ST, HL_START_NTH, HL_INSIDE, HL_END } state;
    // A saved heading and its sector, used for organizing highlights.
    struct Heading heading;
    Sector sector;
    // Storage for the heading spans.
    char line_buf[SZ_HEADING];
    // The sector of the last emitted highlight.
    Sector highlight_sector;
};

// Initializes a highlight scanner. Returns true on success. See also init_md.
static bool init_hl(struct HighlightScanner *scan, const char *path) {
    if (!init_md(&scan->md, path)) {
        return false;
    }
    scan->state = HL_NONE;
    scan->sector = 0;
    scan->highlight_sector = 0;
    return true;
}

// Closes the highlight scanner's file and frees memory for the line.
static void close_hl(struct HighlightScanner *scan) { close_md(&scan->md); }

// Advances a highlight scanner to the next line. See also scan_md.
static bool scan_hl(struct HighlightScanner *scan) {
    if (!scan_md(&scan->md)) {
        return false;
    }
    const struct Span line = scan->md.line;
    switch (scan->state) {
    case HL_NONE:
    case HL_END:
        if (strncmp(line.data, "::: highlight\n", line.len) == 0) {
            assert(scan->sector != 0);
            scan->state = HL_START_NTH;
            if (scan->highlight_sector != scan->sector) {
                scan->highlight_sector = scan->sector;
                scan->state = HL_START_1ST;
            }
        } else {
            scan->state = HL_NONE;
        }
        break;
    case HL_START_1ST:
    case HL_START_NTH:
    case HL_INSIDE:
        if (strncmp(line.data, ":::\n", line.len) == 0) {
            scan->state = HL_END;
        } else {
            scan->state = HL_INSIDE;
        }
        break;
    }
    return true;
}

// Saves the heading in the current line. This creates the difference between
// the HL_START_1ST and HL_START_NTH states.
static void save_heading_hl(struct HighlightScanner *scan) {
    strncpy(scan->line_buf, scan->md.line.data, scan->md.line.len);
    struct Span line = scan->md.line;
    line.data = scan->line_buf;
    scan->heading = parse_md_heading(line);
    scan->sector = scan->md.sector;
}

// A Scheme code line scanner.
struct SchemeScanner {
    // The Scheme file.
    FILE *file;
    // Current line and its capacity.
    struct Span line;
    size_t cap;
    // Current sector within the file.
    Sector sector;
    // If this line is a heading, 1 for :A, 2 for :A.B, 3 for :A.B.C, 4 for
    // :A.B.C.D, and DS_EXERCISE_LEVEL for ?A.B (exercises).
    int level;
    // True if this line is in the "use" block following the heading line.
    bool use;
};

// Line at the end of chapter-*.ss files closing the SICP macro.
const char END_SICP_LINE[] = ") ; end of SICP\n";

// Initializes a Scheme line scanner. Returns true on success.
static bool init_ss(struct SchemeScanner *scan, const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return false;
    }
    scan->file = file;
    scan->line = NULL_SPAN;
    scan->sector = 0;
    scan->level = 0;
    scan->use = false;
    return true;
}

// Closes the Scheme line scanner's file and frees memory for the line.
static void close_ss(struct SchemeScanner *scan) {
    free(scan->line.data);
    scan->line = NULL_SPAN;
    if (scan->file) {
        fclose(scan->file);
        scan->file = NULL;
    }
}

// Advances a Scheme line scanner to the next line. Returns true on successs,
// and false on failure, EOF, or END_SICP_LINE.
static bool scan_ss(struct SchemeScanner *scan) {
    if (!scan->file) {
        return false;
    }
    const bool prev_blank = scan->line.len <= 1;
    scan->line.len = getline(&scan->line.data, &scan->cap, scan->file);
    const bool current_blank = scan->line.len <= 1;
    if (scan->line.len == -1 || strcmp(scan->line.data, END_SICP_LINE) == 0) {
        return false;
    }
    if (scan->level == 0 && prev_blank) {
        if (startswith(scan->line.data, "(Chapter :")) {
            scan->level = 1;
            scan->sector = DS_VALUE(1, scan->line.data[10] - '0');
        } else if (startswith(scan->line.data, "(Section :")) {
            int level = 0;
            Sector sector = 0;
            for (int i = 10; i < scan->line.len; i++) {
                level++;
                sector |= DS_VALUE(level, scan->line.data[i] - '0');
                if (scan->line.data[++i] != '.') break;
            }
            scan->level = level;
            scan->sector = sector;
        } else if (startswith(scan->line.data, "(Exercise ?")) {
            scan->level = DS_EXERCISE_LEVEL;
            int i = 11;
            Sector chapter = scan->line.data[i++] - '0';
            assert(scan->line.data[i++] == '.');
            const char *endptr;
            Sector ex = strtol(scan->line.data + i, (char **)&endptr, 10);
            assert(ex > 0 && endptr);
            scan->sector =
                DS_VALUE(1, chapter) | DS_VALUE(DS_EXERCISE_LEVEL, ex);
        }
    } else if (scan->level != 0) {
        scan->level = 0;
        scan->use = true;
    }
    if (scan->use && current_blank) {
        scan->use = false;
    }
    return true;
}

// Saved state for a Scheme line scanner.
struct SchemeSave {
    // The old scanner, with line set to NULL_SPAN.
    struct SchemeScanner scan;
    // The position in the file as reported by ftell.
    long offset;
};

// Saves a checkpoint in the Scheme file, to be restored later with restore_ss.
static struct SchemeSave save_ss(struct SchemeScanner *scan) {
    struct SchemeSave save = {.scan = *scan};
    save.scan.line = NULL_SPAN;
    save.offset = ftell(scan->file);
    return save;
}

// Restores a checkpoint saved by save_ss. Everything will be as it was at that
// point, except the line will be NULL_SPAN until the next scan.
static void restore_ss(struct SchemeScanner *scan, struct SchemeSave save) {
    *scan = save.scan;
    fseek(scan->file, save.offset, SEEK_SET);
}

// Renders a heading to out. The level determines h1/h2/etc. If id is present,
// uses it and renders a "#" link. If heading.label is present, renders a
// .number span. If href_fmt is present, renders heading.title as a link using
// href_fmt and the printf-style arguments that follow.
static void render_heading(FILE *out, int level, struct Span id,
                           struct Heading heading, const char *href_fmt, ...) {
    if (id.data) {
        fprintf(out,
                "<h%d id=\"%.*s\" class=\"anchor\">"
                "<a class=\"anchor__link link\" href=\"#%.*s\""
                " aria-hidden=\"true\">#</a> ",
                level, id.len, id.data, id.len, id.data);
    } else {
        fprintf(out, "<h%d>", level);
    }
    if (heading.label.data) {
        // Make chapter numbers big.
        const char *big = "";
        if (level == 1 && heading.label.len == 1) {
            big = " number--big";
        }
        fprintf(out, "<span class=\"number%s\">%.*s</span> ", big,
                heading.label.len, heading.label.data);
    }
    if (href_fmt) {
        char href[SZ_HREF];
        va_list args;
        va_start(args, href_fmt);
        vsnprintf(href, sizeof href, href_fmt, args);
        va_end(args);
        fprintf(
            out,
            // &#65279; is the "zero width no-break space" entity. We put this
            // at the start of the span to prevent the external icon from
            // wrapping onto the next line by itself.
            "<a class=\"link\" href=\"%s\">%.*s<span class=\"nowrap\">&#65279;"
            "<svg class=\"external\" width=\"24\" height=\"24\""
            " aria-hidden=\"true\"><use xlink:href=\"#external\"/>"
            "</svg></span></a>",
            href, heading.title.len, heading.title.data);
    } else {
        fwrite(heading.title.data, heading.title.len, 1, out);
    }
    fprintf(out, "</h%d>\n", level);
}

// Renders the start of a highlighted blockquote in highlight.html. Most of the
// work is offloaded to the Lua script. Constructs the id from label and the
// one-based index of this highlight in all the highlights under that label.
static void render_highlight_start(FILE *out, struct Span label, int index) {
    fprintf(out, "<div id=\"%.*s-q%d\" class=\"highlight\">\n", label.len,
            label.data, index);
}

// Renders the end of a highlighted blockquote.
static void render_highlight_end(FILE *out) { fputs("</div>\n", out); }

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
    if (tr->depth > 0) {
        assert(--tr->depth == 0);
    }
    fputs("</ul></nav>\n", out);
}

// Renders an item in a table of contents to out. The depth can be at most one
// greater than the previous item. The link's href is given in printf style.
static void render_toc_item(struct TocRenderer *tr, FILE *out, int depth,
                            struct Heading heading, const char *href_fmt, ...) {
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
            // Put a space between <span> and <a> so that they don't run
            // together in alternative stylesheets like Safari Reader.
            "<span class=\"toc__label\">%.*s</span> "
            "<a href=\"%s\">%.*s</a>",
            heading.label.len, heading.label.data, href, heading.title.len,
            heading.title.data);
}

// Renderer for literate code.
struct LiterateRenderer {
    // State for the current line.
    enum LiterateState { LR_PROSE, LR_CODE } state;
    // True if there is a pending blank line we haven't rendered yet.
    bool pending_blank;
};

// Creates a new table-of-contents renderer.
static struct LiterateRenderer new_literate_renderer(void) {
    return (struct LiterateRenderer){
        .state = LR_PROSE,
        .pending_blank = false,
    };
}

// Ends a section of literate output.
static void end_literate_section(struct LiterateRenderer *lr, FILE *out) {
    if (lr->state == LR_CODE) {
        fputs("```\n", out);
    }
    putc('\n', out);
    lr->state = LR_PROSE;
    lr->pending_blank = false;
}

// Renders a line of prose or code.
static void render_literate(struct LiterateRenderer *lr, FILE *out,
                            struct Span line) {
    if (line.len <= 1 || strcmp(line.data, ";;\n") == 0) {
        lr->pending_blank = true;
        return;
    }
    enum LiterateState prev_state = lr->state;
    lr->state = startswith(line.data, ";; ") ? LR_PROSE : LR_CODE;
    if (lr->state == prev_state && lr->pending_blank) {
        putc('\n', out);
    }
    lr->pending_blank = false;
    switch (lr->state) {
    case LR_PROSE:
        if (prev_state == LR_CODE) {
            fputs("```\n\n", out);
        }
        fwrite(line.data + 3, line.len - 3, 1, out);
        break;
    case LR_CODE:
        if (prev_state == LR_PROSE) {
            fputs("\n```\n", out);
        }
        fwrite(line.data, line.len, 1, out);
        break;
    }
}

// State for rendering (use (id name ...) ...) blocks as nested lists.
struct ImportRenderer {
    // Number of unclosed parens in the use-block.
    int depth;
};

// Creates a new import renderer.
static struct ImportRenderer new_import_renderer(void) {
    return (struct ImportRenderer){
        .depth = 0,
    };
}

// Renders imports from the given line.
static void render_import(struct ImportRenderer *ir, FILE *out,
                          struct Span line) {
    int start = -1;
    bool first = true;
    for (int i = 0; i < line.len; i++) {
        char c = line.data[i];
        switch (c) {
        case '(':
            ir->depth++;
            first = true;
            if (ir->depth == 1) {
                fputs(
                    "<aside class=\"imports\"><h4>Imports:</h4>"
                    "<ul class=\"flat\">\n",
                    out);
            } else if (ir->depth == 2) {
                fputs("<li class=\"flat__item\">\n", out);
            }
            break;
        default:
            if (start == -1) {
                start = i;
            }
            break;
        case ' ':
        case '\n':
        case ')':
            if (ir->depth == 2 && start != -1) {
                int len = i - start;
                char *ptr = line.data + start;
                if (first) {
                    const char sigil = ptr[0];
                    ptr++;
                    len--;
                    const char *prefix = sigil == '?' ? "Ex&nbsp;" : "";
                    fprintf(out,
                            "[%s%.*s](%c%.*s)"
                            "<ul class=\"flat\">\n",
                            prefix, len, ptr, sigil, len, ptr);
                } else {
                    const char *punct = "";
                    if (c == ')'
                        && !(i < line.len && line.data[i + 1] == ')')) {
                        punct = ",";
                    }
                    fprintf(out,
                            "<li class=\"flat__item nowrap\">`%.*s`%s</li>\n",
                            len, ptr, punct);
                }
            }
            if (c == ')') {
                if (ir->depth == 2) {
                    fputs("</ul></li>\n", out);
                } else if (ir->depth == 1) {
                    fputs("</ul></aside>\n", out);
                }
                // Avoid going to -1 when we encounter the heading's ')', which
                // is on the same line as the use-block's ')'.
                if (ir->depth > 0) {
                    ir->depth--;
                }
            }
            first = false;
            start = -1;
            break;
        }
    }
}

// Returns the sector for the given textbook chapter/section.
static Sector make_sector(int chapter, int section) {
    return (Sector)chapter | ((Sector)section << DS_BITS);
}

// Number of chapters in the textbook.
#define NUM_CHAPTERS 5

// Returns the number of sections in the given 1-based chapter.
static int num_sections(int chapter) {
    assert(chapter >= 1 && chapter <= 5);
    return (int[]){3, 5, 5, 4, 5}[chapter - 1];
}

// Base URL for the online SICP textbook.
static const char TEXT_URL_BASE[] =
    "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H";

// Base URL for the online SICP video lectures.
static const char LECTURE_URL_BASE[] =
    "https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/"
    "6-001-structure-and-interpretation-of-computer-programs-spring-2005/"
    "video-lectures";

// Returns the page number to use in the online SICP textbook URL for a given
// sector in text.md. Only takes into account the chapter and section.
static int text_url_num(Sector s) {
    int chapter = DS_INDEX(s, 1);
    int section = DS_INDEX(s, 2);
    if (chapter == 0) {
        assert(section >= 1 && section <= 3);
        return (int[]){3, 5, 7}[section - 1];
    }
    return 8 + chapter + (int[]){0, 3, 8, 13, 17}[chapter - 1] + section;
}

// Writes into buf the SICP video lecture URL suffix (after LECTURE_URL_BASE and
// a slash) for a given lecture heading.
static void level_url_suffix(struct Heading heading, char *buf, int size) {
    assert(size > 0);
    assert(heading.label.data);
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
        } else if (buf[i - 1] != '-') {
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
#define LANGUAGE "language"

// Helpers to construct input/output paths.
#define STDIN "/dev/stdin"
#define STDOUT "/dev/stdout"
#define INPUT(f) ("notes/" f ".md")
#define OUTPUT(f) ("docs/" f ".html")
#define OUTPUT_DIR(d) ("docs/" d "/")

// Helpers to construct link targets.
#define PARENT "../"
#define HREF(p) (p ".html")

// Paths to the Scheme source files for exercises.
const char *const SCHEME_FILES[NUM_CHAPTERS] = {
    "src/sicp/chapter-1.ss", "src/sicp/chapter-2.ss", "src/sicp/chapter-3.ss",
    "src/sicp/chapter-4.ss", "src/sicp/chapter-5.ss",
};

// Extracts chapter C from */C/index.html. Returns true on success.
static bool extract_chapter(int *chapter, const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash > output && strcmp(slash, "/" INDEX ".html") == 0)) {
        return false;
    }
    *chapter = slash[-1] - '0';
    if (!(*chapter >= 1 && *chapter <= NUM_CHAPTERS)) {
        return false;
    }
    return true;
}

// Extracts chapter C and section S from */C/S.html. Returns true on success.
static bool extract_chapter_section(int *chapter, int *section,
                                    const char *output) {
    const char *slash = strrchr(output, '/');
    if (!(slash && slash > output && slash[1])) {
        return false;
    }
    *chapter = slash[-1] - '0';
    *section = slash[1] - '0';
    if (!(*chapter >= 1 && *chapter <= 5 && *section >= 1
          && *section <= num_sections(*chapter))) {
        return false;
    }
    return true;
}

// Returns the "prev" link for a chapter page, possibly using the given buffer.
static const char *href_chapter_prev(int chapter, bool text, char *buf,
                                     int cap) {
    if (chapter == 1) {
        return text ? "../" FRONT ".html" : "../" LANGUAGE ".html";
    }
    snprintf(buf, cap, "../%d/%d.html", chapter - 1, num_sections(chapter - 1));
    return buf;
}

// Returns the "prev" link for a section page, possibly using the given buffer.
static const char *href_section_prev(int section, char *buf, int cap) {
    if (section == 1) {
        return HREF(INDEX);
    }
    snprintf(buf, cap, "%d.html", section - 1);
    return buf;
}

// Returns the "next" link for a section page, possibly using the given buffer.
static const char *href_section_next(int chapter, int section, char *buf,
                                     int cap) {
    const int last_section = num_sections(chapter);
    if (chapter == NUM_CHAPTERS && section == last_section) {
        return NULL;
    }
    if (section == last_section) {
        snprintf(buf, cap, "../%d/" INDEX ".html", chapter + 1);
    } else {
        snprintf(buf, cap, "%d.html", section + 1);
    }
    return buf;
}

// Generates docs/index.html.
static bool gen_index(const char *output) {
    return pandoc((struct PandocOpts){
        .input = INPUT(INDEX),
        .output = output,
        .dest = output,
        .title = "SICP Study",
        .prev = NULL,
        .up = NULL,
        .next = NULL,
    });
}

// Generates docs/text/index.html.
static bool gen_text_index(const char *output) {
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(TEXT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Notes",
                                .prev = NULL,
                                .up = HREF(PARENT INDEX),
                                .next = HREF(HIGHLIGHT),
                            })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Textbook Notes"),
                   NULL);
    while (scan_md(&scan) && scan.sector == 0) {
        copy_md(&scan, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    render_toc_item(&tr, proc.in, 1, TITLE_HEADING("Highlights"),
                    HREF(HIGHLIGHT));
    do {
        if (scan.level == 2) {
            struct Heading h = parse_md_heading(scan.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_toc_item(&tr, proc.in, 1, h, FRONT ".html#%.*s", id.len,
                            id.data);
        }
    } while (scan_md(&scan) && DS_INDEX(scan.sector, 1) == 0);
    do {
        if (scan.level == 1) {
            render_toc_item(&tr, proc.in, 1, parse_md_heading(scan.line),
                            "%d/" INDEX ".html", DS_INDEX(scan.sector, 1));
        } else if (scan.level == 2) {
            render_toc_item(&tr, proc.in, 2, parse_md_heading(scan.line),
                            "%d/%d.html", DS_INDEX(scan.sector, 1),
                            DS_INDEX(scan.sector, 2));
        }
    } while (scan_md(&scan));
    close_md(&scan);
    render_toc_end(&tr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates docs/text/highlight.html.
static bool gen_text_highlight(const char *output) {
    struct HighlightScanner scan;
    if (!init_hl(&scan, INPUT(TEXT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Highlights",
                                .prev = HREF(INDEX),
                                .up = HREF(INDEX),
                                .next = HREF(FRONT),
                            })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Highlights"), NULL);
    int index = 1;
    char label_buf[SZ_LABEL];
    struct Span label = SPAN("front");
    while (scan_hl(&scan)) {
        switch (scan.state) {
        case HL_NONE:
            if (DS_INDEX(scan.md.sector, 1) == 0) {
                if (scan.md.level == 2) {
                    save_heading_hl(&scan);
                }
            } else {
                if (scan.md.level == 1) {
                    save_heading_hl(&scan);
                }
                if (scan.md.level == 1 || scan.md.level == 2) {
                    label = parse_md_heading(scan.md.line).label;
                    strncpy(label_buf, label.data, label.len);
                    label.data = label_buf;
                    index = 1;
                }
            }
            break;
        case HL_START_1ST:;
            struct Span id = scan.heading.label;
            char buf[SZ_HEADING];
            if (!id.data) {
                id = tolower_s(scan.heading.title, buf, sizeof buf);
            }
            render_heading(proc.in, 2, id, scan.heading, "%s-%d.html",
                           TEXT_URL_BASE, text_url_num(scan.sector));
            // fallthrough
        case HL_START_NTH:
            render_highlight_start(proc.in, label, index++);
            break;
        case HL_INSIDE:
            copy_md(&scan.md, proc.in);
            break;
        case HL_END:
            render_highlight_end(proc.in);
            break;
        }
    }
    close_hl(&scan);
    return finish_pandoc(&proc, output);
}

// Generates docs/text/front.html.
static bool gen_text_front(const char *output) {
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(TEXT))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Frontmatter Notes",
                                .prev = HREF(HIGHLIGHT),
                                .up = HREF(INDEX),
                                .next = HREF("1/" INDEX),
                            })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Frontmatter"), NULL);
    while (scan_md(&scan) && scan.sector == 0) continue;
    do {
        if (scan.level > 1) {
            struct Heading h = parse_md_heading(scan.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            render_heading(proc.in, scan.level, id, h, "%s-%d.html",
                           TEXT_URL_BASE, text_url_num(scan.sector));
        } else {
            copy_md(&scan, proc.in);
        }
    } while (scan_md(&scan) && scan.level != 1);
    close_md(&scan);
    return finish_pandoc(&proc, output);
}

// Generates docs/text/*/index.html.
static bool gen_text_chapter(const char *output) {
    int chapter;
    if (!extract_chapter(&chapter, output)) {
        fprintf(stderr, "%s: invalid text chapter\n", output);
        return false;
    }
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(TEXT))) {
        return false;
    }
    char title[] = "SICP Chapter _ Notes";
    title[13] = '0' + chapter;
    char prev_buf[SZ_HREF];
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = title,
                                .prev = href_chapter_prev(
                                    chapter, true, prev_buf, sizeof prev_buf),
                                .up = HREF(PARENT INDEX),
                                .next = HREF("1"),
                            })) {
        return false;
    }
    const Sector target_sector = make_sector(chapter, 0);
    while (scan_md(&scan) && scan.sector != target_sector) continue;
    assert(scan.sector == target_sector);
    struct Heading h = parse_md_heading(scan.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h, "%s-%d.html", TEXT_URL_BASE,
                   text_url_num(target_sector));
    while (scan_md(&scan) && scan.level == 0) {
        copy_md(&scan, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    do {
        if (scan.level == 2) {
            int section = DS_INDEX(scan.sector, 2);
            render_toc_item(&tr, proc.in, 1, parse_md_heading(scan.line),
                            "%d.html", section);
        } else if (scan.level == 3) {
            int section = DS_INDEX(scan.sector, 2);
            int subsection = DS_INDEX(scan.sector, 3);
            render_toc_item(&tr, proc.in, 2, parse_md_heading(scan.line),
                            "%d.html#%d.%d.%d", section, chapter, section,
                            subsection);
        }
    } while (scan_md(&scan) && scan.level != 1);
    close_md(&scan);
    render_toc_end(&tr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates docs/text/*/*.html.
static bool gen_text_section(const char *output) {
    int chapter, section;
    if (!extract_chapter_section(&chapter, &section, output)) {
        fprintf(stderr, "%s: invalid text section\n", output);
        return false;
    }
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(TEXT))) {
        return false;
    }
    char title[] = "SICP Section _._ Notes";
    title[13] = '0' + chapter;
    title[15] = '0' + section;
    char prev_buf[SZ_HREF], next_buf[SZ_HREF];
    struct PandocProc proc;
    if (!fork_pandoc(
            &proc,
            (struct PandocOpts){
                .input = STDIN,
                .output = STDOUT,
                .dest = output,
                .title = title,
                .prev = href_section_prev(section, prev_buf, sizeof prev_buf),
                .up = HREF(INDEX),
                .next = href_section_next(chapter, section, next_buf,
                                          sizeof next_buf),
            })) {
        return false;
    }
    const Sector target_sector = make_sector(chapter, section);
    while (scan_md(&scan) && scan.sector != target_sector) continue;
    assert(scan.sector == target_sector);
    struct Heading h = parse_md_heading(scan.line);
    assert(h.label.data);
    const int page_num = text_url_num(target_sector);
    render_heading(proc.in, 1, NULL_SPAN, h, "%s-%d.html", TEXT_URL_BASE,
                   page_num);
    while (scan_md(&scan) && scan.level != 1 && scan.level != 2) {
        if (scan.level >= 3) {
            h = parse_md_heading(scan.line);
            if (h.label.data) {
                assert(scan.level == 3);
                render_heading(proc.in, 2, h.label, h,
                               "%s-%d.html#%%25_sec_%d.%d.%d", TEXT_URL_BASE,
                               page_num, chapter, section,
                               DS_INDEX(scan.sector, 3));
            } else {
                assert(scan.level == 4);
                char id[SZ_LABEL];
                snprintf(id, sizeof id, "%d.%d.%d.%d", chapter, section,
                         DS_INDEX(scan.sector, 3), DS_INDEX(scan.sector, 4));
                render_heading(proc.in, 3, SPAN(id), h, NULL);
            }
        } else {
            copy_md(&scan, proc.in);
        }
    }
    close_md(&scan);
    return finish_pandoc(&proc, output);
}

// Generates docs/lecture/index.html.
static bool gen_lecture_index(const char *output) {
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(LECTURE))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Lecture Notes",
                                .prev = NULL,
                                .up = HREF(PARENT INDEX),
                                .next = HREF(HIGHLIGHT),
                            })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Lecture Notes"), NULL);
    while (scan_md(&scan) && scan.sector == 0) {
        copy_md(&scan, proc.in);
    }
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    render_toc_item(&tr, proc.in, 1, TITLE_HEADING("Highlights"),
                    HREF(HIGHLIGHT));
    do {
        if (scan.level == 1) {
            struct Heading h = parse_md_heading(scan.line);
            char buf[SZ_LABEL];
            struct Span href = tolower_s(h.label, buf, sizeof buf);
            render_toc_item(&tr, proc.in, 1, h, "%.*s.html", href.len,
                            href.data);
        }
    } while (scan_md(&scan));
    close_md(&scan);
    render_toc_end(&tr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates docs/lecture/highlight.html.
static bool gen_lecture_highlight(const char *output) {
    struct HighlightScanner scan;
    if (!init_hl(&scan, INPUT(LECTURE))) {
        return false;
    }
    const struct PandocOpts opts = {
        .input = STDIN,
        .output = STDOUT,
        .dest = output,
        .title = "SICP Lecture Highlights",
        .prev = HREF(INDEX),
        .up = HREF(INDEX),
        .next = HREF("1a"),
    };
    struct PandocProc proc;
    if (!fork_pandoc(&proc, opts)) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Highlights"), NULL);
    int index = 0;
    while (scan_hl(&scan)) {
        switch (scan.state) {
        case HL_NONE:
            if (scan.md.level == 1) {
                save_heading_hl(&scan);
                index = 1;
            }
            break;
        case HL_START_1ST:;
            char buf[SZ_LABEL];
            struct Span label = tolower_s(scan.heading.label, buf, sizeof buf);
            char suffix[SZ_HEADING];
            level_url_suffix(scan.heading, suffix, sizeof suffix);
            render_heading(proc.in, 2, label, scan.heading, "%s/%s",
                           LECTURE_URL_BASE, suffix);
            // fallthrough
        case HL_START_NTH:;
            render_highlight_start(proc.in, label, index++);
            break;
        case HL_INSIDE:
            copy_md(&scan.md, proc.in);
            break;
        case HL_END:
            render_highlight_end(proc.in);
            break;
        }
    }
    close_hl(&scan);
    return finish_pandoc(&proc, output);
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
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(LECTURE))) {
        return false;
    }
    char title[SZ_HEADING];
    snprintf(title, sizeof title, "SICP Lecture %d%c Notes", number,
             toupper(a_or_b));
    const char *prev;
    char prev_buf[SZ_RELATIVE];
    if (number == 1 && a_or_b == 'a') {
        prev = HREF(HIGHLIGHT);
    } else {
        int ab = a_or_b - 'a';
        snprintf(prev_buf, sizeof prev_buf, "%d%c.html", number + ab - 1,
                 'b' - ab);
        prev = prev_buf;
    }
    const char *next;
    char next_buf[SZ_RELATIVE];
    if (number == 10 && a_or_b == 'b') {
        next = NULL;
    } else {
        int ab = a_or_b - 'a';
        snprintf(next_buf, sizeof next_buf, "%d%c.html", number + ab, 'b' - ab);
        next = next_buf;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = title,
                                .prev = prev,
                                .up = HREF(INDEX),
                                .next = next,
                            })) {
        return false;
    }
    const Sector target_sector = 1 + (number - 1) * 2 + a_or_b - 'a';
    while (scan_md(&scan) && scan.sector != target_sector) continue;
    assert(scan.sector == target_sector);
    struct Heading h = parse_md_heading(scan.line);
    char suffix[SZ_HEADING];
    level_url_suffix(h, suffix, sizeof suffix);
    render_heading(proc.in, 1, NULL_SPAN, h, "%s/%s", LECTURE_URL_BASE, suffix);
    while (scan_md(&scan) && scan.level != 1) {
        if (scan.level > 1) {
            h = parse_md_heading(scan.line);
            assert(!h.label.data);
            char id[SZ_LABEL];
            write_dotted_section(id, sizeof id, scan.sector >> DS_BITS);
            render_heading(proc.in, scan.level, SPAN(id), h, NULL);
        } else {
            copy_md(&scan, proc.in);
        }
    }
    close_md(&scan);
    return finish_pandoc(&proc, output);
invalid:
    fprintf(stderr, "%s: invalid lecture\n", output);
    return false;
}

// Generates docs/exercise/index.html.
static bool gen_exercise_index(const char *output) {
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(EXERCISE))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Exercise Solutions",
                                .prev = NULL,
                                .up = HREF(PARENT INDEX),
                                .next = HREF(LANGUAGE),
                            })) {
        return false;
    }
    render_heading(proc.in, 1, NULL_SPAN, TITLE_HEADING("Exercise Solutions"),
                   NULL);
    while (scan_md(&scan) && scan.sector == 0) {
        copy_md(&scan, proc.in);
    }
    struct Heading language = parse_md_heading(scan.line);
    assert(!language.label.data);
    close_md(&scan);
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    render_toc_item(&tr, proc.in, 1, language, HREF(LANGUAGE));
    for (int i = 0; i < NUM_CHAPTERS; i++) {
        struct SchemeScanner scan;
        if (!init_ss(&scan, SCHEME_FILES[i])) {
            return false;
        }
        while (scan_ss(&scan)) {
            if (scan.level == 1) {
                render_toc_item(&tr, proc.in, 1, parse_ss_heading(scan.line),
                                "%d/" INDEX ".html", DS_INDEX(scan.sector, 1));
            } else if (scan.level == 2) {
                render_toc_item(&tr, proc.in, 2, parse_ss_heading(scan.line),
                                "%d/%d.html", DS_INDEX(scan.sector, 1),
                                DS_INDEX(scan.sector, 2));
            }
        }
        close_ss(&scan);
    }
    render_toc_end(&tr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates docs/exercise/language.html.
static bool gen_exercise_language(const char *output) {
    struct MarkdownScanner scan;
    if (!init_md(&scan, INPUT(EXERCISE))) {
        return false;
    }
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = "SICP Exercises Language",
                                .prev = HREF(INDEX),
                                .up = HREF(INDEX),
                                .next = HREF("1/" INDEX),
                            })) {
        return false;
    }
    while (scan_md(&scan) && scan.sector != 1) continue;
    do {
        if (scan.level > 1) {
            struct Heading h = parse_md_heading(scan.line);
            assert(!h.label.data);
            char buf[SZ_HEADING];
            struct Span id = tolower_s(h.title, buf, sizeof buf);
            for (int i = 0; i < id.len; i++) assert(id.data[i] != ' ');
            render_heading(proc.in, scan.level, id, h, NULL);
        } else {
            copy_md(&scan, proc.in);
        }
    } while (scan_md(&scan));
    close_md(&scan);
    return finish_pandoc(&proc, output);
}

// Generates docs/exercise/*/index.html.
static bool gen_exercise_chapter(const char *output) {
    int chapter;
    if (!extract_chapter(&chapter, output)) {
        fprintf(stderr, "%s: invalid exercise chapter\n", output);
        return false;
    }
    struct SchemeScanner scan;
    if (!init_ss(&scan, SCHEME_FILES[chapter - 1])) {
        return false;
    }
    char title[] = "SICP Chapter _ Exercises";
    title[13] = '0' + chapter;
    char prev_buf[SZ_HREF];
    struct PandocProc proc;
    if (!fork_pandoc(&proc, (struct PandocOpts){
                                .input = STDIN,
                                .output = STDOUT,
                                .dest = output,
                                .title = title,
                                .prev = href_chapter_prev(
                                    chapter, false, prev_buf, sizeof prev_buf),
                                .up = HREF(PARENT INDEX),
                                .next = HREF("1"),
                            })) {
        return false;
    }
    const Sector target_sector = make_sector(chapter, 0);
    while (scan_ss(&scan) && scan.sector != target_sector) continue;
    assert(scan.sector == target_sector);
    const struct SchemeSave save = save_ss(&scan);
    struct Heading h = parse_ss_heading(scan.line);
    assert(h.label.data);
    render_heading(proc.in, 1, NULL_SPAN, h, "%s-%d.html", TEXT_URL_BASE,
                   text_url_num(target_sector));
    render_heading(proc.in, 2, SPAN("exercises"), TITLE_HEADING("Exercises"),
                   NULL);
    fputs("<ol class=\"exercises\">\n", proc.in);
    int section = 0;
    do {
        if (scan.level == 2) {
            section = DS_INDEX(scan.sector, 2);
        } else if (scan.level == DS_EXERCISE_LEVEL) {
            int exercise = DS_INDEX(scan.sector, DS_EXERCISE_LEVEL);
            fprintf(proc.in,
                    "<li class=\"exercises__item\">"
                    "<a href=\"%d.html#ex%d.%d\">%d.%02d</a>"
                    "</li>\n",
                    section, chapter, exercise, chapter, exercise);
        }
    } while (scan_ss(&scan));
    fputs("</ol>\n", proc.in);
    restore_ss(&scan, save);
    struct TocRenderer tr = new_toc_renderer();
    render_toc_start(&tr, proc.in);
    do {
        if (scan.level == 2) {
            int section = DS_INDEX(scan.sector, 2);
            render_toc_item(&tr, proc.in, 1, parse_ss_heading(scan.line),
                            "%d.html", section);
        } else if (scan.level == 3) {
            int section = DS_INDEX(scan.sector, 2);
            int subsection = DS_INDEX(scan.sector, 3);
            render_toc_item(&tr, proc.in, 2, parse_ss_heading(scan.line),
                            "%d.html#%d.%d.%d", section, chapter, section,
                            subsection);
        }
    } while (scan_ss(&scan));
    close_ss(&scan);
    render_toc_end(&tr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates docs/exercise/*/*.html.
static bool gen_exercise_section(const char *output) {
    int chapter, section;
    if (!extract_chapter_section(&chapter, &section, output)) {
        fprintf(stderr, "%s: invalid exercise section\n", output);
        return false;
    }
    struct SchemeScanner scan;
    if (!init_ss(&scan, SCHEME_FILES[chapter - 1])) {
        return false;
    }
    char title[] = "SICP Section _._ Exercises";
    title[13] = '0' + chapter;
    title[15] = '0' + section;
    char prev_buf[SZ_HREF], next_buf[SZ_HREF];
    struct PandocProc proc;
    if (!fork_pandoc(
            &proc,
            (struct PandocOpts){
                .input = STDIN,
                .output = STDOUT,
                .dest = output,
                .title = title,
                .prev = href_section_prev(section, prev_buf, sizeof prev_buf),
                .up = HREF(INDEX),
                .next = href_section_next(chapter, section, next_buf,
                                          sizeof next_buf),
            })) {
        return false;
    }
    const Sector target_sector = make_sector(chapter, section);
    while (scan_ss(&scan) && scan.sector != target_sector) continue;
    assert(scan.sector == target_sector);
    struct Heading h = parse_ss_heading(scan.line);
    assert(h.label.data);
    const int page_num = text_url_num(target_sector);
    render_heading(proc.in, 1, NULL_SPAN, h, "%s-%d.html", TEXT_URL_BASE,
                   page_num);
    struct LiterateRenderer lr = new_literate_renderer();
    struct ImportRenderer ir = new_import_renderer();
    while (scan_ss(&scan) && scan.level != 1 && scan.level != 2) {
        if (scan.level >= 3) {
            end_literate_section(&lr, proc.in);
            h = parse_ss_heading(scan.line);
            assert(h.label.data);
            if (scan.level == 3) {
                render_heading(proc.in, 2, h.label, h,
                               "%s-%d.html#%%25_sec_%d.%d.%d", TEXT_URL_BASE,
                               page_num, chapter, section,
                               DS_INDEX(scan.sector, 3));
            } else if (scan.level == 4) {
                render_heading(proc.in, 3, h.label, h, NULL);
            } else if (scan.level == DS_EXERCISE_LEVEL) {
                struct Span num = h.label;
                char id_buf[SZ_LABEL], title_buf[SZ_HEADING];
                snprintf(id_buf, sizeof id_buf, "ex%.*s", num.len, num.data);
                snprintf(title_buf, sizeof title_buf, "Exercise %.*s", num.len,
                         num.data);
                h = (struct Heading){
                    .label = NULL_SPAN,
                    .title = SPAN(title_buf),
                };
                render_heading(proc.in, 3, SPAN(id_buf), h,
                               "%s-%d.html#%%25_thm_%.*s", TEXT_URL_BASE,
                               page_num, num.len, num.data);
            }
        } else if (scan.use) {
            render_import(&ir, proc.in, scan.line);
        } else {
            render_literate(&lr, proc.in, scan.line);
        }
    }
    close_ss(&scan);
    end_literate_section(&lr, proc.in);
    return finish_pandoc(&proc, output);
}

// Generates the given output file.
static bool gen(const char *output) {
    if (strcmp(output, OUTPUT(INDEX)) == 0) {
        return gen_index(output);
    }
    if (startswith(output, OUTPUT_DIR(TEXT))) {
        if (strcmp(output, OUTPUT(TEXT "/" INDEX)) == 0) {
            return gen_text_index(output);
        }
        if (strcmp(output, OUTPUT(TEXT "/" HIGHLIGHT)) == 0) {
            return gen_text_highlight(output);
        }
        if (strcmp(output, OUTPUT(TEXT "/" FRONT)) == 0) {
            return gen_text_front(output);
        }
        if (endswith(output, "/" INDEX ".html")) {
            return gen_text_chapter(output);
        }
        return gen_text_section(output);
    }
    if (startswith(output, OUTPUT_DIR(LECTURE))) {
        if (strcmp(output, OUTPUT(LECTURE "/" INDEX)) == 0) {
            return gen_lecture_index(output);
        }
        if (strcmp(output, OUTPUT(LECTURE "/" HIGHLIGHT)) == 0) {
            return gen_lecture_highlight(output);
        }
        return gen_lecture_page(output);
    }
    if (startswith(output, OUTPUT_DIR(EXERCISE))) {
        if (strcmp(output, OUTPUT(EXERCISE "/" INDEX)) == 0) {
            return gen_exercise_index(output);
        }
        if (strcmp(output, OUTPUT(EXERCISE "/" LANGUAGE)) == 0) {
            return gen_exercise_language(output);
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
