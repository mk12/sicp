// Copyright 2020 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// Path to the output file.
static const char *output;

// Returns true if s starts with the prefix p.
#define STARTSWITH(s, p) (strncmp(s, p, strlen(p)) == 0)

// Checks if the output is a given file, or is under a given directory.
#define IS_FILE(f) (strcmp(output, "docs/" f ".html") == 0)
#define IS_UNDER(d) (STARTSWITH(output, "docs/" d "/"))

// Constructs the path for an input Markdown file.
#define MARKDOWN(f) ("notes/" f ".md")

// Name of the pandoc binary.
static const char *const PANDOC = "pandoc";

// Default options to pass to pandoc.
static const char *const DEFAULT_OPTIONS[] = {
    "--from=markdown",
    "--to=html5",
    "--standalone",
    "--template=notes/template.html",
    "--highlight-style=pygments",
};
const int N_DEFAULT_OPTIONS =
    sizeof DEFAULT_OPTIONS / sizeof DEFAULT_OPTIONS[0];

// Options used to invoke pandoc.
struct PandocOptions {
    // String that identifies the active page/tab.
    const char *id;
    // Contents of <title>...</title>.
    const char *title;
    // Relative path to the root of the website.
    const char *root;
    // Path to the input file.
    const char *input;
    // Depth for table of contents (0 for no TOC).
    int toc_depth;
};

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

// Invokes pandoc, printing the command before executing it. Normally does not
// return since it replaces the current process. Returns 1 on error.
static int pandoc(const struct PandocOptions opts) {
    const int LEN =
        1  // pandoc
        + N_DEFAULT_OPTIONS
        + 3   // id, title, root
        + 2   // (optional) toc, toc-depth
        + 2   // -o output
        + 1   // input
        + 1;  // NULL
    const char *argv[LEN];
    int i = 0;
    argv[i++] = PANDOC;
    memcpy(argv + i, DEFAULT_OPTIONS, N_DEFAULT_OPTIONS * sizeof argv[0]);
    i += N_DEFAULT_OPTIONS;
    // We don't have to clean up these allocations because all this process's
    // memory will go poof after we call exec.
    argv[i++] = concat("--metadata=", opts.id);
    argv[i++] = concat("--metadata=title:", opts.title);
    argv[i++] = concat("--metadata=root:", opts.root);
    char toc_depth_arg[] = "--toc-depth=N";
    if (opts.toc_depth > 0) {
        assert(opts.toc_depth <= 9);
        toc_depth_arg[sizeof toc_depth_arg - 2] = '0' + opts.toc_depth;
        argv[i++] = "--toc";
        argv[i++] = toc_depth_arg;
    }
    argv[i++] = "-o";
    argv[i++] = output;
    argv[i++] = opts.input;
    argv[i++] = NULL;
    assert(i <= LEN);
    for (int j = 0; j < i - 1; j++) {
        // Quote the title argument since it has spaces.
        const char *quote = j == 5 ? "'" : "";
        const char *space = j == i - 2 ? "" : " ";
        printf("%s%s%s%s", quote, argv[j], quote, space);
    }
    putchar('\n');
    execvp(PANDOC, (char **)argv);
    perror(NULL);
    return 1;
}

// A Markdown section is represented by an integer s, where:
//
//     s & (MD_MASK << 0*MD_LEVEL)  gives the h1 level,
//     s & (MD_MASK << 1*MD_LEVEL)  gives the h2 level,
//     ...
//
// A level of 0 means that heading has not been encountered yet.
typedef uint64_t MarkdownSection;
#define MD_MASK 0xff
#define MD_LEVEL 8

// State for the Markdown line scanner.
struct MarkdownState {
    // File descriptor.
    FILE *file;
    // Current line, capacity, and length (excluding null terminator).
    char *line;
    size_t cap;
    ssize_t len;
    // True if we are inside a fenced code block.
    bool code;
    // Current section within the document.
    MarkdownSection section;
    // If this line is a heading, 0/1/... for h1/h2/..., otherwise -1.
    int heading;
};

// Initializes a Markdown line scanner. Returns 1 on error.
static int init_md(struct MarkdownState *state, const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return 1;
    }
    state->file = file;
    state->line = NULL;
    state->len = 1;
    state->code = false;
    state->section = 0;
    state->heading = -1;
    return 0;
}

// Advances a Markdown line scanner to the next line. Returns true if there are
// still more lines to scan.
static bool scan_md(struct MarkdownState *state) {
    const bool prev_blank = state->len == 1;
    state->len = getline(&state->line, &state->cap, state->file);
    state->heading = -1;
    if (state->len == -1) {
        fclose(state->file);
        return false;
    }
    if (STARTSWITH(state->line, "```")) {
        state->code = !state->code;
        return true;
    }
    if (!state->code && prev_blank) {
        int h = 0;
        while (h < state->len && state->line[h++] == '#');
        if (h > 0 && h < state->len && state->line[h] == ' ') {
            const MarkdownSection mask = (1 << h*MD_LEVEL) - 1;
            const MarkdownSection inc = 1 << (h - 1)*MD_LEVEL;
            state->section = (state->section & mask) + inc;
            state->heading = h - 1;
        }
    }
    return true;
}

// Generates the index page for text.md or lecture.md. Returns 1 on error.
static int gen_index_from_md(struct PandocOptions opts) {
    // Fork the process, using a pipe for the child's stdin.
    int child_stdin[2];
    if (pipe(child_stdin) == -1) {
        perror("pipe");
        return 1;
    }
    pid_t pid;
    if ((pid = fork()) == -1) {
        perror("fork");
        close(child_stdin[0]);
        close(child_stdin[1]);
        return 1;
    }
    if (pid == 0) {
        // Close the write-side of child_stdin.
        close(child_stdin[1]);
        // Move the read-side of child_stdin to file descriptor 0 (stdin).
        dup2(child_stdin[0], 0);
        close(child_stdin[0]);
        // Invoke pandoc on stdin.
        opts.input = "/dev/stdin";
        opts.toc_depth = 0;
        return pandoc(opts);
    }
    // Close the read-side of child_stdin.
    close(child_stdin[0]);
    // Write to the write-side of child_stdin.
    struct MarkdownState state;
    init_md(&state, opts.input);
    while (scan_md(&state)) {
        dprintf(child_stdin[1], "");
    }
    // Wait for the child process to finish.
    if (waitpid(pid, NULL, 0) == -1) {
        perror("waitpid");
        return 1;
    };
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s OUT_FILE\n", argv[0]);
        return 1;
    }
    output = argv[1];
    if (IS_FILE("index")) {
        return pandoc((struct PandocOptions){
            .id = "index",
            .title = "SICP Study",
            .root = "",
            .input = MARKDOWN("index"),
            .toc_depth = 0,
        });
    }
    if (IS_FILE("quote")) {
        return pandoc((struct PandocOptions){
            .id = "quote",
            .title = "SICP Quotes",
            .root = "",
            .input = MARKDOWN("quote"),
            .toc_depth = 2,
        });
    }
    if (IS_UNDER("text")) {
        if (IS_FILE("text/index")) {
            return gen_index_from_md((struct PandocOptions) {
                .id = "text",
                .title = "SICP Text Notes",
                .root = "..",
                .input = MARKDOWN("text"),
                .toc_depth = 3,
            });
        }
    }  
    if (IS_UNDER("lecture")) {
        if (IS_FILE("lecture/index")) {
            return gen_index_from_md((struct PandocOptions) {
                .id = "lecture",
                .title = "SICP Lecture Notes",
                .root = "..",
                .input = MARKDOWN("text"),
                .toc_depth = 3,
            });
        }
    }
    if (IS_UNDER("exercise")) {
        if (IS_FILE("exercise/index")) {

        } else {

        }
    }
    fprintf(stderr, "%s: %s: invalid output file\n", argv[0], argv[1]);
    return 1;
}
