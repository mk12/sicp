// Copyright 2021 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

@import AppKit.NSSpellChecker;
@import Foundation.NSOrthography;
@import Foundation.NSSpellServer;
@import Foundation.NSString;
@import Foundation.NSTextCheckingResult;

// Wrapper around NSSpellChecker that stores the document tag.
struct SpellChecker {
    NSSpellChecker *checker;
    int tag;
};

// State for spell checking.
struct State {
    // Spell checker.
    struct SpellChecker spell;
    // Cumulative status, set to 1 if there are any errors.
    int status;
    // The file being scanned.
    const char *path;
    FILE *file;
    // Current line with its length and capacity.
    int lineno;
    char *line;
    ssize_t len;
    size_t cap;
};

// Initializes spell checker state for the given file. Returns true on success.
static bool init_state(struct State *state, struct SpellChecker spell,
                       const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return false;
    }
    state->spell = spell;
    state->status = 0;
    state->path = path;
    state->file = file;
    state->lineno = 0;
    state->line = NULL;
    state->len = 0;
    state->cap = 0;
    return true;
}

// Closes the state's file.
static void close_state(struct State *state) {
    if (state->file) {
        fclose(state->file);
        state->file = NULL;
    }
}

// Scans a line from the file. Returns false on error or EOF.
static bool scan(struct State *state) {
    if (!state->file) {
        return false;
    }
    state->len = getline(&state->line, &state->cap, state->file);
    state->lineno++;
    return state->len != -1;
}

// An inclusive range of lines, for reporting in error messages.
struct Source {
    int first;
    int last;
};

// Checks spelling and grammar in a string, printing error messages to stdout.
static void check_string(struct State *state, struct Source src,
                         NSString *str) {
    NSArray<NSTextCheckingResult *> *results =
        [state->spell.checker checkString:str
                                    range:NSMakeRange(0, str.length)
                                    types:NSTextCheckingTypeSpelling
                                          | NSTextCheckingTypeGrammar
                                  options:NULL
                   inSpellDocumentWithTag:state->spell.tag
                              orthography:NULL
                                wordCount:NULL];
    const char *c_str = str.UTF8String;
    for (NSTextCheckingResult *result in results) {
        NSRange range = result.range;
        NSString *replacement = NULL;
        NSArray<NSDictionary<NSString *, id> *> *grammarDetails = NULL;
        switch (result.resultType) {
        case NSTextCheckingTypeSpelling:
            replacement = [state->spell.checker
                correctionForWordRange:range
                              inString:str
                              language:state->spell.checker.language
                inSpellDocumentWithTag:state->spell.tag];
            break;
        case NSTextCheckingTypeGrammar:
            grammarDetails = result.grammarDetails;
            break;
        default:
            assert(false);
        }
        state->status = 1;
        fputs(state->path, stdout);
        if (src.first == src.last) {
            printf("%d: ", src.first);
        } else {
            printf("%d-%d: ", src.first, src.last);
        }
        printf("“%.*s”", (int)range.length, c_str + range.location);
        if (replacement) {
            printf(" (did you mean “%s”?)", replacement.UTF8String);
        }
        if (grammarDetails) {
            for (NSDictionary<NSString *, id> *detail in grammarDetails) {
                printf("\n    ");
                NSValue *value = detail[NSGrammarRange];
                if (value) {
                    NSRange subrange = value.rangeValue;
                    printf("“%.*s”", (int)subrange.length, c_str + range.location + subrange.location);
                }
                NSString *description = detail[NSGrammarUserDescription];
                if (description) {
                    printf(": %s", description.UTF8String);
                }
            }
        }
        putchar('\n');
    }
}

// Current mode within a Markdown file.
enum Mode {
    // Normal mode.
    M_NORMAL,
    // A fenced code block, surrounded by "```".
    M_CODE_BLOCK_START,
    M_CODE_BLOCK,
    M_CODE_BLOCK_END,
    // A block of display math, surrounded by "$$".
    M_DISPLAY_MATH_START,
    M_DISPLAY_MATH,
    M_DISPLAY_MATH_END,
    // An exercise div, surrounded by "::: exercises" and ":::".
    M_EXERCISE_DIV_START,
    M_EXERCISE_DIV,
    M_EXERCISE_DIV_END,
    // An HTML <pre> block.
    M_HTML_PRE_START,
    M_HTML_PRE,
    M_HTML_PRE_END,
};

// Returns the new mode for the given line of Markdown.
static enum Mode next_mode(enum Mode old, const char *line) {
    switch (old) {
    case M_NORMAL:
    case M_CODE_BLOCK_END:
    case M_DISPLAY_MATH_END:
    case M_EXERCISE_DIV_END:
    case M_HTML_PRE_END:
        if (strcmp(line, "```\n") == 0) {
            return M_CODE_BLOCK_START;
        };
        if (strcmp(line, "$$\n") == 0) {
            return M_DISPLAY_MATH_START;
        }
        if (strcmp(line, "::: exercises\n") == 0) {
            return M_EXERCISE_DIV_START;
        }
        if (strncmp(line, "<pre>", 5) == 0) {
            return M_HTML_PRE_START;
        }
        return M_NORMAL;
    case M_CODE_BLOCK_START:
    case M_CODE_BLOCK:
        if (strcmp(line, "```\n") == 0) {
            return M_CODE_BLOCK_END;
        };
        return M_CODE_BLOCK;
    case M_DISPLAY_MATH_START:
    case M_DISPLAY_MATH:
        if (strcmp(line, "$$\n") == 0) {
            return M_DISPLAY_MATH_END;
        };
        return M_DISPLAY_MATH;
    case M_EXERCISE_DIV_START:
    case M_EXERCISE_DIV:
        if (strcmp(line, ":::\n") == 0) {
            return M_EXERCISE_DIV_END;
        };
        return M_EXERCISE_DIV;
    case M_HTML_PRE_START:
    case M_HTML_PRE:
        if (strcmp(line, "</code></pre>\n") == 0) {
            return M_HTML_PRE_END;
        };
        return M_HTML_PRE;
    }
}

// Converts a line of Markdown to plain text, inline. Returns a pointer into s
// indicating the beginning (e.g. would go past the hashes in headings), or NULL
// if this line has no content that should be spellchecked.
static char *strip_markdown(char *s) {
    if (s[0] == '[' || (s[0] == '$' && s[1] == '$')
        || strncmp(s, ":::", 3) == 0) {
        return NULL;
    }
    if (*s == '#') {
        while (*s == '#') s++;
        while (*s == ' ') s++;
    } else if (*s == '>') {
        s++;
        while (*s == ' ') s++;
    } else {
        while (*s == ' ') s++;
        if (*s == '-') {
            s++;
            while (*s == ' ') s++;
        } else if (*s > '1' && *s < '9') {
            char *p = s;
            while (*p > '1' && *p < '9') p++;
            if (*p == '.') {
                p++;
                if (*p == ' ') {
                    while (*p == ' ') p++;
                    s = p;
                }
            }
        }
    }
    char *const start = s;
    char *p = s;
    char delim;
    while (*p) {
        switch (*p) {
        case '\\':
            p++;
            if (!*p) goto end;
            *s++ = *p++;
            break;
        case '`':
        case '$':
            delim = *p;
            p++;
            while (*p && *p != delim) p++;
            if (!*p) goto end;
            p++;
            *s++ = 'C';
            *s++ = 'C';
            break;
        case '<':
            p++;
            while (*p && *p != '>') p++;
            if (!*p) goto end;
            p++;
            break;
        case '[':
            if (p[1] == '@') {
                while (*p && *p != ']') p++;
                if (!*p) goto end;
                p++;
                break;
            }
            // fallthrough
        case '_':
            p++;
            break;
        case ']':
            p++;
            if (*p == '[')
                delim = ']';
            else if (*p == '(')
                delim = ')';
            else
                assert(false);
            while (*p && *p != delim) p++;
            if (!*p) goto end;
            p++;
            break;
        default:
            *s++ = *p++;
            break;
        }
    }
end:
    *s = '\0';
    return start;
}

// Spellchecks a Markdown file.
static void check_markdown(struct State *state, bool print) {
    enum Mode mode = M_NORMAL;
    while (scan(state)) {
        mode = next_mode(mode, state->line);
        if (mode != M_NORMAL) {
            continue;
        }
        char *plain = strip_markdown(state->line);
        if (!plain) continue;
        if (print) {
            fputs(plain, stdout);
        } else {
            NSString *line = @(plain);
            struct Source src = {.first = state->lineno, .last = state->lineno};
            check_string(state, src, line);
        }
    }
}

// Spellchecks the comments in a Scheme file.
static void check_scheme(struct State *state, bool print) { assert(false); }

// Supported file types.
enum FileType {
    FT_NONE,
    FT_MARKDOWN,
    FT_SCHEME,
};

// Returns the file type based on the path's extension, or FT_NONE on failure.
static enum FileType detect_filetype(const char *path) {
    const char *dot = strrchr(path, '.');
    if (!(dot && dot[1] && dot[2])) {
        return FT_NONE;
    }
    if (strcmp(dot, ".md") == 0) {
        return FT_MARKDOWN;
    }
    if (strcmp(dot, ".ss") == 0) {
        return FT_SCHEME;
    }
    return FT_NONE;
}

// Spellchecks a single file. Returns 0 on success.
static int check(struct SpellChecker spell, bool print, const char *path) {
    enum FileType ft = detect_filetype(path);
    if (ft == FT_NONE) {
        fprintf(stderr, "%s: invalid file type\n", path);
        return 1;
    }
    struct State state;
    if (!init_state(&state, spell, path)) {
        return 1;
    }
    switch (ft) {
    case FT_MARKDOWN:
        check_markdown(&state, print);
        break;
    case FT_SCHEME:
        check_scheme(&state, print);
        break;
    case FT_NONE:
        assert(false);
    }
    close_state(&state);
    return state.status;
}

// Returns a list of non-English words to ignore.
static NSArray<NSString *> *ignored_words() { return @[]; }

int main(int argc, char **argv) {
    if (argc == 1) {
        fprintf(stderr, "\
usage: %s [-p | -d] FILE ...\n\
\n\
Spellchecks Markdown (.md) or Scheme (.ss) files using NSSpellChecker.\n\
Pass the -p flag to print the file as plain text without spellchecking,\n\
or the -d flag to show a diff from the input to the plain text.\n\
",
                argv[0]);
        return 1;
    }
    if (strcmp(argv[1], "-d") == 0) {
        if (argc != 3) {
            fprintf(stderr, "%s: -d only works with one file\n", argv[0]);
            return 1;
        }
        char cmd[256];
        // Note: git diff does not work with process substitution or /dev/stdin,
        // but it does work with "-" to mean stdin.
        snprintf(cmd, sizeof cmd, "%s -p '%s' | git diff --no-index '%s' -",
                 argv[0], argv[2], argv[2]);
        // Repeat /bin/bash for argv[0].
        if (execl("/bin/bash", "/bin/bash", "-c", cmd, NULL) == -1) {
            perror("execl");
            return 1;
        }
        assert(false);
    }
    bool print = false;
    int first_idx = 1;
    if (strcmp(argv[1], "-p") == 0) {
        print = true;
        first_idx++;
    }
    struct SpellChecker spell = {
        .checker = [NSSpellChecker sharedSpellChecker],
        .tag = [NSSpellChecker uniqueSpellDocumentTag],
    };
    spell.checker.language = @"en_US";
    spell.checker.automaticallyIdentifiesLanguages = NO;
    [spell.checker setIgnoredWords:ignored_words()
            inSpellDocumentWithTag:spell.tag];
    int status = 0;
    for (int i = first_idx; i < argc; i++) {
        status |= check(spell, print, argv[i]);
    }
    return status;
}