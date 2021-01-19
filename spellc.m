// Copyright 2021 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

@import AppKit.NSSpellChecker;
@import Foundation.NSOrthography;
@import Foundation.NSSpellServer;
@import Foundation.NSString;
@import Foundation.NSTextCheckingResult;

// Calculates the length of an array.
#define ARRAY_LEN(a) (sizeof a / sizeof a[0])

// We ignore spelling errors for these words.
static NSArray<NSString *> *ignored_words() {
    return @[
        @"conses",
        @"consing",
        @"desugars",
        @"Haumann",
        @"Kuna",
        @"luaposix",
        @"noninfringement",
        @"Paweł",
        @"skylighting",
        @"tabler",
        @"vnu",
        @"supertype",
        @"integerized",
        @"integerizing",
        @"indeterminates",
        @"mutexes",
        @"corecursion",
    ];
}

// We ignore grammatical errors whose descriptions contain any of these strings.
static const char *const IGNORED_GRAMMAR[] = {
    "Consider rewriting as ‘,\"’",
    "If this is an ordinary number, consider",
    "The first word of a sentence should be capitalized",
    "This may be a sentence fragment",
};
static const int N_IGNORED_GRAMMAR = ARRAY_LEN(IGNORED_GRAMMAR);

// Hash of a string.
typedef uint64_t Hash;

// We ignore errors about blocks hashing to any of these values. A block is an
// input to check_string: single lines for Markdown files and several lines
// combined together for Scheme files.
static const Hash IGNORED_BLOCKS[] = {
    0x07b97a28e478dc1c,  // "Declarative is what is, imperative is how to."
    0x14792bbe9437ea8a,  // ... "pattern variables and their matched values."
    0x431634165fef1130,  // "enumerate leaves" ...
    0xda2db6ed16d5c7e0,  // "So what you have is, at each level," ...
    0xee3105c7edb86378,  // "enumerate interval" ...
    0xf8756381627f7370,  // "If the body contains more than one" ...
    0xfddbd8df1da85f35,  // "A number CC is a fixed point of a function if CC."
};
static const int N_IGNORED_BLOCKS = ARRAY_LEN(IGNORED_BLOCKS);

// We ignore errors about phrases hashing to any of these values. A phrase is
// the full range for an error, usually a single word for a spelling error and a
// full sentence for a grammatical error.
static const Hash IGNORED_PHRASES[] = {
    0x04aa147212cc4e58,  // "Example: Counting change"
    0x06430e6c88dd925c,  // "Abstraction layer"
    0x075221d779908ada,  // "Why am I the same?"
    0x0ca4ef2b9ea03c84,  // "They are modeled to our permanent satisfaction" ...
    0x132f347f08bda2dc,  // "Computation provides a framework" ...
    0x1584480f2af38f50,  // "Complex number arithmetic"
    0x173a98e3fb50ede5,  // "The words of English are ambiguous:" ...
    0x27a06190ee1f6efb,  // ... "which you can advance through with F8."
    0x3902e0729e7d80e0,  // "operations on aggregates"
    0x43a647857079aa45,  // "The Fermat test"
    0x4d59aaf5ff9de024,  // "Environments, frames, and bindings"
    0x579c39a881d95041,  // "Avoiding mutable state"
    0x5d57a1fc209add8b,  // "Environment model"
    0x6c59779781ad610f,  // "Normal-order evaluation"
    0x6cc5a69e86ac6fbe,  // "An example of normal-order procedure application:"
    0x76e301bea89320d5,  // ... "The square root of" ...
    0x83a84bda5d616929,  // "Editor support"
    0xa89ed3d7fbe46522,  // "LaTeX math: inline CC, display CC."
    0xa9a9830f7b756ea3,  // "Website source:"
    0xacf3a3e92669aa36,  // "Before submitting a PR, run CC."
    0xaeb7cd203a53b835,  // ... "taking up 3 MB (!)."
    0xbeb3b8947d1a64d1,  // "The LHS is what you compare your expression to."
    0xbfe161bde0d47a33,  // "GitHub mark"
    0xc727ece995264a22,  // "Not so with the Fermat test."
    0xc7daee857a0dc5d1,  // "What's in your hands, I think and hope" ...
    0xd38917ad00055e36,  // ... "are the things that were erected" ...
    0xd72d5f50fe3ba97c,  // "Fibonacci sequence"
    0xdc7dc0140942f368,  // "Each rule is of the form CC."
    0xf1814ee4ad4769ea,  // "SICP derivative work"
    0xf1c38aa17d8e84df,  // "Simplification process"
    0xfaa92fcf419fbcf0,  // "For more information, see the website."
    0xfd064f487f8dc8bd,  // "Original work"
};
static const int N_IGNORED_PHRASES = ARRAY_LEN(IGNORED_PHRASES);

// We ignore errors about subphrases hashing to any of these values. A subphrase
// is a part of a phrase that a grammatical error singles out.
static const Hash IGNORED_SUBPHRASES[] = {
    0x000000000b87784c,  // "---"
};
static const int N_IGNORED_SUBPHRASES = ARRAY_LEN(IGNORED_SUBPHRASES);

// Returns true if the given array of hashes is sorted.
static bool hashes_are_sorted(const Hash *array, int n) {
    for (int i = 1; i < n; i++) {
        if (array[i - 1] >= array[i]) {
            return false;
        }
    }
    return true;
}

// The djb2 hashing algorithm.
static Hash hash_string(const char *s, int n) {
    Hash h = 5381;
    for (int i = 0; i < n; i++) {
        h = ((h << 5) + h) + s[i];
    }
    return h;
}

// Hashes the given range in the string.
static Hash hash_range(const char *str, NSRange range) {
    return hash_string(str + range.location, range.length);
}

// Returns true if array (length n) contains hash h.
static bool contains_hash(const Hash *array, int n, Hash hash) {
    // Binary search.
    int lo = 0;
    int hi = n;
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        Hash h = array[mid];
        if (hash == h) return true;
        if (hash < h)
            hi = mid;
        else
            lo = mid + 1;
    }
    return false;
}

// Returns true if we should skip all errors for the given block.
static bool should_skip_block(const char *str, NSRange range) {
    return contains_hash(IGNORED_BLOCKS, N_IGNORED_BLOCKS,
                         hash_range(str, range));
}

// Returns true if we should skip all errors for the given phrase.
static bool should_skip_phrase(const char *str, NSRange range) {
    return contains_hash(IGNORED_PHRASES, N_IGNORED_PHRASES,
                         hash_range(str, range));
}

// Given a range r2 relative to r1, returns r2 as an absolute range.
static NSRange add_ranges(NSRange r1, NSRange r2) {
    r2.location += r1.location;
    return r2;
}

// Returns true if we should skip a particular grammatical error about the given
// subphrase (subrange_value) within a phrase (range).
static bool should_skip_subphrase(const char *str, NSRange range,
                                  NSValue *subrange_value,
                                  NSString *description) {
    const char *c_description = description.UTF8String;
    for (int i = 0; i < N_IGNORED_GRAMMAR; i++) {
        if (strstr(c_description, IGNORED_GRAMMAR[i]) != NULL) {
            return true;
        }
    }
    if (!subrange_value) {
        return false;
    }
    NSRange subrange = add_ranges(range, subrange_value.rangeValue);
    return contains_hash(IGNORED_SUBPHRASES, N_IGNORED_SUBPHRASES,
                         hash_range(str, subrange));
}

// Wrapper around NSSpellChecker that stores the document tag.
struct SpellChecker {
    NSSpellChecker *checker;
    int tag;
};

// Options to the program.
struct Options {
    // Instead of spellchecking, print the result of converting Markdown to
    // plain text (the spellchecking input).
    bool print_plain;
    // Print hashes after spellchecker errors. These can be used to modify this
    // program to ignore those specific errors.
    bool print_hashes;
};

// State for spell checking.
struct State {
    struct SpellChecker spell;
    struct Options options;
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
                       struct Options options, const char *path) {
    FILE *file = fopen(path, "r");
    if (!file) {
        perror(path);
        return false;
    }
    state->spell = spell;
    state->options = options;
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
static void check_block(struct State *state, struct Source src,
                        const char *str) {
    NSString *ns_str = @(str);
    NSRange full_range = NSMakeRange(0, ns_str.length);
    if (should_skip_block(str, full_range)) {
        return;
    }
    NSArray<NSTextCheckingResult *> *results =
        [state->spell.checker checkString:ns_str
                                    range:full_range
                                    types:NSTextCheckingTypeSpelling
                                          | NSTextCheckingTypeGrammar
                                  options:NULL
                   inSpellDocumentWithTag:state->spell.tag
                              orthography:NULL
                                wordCount:NULL];
    for (NSTextCheckingResult *result in results) {
        NSRange range = result.range;
        if (should_skip_phrase(str, range)) {
            continue;
        }
        NSString *replacement = NULL;
        NSArray<NSDictionary<NSString *, id> *> *grammarDetails = NULL;
        uint64_t grammarSkip = 0;
        switch (result.resultType) {
        case NSTextCheckingTypeSpelling:
            replacement = [state->spell.checker
                correctionForWordRange:range
                              inString:ns_str
                              language:state->spell.checker.language
                inSpellDocumentWithTag:state->spell.tag];
            break;
        case NSTextCheckingTypeGrammar:
            grammarDetails = result.grammarDetails;
            assert(grammarDetails.count <= 64);
            int index = -1, skips = 0;
            for (NSDictionary<NSString *, id> *detail in grammarDetails) {
                index++;
                NSValue *subrange = detail[NSGrammarRange];
                NSString *description = detail[NSGrammarUserDescription];
                if (should_skip_subphrase(str, range, subrange, description)) {
                    skips++;
                    grammarSkip |= UINT64_C(1) << index;
                }
            }
            if (skips == (int)grammarDetails.count) {
                continue;
            }
            break;
        default:
            assert(false);
        }
        state->status = 1;
        fputs(state->path, stdout);
        if (src.first == src.last) {
            printf(":%d: ", src.first);
        } else {
            printf(":%d-%d: ", src.first, src.last);
        }
        printf("“%.*s”", (int)range.length, str + range.location);
        if (replacement) {
            printf(" (did you mean “%s”?)", replacement.UTF8String);
        }
        if (state->options.print_hashes) {
            printf(" [block = 0x%016llx]", hash_range(str, full_range));
        }
        if (grammarDetails) {
            if (state->options.print_hashes) {
                printf(" [phrase = 0x%016llx]", hash_range(str, range));
            }
            int index = -1;
            for (NSDictionary<NSString *, id> *detail in grammarDetails) {
                index++;
                if ((grammarSkip & (UINT64_C(1) << index)) != 0) {
                    continue;
                }
                printf("\n    ");
                NSValue *value = detail[NSGrammarRange];
                NSRange subrange;
                if (value) {
                    subrange = add_ranges(range, value.rangeValue);
                    printf("“%.*s”", (int)subrange.length,
                           str + subrange.location);
                }
                NSString *description = detail[NSGrammarUserDescription];
                if (value) fputs(": ", stdout);
                fputs(description.UTF8String, stdout);
                if (value && state->options.print_hashes) {
                    printf(" [subphrase = 0x%016llx]",
                           hash_range(str, subrange));
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

// Returns true if s starts with the given prefix.
static bool startswith(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

// Returns the new mode for the given line of Markdown.
static enum Mode next_mode(enum Mode old, const char *line) {
    switch (old) {
    case M_NORMAL:
    case M_CODE_BLOCK_END:
    case M_DISPLAY_MATH_END:
    case M_EXERCISE_DIV_END:
    case M_HTML_PRE_END:
        if (startswith(line, "```")) {
            return M_CODE_BLOCK_START;
        };
        if (strcmp(line, "$$\n") == 0) {
            return M_DISPLAY_MATH_START;
        }
        if (strcmp(line, "::: exercises\n") == 0) {
            return M_EXERCISE_DIV_START;
        }
        if (startswith(line, "<pre>")) {
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
    // Skip link definitions, display math, and divs.
    if (s[0] == '[' || (s[0] == '$' && s[1] == '$') || startswith(s, ":::")) {
        return NULL;
    }
    if (*s == '#') {
        // Headings.
        while (*s == '#') s++;
        while (*s == ' ') s++;
    } else if (*s == '>') {
        // Block quotes.
        s++;
        while (*s == ' ') s++;
    } else {
        while (*s == ' ') s++;
        // Lists.
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
        // Escaped backticks, dollar signs, etc.
        case '\\':
            p++;
            if (!*p) goto end;
            *s++ = *p++;
            break;
        // Inline code/math.
        case '`':
        case '$':
            delim = *p;
            p++;
            while (*p && *p != delim) p++;
            if (!*p) goto end;
            p++;
            *s++ = 'C';
            *s++ = 'C';
            if (p[0] == 't' && p[1] == 'h' && p[3] == ' ') {
                s[-2] = '1';
                s[-1] = 's';
                *s++ = 't';
                p += 2;
            }
            break;
        // HTML tags and URLs.
        case '<':
            p++;
            if (startswith(p, "http")) {
                *s++ = 'U';
                *s++ = 'R';
                *s++ = 'L';
            }
            while (*p && *p != '>') p++;
            if (!*p) goto end;
            p++;
            break;
        // Links and citations.
        case '[':
            if (p[1] == '@') {
                while (*p && *p != ']') p++;
                if (!*p) goto end;
                p++;
                break;
            }
            // fallthrough
        // Emphasis.
        case '_':
        case '*':
            p++;
            break;
        // Link targets.
        case ']':
            if (p[1] == ' ') {
                *s++ = *p++;
                break;
            }
            p++;
            if (*p == '[')
                delim = ']';
            else if (*p == '(')
                delim = ')';
            else if (*p == '{')
                delim = '}';
            else
                assert(false);
            while (*p && *p != delim) p++;
            if (!*p) goto end;
            p++;
            break;
        // HTML entities.
        case '&':
            if (p > start && (p[-1] == ' ' || p[-1] == '`' || p[-1] == 'r')
                && islower(p[1])) {
                while (*p && *p != ';') p++;
                if (!*p) goto end;
                p++;
                if (startswith(p, "ed ")) {
                    p += 2;
                }
            } else {
                *s++ = *p++;
            }
            break;
        // Lambda (λ).
        case '\xce':
            if (p[1] == '\xbb') {
                *s++ = 'L';
                p += 2;
            } else {
                *s++ = *p++;
            }
            break;
        // Filenames.
        case '.':
            if (p > start && isalnum(p[-1]) && islower(p[1])) {
                while (s > start && *s != ' ') s--;
                if (*s == ' ') s++;
                *s++ = 'F';
                *s++ = 'I';
                *s++ = 'L';
                while (*p && *p != ' ' && *p != '\n') p++;
                if (!*p) goto end;
            } else {
                *s++ = *p++;
            }
            break;
        // Directories.
        case '/':
            if (p > start && isalnum(p[-1])
                && (p[1] == ' ' || p[1] == '\n' || p[1] == ']')) {
                while (s > start && *s != ' ') s--;
                if (*s == ' ') s++;
                *s++ = 'D';
                *s++ = 'I';
                *s++ = 'R';
                while (*p && *p != ' ' && *p != '\n' && *p != ']') p++;
                if (!*p) goto end;
            } else {
                *s++ = *p++;
            }
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
static void check_markdown(struct State *state) {
    enum Mode mode = M_NORMAL;
    while (scan(state)) {
        mode = next_mode(mode, state->line);
        if (mode != M_NORMAL) {
            continue;
        }
        char *plain = strip_markdown(state->line);
        if (!plain) continue;
        if (state->options.print_plain) {
            fputs(plain, stdout);
        } else {
            struct Source src = {.first = state->lineno, .last = state->lineno};
            check_block(state, src, plain);
        }
    }
}

// Spellchecks the comments in a Scheme file.
static void check_scheme(struct State *state) { assert(false); }

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
static int check(struct SpellChecker spell, struct Options options,
                 const char *path) {
    enum FileType ft = detect_filetype(path);
    if (ft == FT_NONE) {
        fprintf(stderr, "%s: invalid file type\n", path);
        return 1;
    }
    struct State state;
    if (!init_state(&state, spell, options, path)) {
        return 1;
    }
    switch (ft) {
    case FT_MARKDOWN:
        check_markdown(&state);
        break;
    case FT_SCHEME:
        check_scheme(&state);
        break;
    case FT_NONE:
        assert(false);
    }
    close_state(&state);
    return state.status;
}

int main(int argc, char **argv) {
    assert(hashes_are_sorted(IGNORED_BLOCKS, N_IGNORED_BLOCKS));
    assert(hashes_are_sorted(IGNORED_PHRASES, N_IGNORED_PHRASES));
    assert(hashes_are_sorted(IGNORED_SUBPHRASES, N_IGNORED_SUBPHRASES));
    if (argc == 1) {
        fprintf(stderr, "\
usage: %s [-p | -d | -x] FILE ...\n\
\n\
Spellchecks Markdown (.md) or Scheme (.ss) files using NSSpellChecker.\n\
Pass the -p flag to print the file as plain text without spellchecking,\n\
or the -d flag to show a diff from the input to the plain text.\n\
Pass the -x flag to print hashes that can be used for ignoring errors.\n\
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
    struct Options options = {.print_plain = false, .print_hashes = false};
    int idx = 1;
    if (strcmp(argv[1], "-p") == 0) {
        options.print_plain = true;
        idx++;
    } else if (strcmp(argv[1], "-x") == 0) {
        options.print_hashes = true;
        idx++;
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
    for (; idx < argc; idx++) {
        status |= check(spell, options, argv[idx]);
    }
    return status;
}
