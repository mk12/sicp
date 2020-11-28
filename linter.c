// Copyright 2020 Mitchell Kember. Subject to the MIT License.

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Maximum number of columns allowed by the style guide.
#define MAX_COLUMNS 80

// Skip alignment checks on lines ending with this comment.
static const char NO_ALIGN_COMMENT[] = "; NOALIGN\n";

// Bit flags specifying indentation rules for an operator. The indentation of a
// line is determined by the last unclosed paren's operator.
enum IndentRules {
    // By default, operands must line up with the operator:
    //
    //     (operator
    //      operand1
    //      operand2
    //      ...)
    //
    // Or with the first operand, if it's on the same line as the operator:
    //
    //     (operator operand1
    //               operand2
    //               ...)
    //
    // Multiple operands per line are also allowed, though this should only be
    // used in special cases (e.g. import lists, format arguments):
    //
    //     (operator operand1 operand2
    //               operand3 operand4)
    //
    IR_DEFAULT = 0x00,

    // When the special bit is set, the body (if present) must be indented by
    // two spaces from the open paren:
    //
    //     (operator operand
    //       body
    //       ...)
    //
    // Operands, if any, must be on the same line as the operator.
    IR_SPECIAL = 0x01,

    // When the wrapper bit is set, the contents must be unindented:
    //
    //     (operator operand1
    //               operand2
    //     contents
    //     ...
    //     ) ; operator
    //
    // Contents come after the optional operands and (if the special bit is set)
    // the optional body. Wrapper forms can only occur at the top level or
    // nested inside other wrapper forms.
    IR_WRAPPER = 0x02,

    // When the uniform bit is set in conjunction with the special bit, the body
    // must still be indented by two spaces:
    //
    //     (operator
    //       body
    //       ...)
    //
    // But if there are operands on the same line as the operator, then
    // indentation follows the default case:
    //
    //     (operator body
    //               ...)
    //
    // In other words, all operands are uniform and they comprise the body,
    // rather than being distinct from it.
    IR_UNIFORM = 0x04,
};

// Map from operand names to indentation rules.
static const struct {
    const char *name;
    enum IndentRules rules;
} INDENT_RULES[] = {
    // Exceptional cases.
    { .name = "SICP", .rules = IR_WRAPPER },
    { .name = "begin", .rules = IR_SPECIAL | IR_UNIFORM },
    { .name = "cond", .rules = IR_SPECIAL | IR_UNIFORM },
    { .name = "library", .rules = IR_SPECIAL | IR_WRAPPER },

    // Special forms.
    { .name = "Chapter", .rules = IR_SPECIAL },
    { .name = "Exercise", .rules = IR_SPECIAL },
    { .name = "Section", .rules = IR_SPECIAL },
    { .name = "case", .rules = IR_SPECIAL },
    { .name = "define", .rules = IR_SPECIAL },
    { .name = "define-record-type", .rules = IR_SPECIAL },
    { .name = "define-syntax", .rules = IR_SPECIAL },
    { .name = "lambda", .rules = IR_SPECIAL },
    { .name = "let", .rules = IR_SPECIAL },
    { .name = "let*", .rules = IR_SPECIAL },
    { .name = "let-syntax", .rules = IR_SPECIAL },
    { .name = "let-values", .rules = IR_SPECIAL },
    { .name = "letrec", .rules = IR_SPECIAL },
    { .name = "parameterize", .rules = IR_SPECIAL },
    { .name = "syntax-case", .rules = IR_SPECIAL },
    { .name = "syntax-rules", .rules = IR_SPECIAL },
    { .name = "unless", .rules = IR_SPECIAL },
    { .name = "when", .rules = IR_SPECIAL },
};

// Looks up the indentation rules for the given operator.
static enum IndentRules lookup_indent_rules(const char *s, int len) {
    assert(len > 0);
    const int array_len = sizeof INDENT_RULES / sizeof INDENT_RULES[0];
    for (int i = 0; i < array_len; i++) {
        if (strncmp(s, INDENT_RULES[i].name, len) == 0) {
            return INDENT_RULES[i].rules;
        }
    }
    return IR_DEFAULT;
}

// Linter state for a single file.
struct State {
    // File currently being linted.
    const char *filename;
    // One-based line number.
    int lineno;
    // Cumulative status, set to 1 if there are any errors.
    int status;
    // Number of blank lines in a row seen.
    int prev_blanks;
    // True if we are inside a string.
    bool in_string;
    // Number of wrapper forms encountered.
    int num_wrappers;
    // If the last open paren was quoted, its alignment column (allowed as an
    // alternative to the 1st-operand/2-space alignment). Otherwise, -1.
    int quoted_align;
    // Number of unclosed parens that remain.
    int depth;
    // Stack of alignments, valid from stack[0] to stack[depth] inclusive. A new
    // line is expected to be indented by stack[depth] spaces. The bottom,
    // stack[0], is always 0 because top-level forms should not be indented.
    int stack[64];
};

// Emits a failure message for the current line.
static void fail(struct State *state, const char *msg) {
    state->status = 1;
    printf("%s:%d: %s\n", state->filename, state->lineno, msg);
}

// Lints the given line, which must be nonempty and end with a newline.
static void lint_line(struct State *state, const char *line, int line_len) {
    assert(line_len > 0);
    assert(line[line_len-1] == '\n');
    assert(state->depth >= 0);
    if (line_len == 1) {
        if (!state->in_string) {
            if (state->prev_blanks == 0 && state->depth > state->num_wrappers) {
                fail(state, "blank line in definition");
            } else if (state->prev_blanks == 1) {
                fail(state, "multiple blank lines");
            }
        }
        state->prev_blanks++;
        return;
    }
    state->prev_blanks = 0;
    if (line_len - 1 > MAX_COLUMNS) {
        fail(state, "line too long");
    }
    if (line[line_len-2] == ' ') {
        fail(state, "trailing whitespace");
    }
    if (line[0] == ';') {
        int i;
        for (i = 1; line[i] == ';'; i++);
        if (i > 3) {
            fail(state, "too many semicolons");
        } else if (i == 3 && state->lineno != 1) {
            fail(state, "';;;' only allowed on first line copyright");
        }
        if (line[i] != '\n' && line[i] != ' ') {
            fail(state, "missing space after ';'");
        }
        return;
    }
    const size_t na_len = sizeof NO_ALIGN_COMMENT - 1;
    const bool no_align = (size_t)line_len >= na_len
        && strncmp(line + line_len - na_len, NO_ALIGN_COMMENT, na_len) == 0;
    char prev = 0;            // previous character
    bool escaped = false;     // last character was unescaped backslash
    bool two_spaces = false;  // saw two spaces in a row
    enum { INDENT, NORMAL, OPERATOR, STRING, COMMENT, COMMENT_SPACE } mode =
        state->in_string ? STRING : INDENT;
    for (int i = 0; i < line_len; i++) {
        char c = line[i];
        if (c == '\t') {
            fail(state, "illegal character '\\t'");
        }
        switch (mode) {
        case INDENT:
            if (c == ' ') {
                break;
            }
            if (!no_align && i != state->stack[state->depth]) {
                if (i == 0 && state->depth == state->num_wrappers) {
                    // Returning to zero indentation after wrapper opening.
                    state->stack[state->depth] = 0;
                } else if (i == state->quoted_align) {
                    // Quoted form is data, not code.
                    state->stack[state->depth] = state->quoted_align;
                    state->quoted_align = -1;
                } else {
                    fail(state, "incorrect indentation");
                }
            }
            mode = NORMAL;
            // fallthrough
        case NORMAL:
        case OPERATOR:
            if (two_spaces && c != ' ' && c != ';') {
                two_spaces = false;
                fail(state, "unexpected two spaces in a row");
            }
            switch (c) {
            case '"':
                mode = STRING;
                state->in_string = true;
                break;          
            case ';':
                mode = COMMENT;
                if (prev != ' ') {
                    fail(state, "expected space before ';'");
                }
                break;
            case '(':
            case '[':
                mode = OPERATOR;
                state->stack[++state->depth] = i + 1;
                if (i > 0 && prev != ' ' && prev != '(' && prev != '['
                        && prev != '\'' && prev != '`' && prev != ','
                        && prev != '#') {
                    fail(state, "expected space before '('");        
                }
                if (prev == '\'' || (state->quoted_align != -1
                        && (prev == '(' || prev == '['))) {
                    state->quoted_align = i + 1;
                } else {
                    state->quoted_align = -1;
                }
                break;
            case ')':
            case ']':
                mode = NORMAL;
                if (i != 0 && state->depth == state->num_wrappers) {
                    fail(state, "expected ')' at start of line for wrapper");
                }
                if (prev == ' ') {
                    fail(state, "unexpected space before ')'");
                }
                state->depth--;
                break;
            case ' ':
                if (prev == ' ') {
                    two_spaces = true;
                }
                // fallthrough
            case '\n':
                if (mode == OPERATOR) {
                    mode = NORMAL;
                    int start = state->stack[state->depth];
                    enum IndentRules rules = 
                        lookup_indent_rules(line + start, i - start);
                    if ((rules & IR_WRAPPER) != 0) {
                        state->num_wrappers++;
                    }
                    if (c == ' ') {
                        if ((rules & IR_SPECIAL) != 0
                                && (rules & IR_UNIFORM) == 0) {
                            state->stack[state->depth]++;
                        } else {
                            state->stack[state->depth] = i + 1;
                        }
                    } else {
                        if ((rules & IR_SPECIAL) != 0) {
                            state->stack[state->depth]++;
                        }
                    }
                }
                break;
            }
            break;
        case STRING:
            if (!escaped && c == '"') {
                mode = NORMAL;
                state->in_string = false;
            }
            break;
        case COMMENT:
            if (c == ';') {
                mode = COMMENT_SPACE;
                break;
            }
            // fallthrough
        case COMMENT_SPACE:
            if (c != ' ') {
                fail(state, "expected space after ';'");
            }
            return;
        }
        prev = c;
        escaped = c == '\\' ? !escaped : false;
    }
}

// Reads and lints the given file.
static int lint(const char* filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror(filename);
        return 1;
    }
    struct State state = {
        .filename = filename,
        .lineno = 1,
        .status = 0,
        .prev_blanks = 0,
        .in_string = false,
        .num_wrappers = 0,
        .quoted_align = 0,
        .depth = 0,
        .stack = {0},
    };
    char *line = NULL;
    size_t cap = 0;
    ssize_t len;
    while ((len = getline(&line, &cap, fp)) != -1) {
        lint_line(&state, line, (int)len);
        state.lineno++;
    }
    fclose(fp);
    return state.status;
}

int main(int argc, char **argv) {
    if (argc == 1) {
        printf("usage: %s FILE ...\n", argv[0]);
        return 0;
    }
    int status = 0;
    for (int i = 1; i < argc; i++) {
        status |= lint(argv[i]);
    }
    return status;
}
