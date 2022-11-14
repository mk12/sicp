// Copyright 2022 Mitchell Kember. Subject to the MIT License.

// This Lua C library provides Scheme syntax highlighting. It is tailored to the
// SICP Study project use case and does not adhere to any spec.
//
// Example usage from Lua:
//
//     schemehl = require("schemehl") -- loads schemehl.so from LUA_CPATH
//
//     print(schemehl.highlight("(define x 1)"))
//
//     print(schemehl.highlight("(paste (:1.2 foo) (?3.4 bar))", {
//         sicp_id_link = function(id)
//             -- id is ":1.2" or "?3.4"
//             return "path/file.html#fragment"
//         end
//     }))
//

#include <assert.h>
#include <ctype.h>
#include <lua5.4/lauxlib.h>
#include <lua5.4/lua.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// If enabled, the highlighter will avoid closing <span> tags before whitespace
// only to reopen them again with the same class as before. One reason to
// disable this would be if any of the CSS styles set a background color.
#define MINIMIZE_TAGS 1

// Enable this to print input code to stderr.
#define DEBUG_INPUT 0

// Enable this to print tokens to stderr.
#define DEBUG_TOKENS 0

// Enable this to print the quote stack to stderr.
#define DEBUG_QUOTES 0

// SICP assertion operators to highlight normally.
static const char *const NORMAL_ASSERTIONS[] = {
    "=$>",
    "=>",
    "=?>",
    "~>",
};

// SICP assertion operators to highlight as errors.
static const char *const ERROR_ASSERTIONS[] = {
    "=!>",
    "=>...",
};

// Special forms to highlight.
static const char *const SPECIAL_FORMS[] = {
    "and",
    "begin",
    "capture-output",
    "case",
    "catch",
    "cond",
    "cons-stream",
    "define",
    "define-record-type",
    "define-syntax",
    "delay",
    "else",
    "export",
    "hide-output",
    "if",
    "import",
    "lambda",
    "let",
    "let*",
    "let*-values",
    "let-syntax",
    "let-values",
    "letrec",
    "letrec*",
    "letrec-syntax",
    "library",
    "or",
    "paste",
    "quasiquote",
    "quasisyntax",
    "quote",
    "set!",
    "syntax",
    "syntax-case",
    "syntax-rules",
    "unquote",
    "unquote-splicing",
    "unsyntax",
    "unsyntax-splicing",
    "with-eval",
};

// Builtin functions to highlight.
static const char *const FUNCTIONS[] = {
    "!",
    "*",
    "+",
    "-",
    "-1+",
    "/",
    "1+",
    "<",
    "<=",
    "=",
    ">",
    ">=",
    "abs",
    "acos",
    "append",
    "apply",
    "asin",
    "assoc",
    "assp",
    "assq",
    "assv",
    "atan",
    "boolean=?",
    "boolean?",
    "bound-identifier=?",
    "caaaar",
    "caaadr",
    "caaar",
    "caadar",
    "caaddr",
    "caadr",
    "caar",
    "cadaar",
    "cadadr",
    "cadar",
    "caddar",
    "cadddr",
    "caddr",
    "cadr",
    "call-with-current-continuation",
    "call-with-input-file",
    "call-with-output-file",
    "call-with-port",
    "call-with-string-output-port",
    "call-with-values",
    "call/cc",
    "car",
    "cdaaar",
    "cdaadr",
    "cdaar",
    "cdadar",
    "cdaddr",
    "cdadr",
    "cdar",
    "cddaar",
    "cddadr",
    "cddar",
    "cdddar",
    "cddddr",
    "cdddr",
    "cddr",
    "cdr",
    "ceiling",
    "char->integer",
    "char-alphabetic?",
    "char-ci<=?",
    "char-ci<?",
    "char-ci=?",
    "char-ci>=?",
    "char-ci>?",
    "char-downcase",
    "char-foldcase",
    "char-general-category",
    "char-lower-case?",
    "char-numeric?",
    "char-ready?",
    "char-title-case?",
    "char-titlecase",
    "char-upcase",
    "char-upper-case?",
    "char-whitespace?",
    "char<=?",
    "char<?",
    "char<?c",
    "char=?",
    "char>=?",
    "char>?",
    "char?",
    "complex?",
    "condition",
    "condition-accessor",
    "condition-irritants",
    "condition-message",
    "condition-predicate",
    "condition-who",
    "condition?",
    "cons",
    "cos",
    "current-error-port",
    "current-input-port",
    "current-output-port",
    "datum->syntax",
    "denominator",
    "display",
    "div-and-mod",
    // Omit "div" here since it looks bad in the generic operation sections
    // where only it would be highlighted but not "add", "sub", and "mul".
    "div0",
    "div0-and-mod0",
    "eq?",
    "equal-hash",
    "equal?",
    "eqv?",
    "error",
    "error?",
    "eval",
    "even?",
    "exact",
    "exact->inexact",
    "exact-integer-sqrt",
    "exact?",
    // Omit "exp" here since we use it for expression parameters.
    "expt",
    "filter",
    "find",
    "finite?",
    "fixnum-width",
    "fixnum?",
    "floor",
    "for-each",
    "force",
    "free-identifier=?",
    "fx*",
    "fx*/carry",
    "fx+",
    "fx+/carry",
    "fx-",
    "fx-/carry",
    "fx<=?",
    "fx<?",
    "fx=?",
    "fx>=?",
    "fx>?",
    "fxand",
    "fxarithmetic-shift",
    "fxarithmetic-shift-left",
    "fxarithmetic-shift-right",
    "fxbit-count",
    "fxbit-field",
    "fxbit-set?",
    "fxcopy-bit",
    "fxcopy-bit-field",
    "fxdiv",
    "fxdiv-and-mod",
    "fxdiv0",
    "fxdiv0-and-mod0",
    "fxeven?",
    "fxfirst-bit-set",
    "fxif",
    "fxior",
    "fxlength",
    "fxmax",
    "fxmin",
    "fxmod",
    "fxmod0",
    "fxnegative?",
    "fxnot",
    "fxodd?",
    "fxpositive?",
    "fxreverse-bit-field",
    "fxrotate-bit-field",
    "fxxor",
    "fxzero?",
    "gcd",
    "hashtable-clear!",
    "hashtable-contains?",
    "hashtable-copy",
    "hashtable-delete!",
    "hashtable-entries",
    "hashtable-equivalence-function",
    "hashtable-hash-function",
    "hashtable-keys",
    "hashtable-mutable?",
    "hashtable-ref",
    "hashtable-set!",
    "hashtable-size",
    "hashtable-update!",
    "hashtable?",
    "identifier-syntax",
    "identifier?",
    "inexact",
    "inexact->exact",
    "inexact?",
    "infinite?",
    "input-port?",
    "integer->char",
    "integer-valued?",
    "integer?",
    "interaction-environment",
    "irritants-condition?",
    "lcm",
    "least-fixnum",
    "length",
    "lexical-violation?",
    "list",
    "list->string",
    "list->vector",
    "list-ref",
    "list-sort",
    "list-tail",
    "list?",
    "log",
    "make-assertion-violation",
    "make-eq-hashtable",
    "make-eqv-hashtable",
    "make-error",
    "make-hashtable",
    "make-syntax-violation",
    "map",
    "max",
    "member",
    "memp",
    "memq",
    "memv",
    "message-condition?",
    "min",
    "mod",
    "mod0",
    "modulo",
    "nan?",
    "negative?",
    "newline",
    "not",
    "null?",
    "number->string",
    "number?",
    "numerator",
    "odd?",
    "pair?",
    "partition",
    "peek-char",
    "port?",
    "positive?",
    "procedure?",
    "quotient",
    "rational-valued?",
    "rational?",
    "rationalize",
    "read",
    "read-char",
    "real-valued?",
    "real?",
    "remainder",
    "remove",
    "remp",
    "remq",
    "remv",
    "reverse",
    "round",
    "set-car!",
    "set-cdr!",
    "sin",
    "sqrt",
    "string",
    "string->list",
    "string->number",
    "string->symbol",
    "string-append",
    "string-ci-hash",
    "string-ci<=?",
    "string-ci<?",
    "string-ci=?",
    "string-ci>=?",
    "string-ci>?",
    "string-copy",
    "string-count",
    "string-downcase",
    "string-fill!",
    "string-foldcase",
    "string-hash",
    "string-length",
    "string-ref",
    "string-set!",
    "string-titlecase",
    "string-upcase",
    "string<=?",
    "string<?",
    "string=?",
    "string>=?",
    "string>?",
    "string?",
    "substring",
    "symbol->string",
    "symbol-hash",
    "symbol=?",
    "symbol?",
    "syntax->datum",
    "syntax-violation",
    "syntax-violation?",
    "tan",
    "truncate",
    "values",
    "vector",
    "vector-fill!",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "vector-sort",
    "vector-sort!",
    "vector?",
    "violation?",
    "warning?",
    "who-condition?",
    "with-exception-handler",
    "with-syntax",
    "write",
    "write-char",
    "zero?",
};

// Gets the length of an array.
#define ARRAY_LEN(a) (sizeof(a) / sizeof(a)[0])

// Asserts that an array is sorted lexicographically.
#define ASSERT_SORTED(a) (assert_sorted((a), ARRAY_LEN(a)))
static void assert_sorted(const char *const *array, size_t len) {
    for (size_t i = 1; i < len; i++) {
        if (strcmp(array[i - 1], array[i]) >= 0) {
            fprintf(stderr, "not sorted: \"%s\" >= \"%s\"\n", array[i - 1],
                    array[i]);
            abort();
        }
    }
}

// A pointer-and-length string. Not necessarily null terminated.
struct Span {
    const char *data;
    size_t len;
};

// Creates a span from a C string.
#define SPAN(s) ((struct Span){s, strlen(s)})

// Returns a subspan from i (inclusive) to j (exclusive).
#define SUBSPAN(span, i, j) ((struct Span){(span).data + i, j - i})

// Returns address of the end of a span.
#define SPAN_END(span) ((span).data + (span).len)

// Gets the minimum of two numbers.
#define MIN(x, y) ((x) < (y) ? (x) : (y))

// Like strcmp but for spans.
static int span_cmp(struct Span s1, struct Span s2) {
    int cmp = strncmp(s1.data, s2.data, MIN(s1.len, s2.len));
    if (cmp != 0) {
        return cmp;
    }
    return s1.len - s2.len;
}

// Returns true if the spans are equal. More efficient than using span_cmp.
static bool span_eq(struct Span s1, struct Span s2) {
    if (s1.len != s2.len) {
        return false;
    }
    return span_cmp(s1, s2) == 0;
}

// Returns true if needle occurs in haystack. The array must be sorted.
#define IN_ARRAY(n, h) (in_array((n), (h), ARRAY_LEN(h)))
static bool in_array(struct Span needle, const char *const *haystack,
                     size_t len) {
    // It's important that these are signed, otherwise we overflow.
    int i = 0;
    int j = len - 1;
    while (i <= j) {
        int m = (i + j) / 2;
        const char *str = haystack[m];
        int cmp = span_cmp(needle, SPAN(str));
        if (cmp < 0) {
            j = m - 1;
        } else if (cmp > 0) {
            i = m + 1;
        } else {
            return true;
        }
    }
    return false;
}

// A growable buffer.
struct Buffer {
    char *data;
    size_t len;
    size_t cap;
};

// Initializes the buffer with the given capacity.
static void buf_init(struct Buffer *buf, int initial_cap) {
    buf->data = malloc(initial_cap);
    buf->len = 0;
    buf->cap = initial_cap;
}

// Reallocates the buffer if necessary to have the additional space needed.
static void buf_ensure_additional(struct Buffer *buf, size_t additional) {
    size_t min_cap = buf->len + additional;
    if (buf->cap >= min_cap) {
        return;
    }
    while (buf->cap < min_cap) {
        buf->cap *= 2;
    }
    buf->data = realloc(buf->data, buf->cap);
}

// Writes to the end of the buffer.
static void buf_write(struct Buffer *buf, struct Span span) {
    buf_ensure_additional(buf, span.len);
    memcpy(buf->data + buf->len, span.data, span.len);
    buf->len += span.len;
}

// Wrapper around a buffer that writes <span> tags for highlighting and escapes
// special characters with HTML entities.
struct Highlighter {
    // Destination buffer.
    struct Buffer *buf;
    // Current CSS class.
    const char *class;
    // Whitespace span not yet written to the buffer.
    struct Span pending_ws;
};

// Initializes a highlighter with a destination buffer.
static void hl_init(struct Highlighter *hl, struct Buffer *buf) {
    hl->buf = buf;
    hl->class = NULL;
    hl->pending_ws.data = NULL;
}

// Flushes any pending </span> to the buffer.
static void hl_flush_tag(struct Highlighter *hl) {
    if (!hl->class) {
        return;
    }
    buf_write(hl->buf, SPAN("</span>"));
    hl->class = NULL;
}

// Flushes any pending whitespace to the buffer.
static void hl_flush_ws(struct Highlighter *hl) {
    if (!hl->pending_ws.data) {
        return;
    }
    buf_write(hl->buf, hl->pending_ws);
    hl->pending_ws.data = NULL;
}

// Flushes any pending writes to the buffer. Must be called at the end.
static void hl_flush(struct Highlighter *hl) {
    hl_flush_tag(hl);
    hl_flush_ws(hl);
}

// Drops pending whitespace so that it won't get written.
static void hl_drop_ws(struct Highlighter *hl) { hl->pending_ws.data = NULL; }

// Writes a highlighted span with the given class (or none if it's null),
// escaping special characters with HTML entities.
static void hl_write(struct Highlighter *hl, const char *class,
                     struct Span span) {
    if (class != hl->class || (!MINIMIZE_TAGS && hl->pending_ws.data)) {
        hl_flush(hl);
        if (class) {
            buf_write(hl->buf, SPAN("<span class=\""));
            buf_write(hl->buf, SPAN(class));
            buf_write(hl->buf, SPAN("\">"));
            hl->class = class;
        }
    } else {
        hl_flush_ws(hl);
    }
    assert(!hl->pending_ws.data);
    size_t i, j;
    for (i = 0, j = 0; j < span.len;) {
        const char *entity;
        switch (span.data[j]) {
        case '<':
            entity = "&lt;";
            break;
        case '>':
            entity = "&gt;";
            break;
        case '&':
            entity = "&amp;";
            break;
        default:
            j++;
            continue;
        }
        buf_write(hl->buf, SUBSPAN(span, i, j));
        buf_write(hl->buf, SPAN(entity));
        j++;
        i = j;
    }
    buf_write(hl->buf, SUBSPAN(span, i, j));
}

// Writes a span consisting only of whitespace. This is a special case because
// so that we can apply the MINIMIZE_TAGS optimization.
static void hl_whitespace(struct Highlighter *hl, struct Span span) {
    assert(!hl->pending_ws.data);
    hl->pending_ws = span;
}

// Returns true if c is a hexadecimal digit.
static bool is_hex(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F');
}

// Returns true if c is a valid character in a Scheme identifier.
static bool is_ident(char c) {
    if (isalnum(c)) {
        return true;
    }
    switch (c) {
    case '!':
    case '$':
    case '%':
    case '*':
    case '+':
    case '-':
    case '.':
    case '/':
    case ':':
    case '<':
    case '=':
    case '>':
    case '?':
    case '^':
    case '_':
    case '~':
        return true;
    default:
        return false;
    }
}

// Names of characters that can be used in character literals such as #\space.
static const char *const CHARACTER_NAMES[] = {"space", "newline", "tab"};

// Kinds of tokens.
//
// Note: We use T_{L,R}PAREN for both parens and square brackets because they
// are interchangeable in Scheme R6RS. Well, they need to be paired correctly,
// e.g. "(+ 1 1]" is invalid, but checking that is not the highlighter's job.
enum TokenKind {
    // Whitespace.
    T_WHITESPACE,
    // Comment, from ";" to the end of the line.
    T_COMMENT,
    // Left parenthesis "(" or bracket "[".
    T_LPAREN,
    // Right parenthesis ")" or bracket "]".
    T_RPAREN,
    // Quote "'".
    T_QUOTE,
    // Quasiquote "`".
    T_QUASIQUOTE,
    // Unquote ",".
    T_UNQUOTE,
    // Syntax quote "#'".
    T_SYNTAX,
    // Syntax quasiquote "#`".
    T_QUASISYNTAX,
    // Syntax unquote "#,".
    T_UNSYNTAX,
    // Hash by itself, like in '#(a vector).
    T_HASH,
    // Identifier.
    T_IDENTIFIER,
    // String literal.
    T_STRING,
    // String literal with escape sequences.
    T_STRING_WITH_ESCAPES,
    // Numeric literal.
    T_NUMBER,
    // Boolean like #t or character like #\a or #\space.
    T_LITERAL,
    // SICP module identifier like :1.2 or ?3.4.
    T_SICP_ID,
    // Metavariable, i.e. identifier surrounded with "«" and "»".
    T_METAVARIABLE,
    // Console output, from "→" to the end of the line.
    T_CONSOLE,
};

#if DEBUG_TOKENS
// Map from TokenKind values to string names.
static const char *TOKEN_KIND_NAMES[] = {
    [T_WHITESPACE] = "whitespace",
    [T_COMMENT] = "comment",
    [T_LPAREN] = "lparen",
    [T_RPAREN] = "rparen",
    [T_QUOTE] = "quote",
    [T_QUASIQUOTE] = "quasiquote",
    [T_UNQUOTE] = "unquote",
    [T_SYNTAX] = "syntax",
    [T_QUASISYNTAX] = "quasisyntax",
    [T_UNSYNTAX] = "unsyntax",
    [T_HASH] = "hash",
    [T_IDENTIFIER] = "identifier",
    [T_STRING] = "string",
    [T_STRING_WITH_ESCAPES] = "string_with_escapes",
    [T_NUMBER] = "number",
    [T_LITERAL] = "literal",
    [T_SICP_ID] = "sicp_id",
    [T_METAVARIABLE] = "metavariable",
    [T_CONSOLE] = "console",
};
#endif

// Returns true if the token is a datum, i.e. a thing which has a value.
static bool is_datum(enum TokenKind kind) {
    switch (kind) {
    case T_WHITESPACE:
    case T_COMMENT:
    case T_LPAREN:
    case T_RPAREN:
    case T_QUOTE:
    case T_QUASIQUOTE:
    case T_UNQUOTE:
    case T_SYNTAX:
    case T_QUASISYNTAX:
    case T_UNSYNTAX:
    case T_HASH:
    case T_CONSOLE:
        return false;
    default:
        return true;
    }
}

// If the token is one of the identifiers "quote", ..., "unsyntax", changes
// *kind to T_QUOTE, ..., T_UNSYNTAX and returns true.
static bool convert_quote_ident(enum TokenKind *kind, struct Span token) {
    if (*kind != T_IDENTIFIER) {
        return false;
    }
    if (span_eq(token, SPAN("quote"))) {
        *kind = T_QUOTE;
    } else if (span_eq(token, SPAN("quasiquote"))) {
        *kind = T_QUASIQUOTE;
    } else if (span_eq(token, SPAN("unquote"))) {
        *kind = T_UNQUOTE;
    } else if (span_eq(token, SPAN("syntax"))) {
        *kind = T_SYNTAX;
    } else if (span_eq(token, SPAN("quasisyntax"))) {
        *kind = T_QUASISYNTAX;
    } else if (span_eq(token, SPAN("unsyntax"))) {
        *kind = T_UNSYNTAX;
    } else {
        return false;
    }
    return true;
}

// Gets the first token in the string starting at p and ending at end (must be
// nonempty). Returns a pointer to the end of the token, and sets *kind.
static const char *token_end(const char *p, const char *end,
                             enum TokenKind *kind) {
    assert(p != end);
    const char *start = p;
    while (p < end && isspace(*p)) {
        p++;
    }
    if (p != start) {
        *kind = T_WHITESPACE;
        return p;
    }
    assert(p != end);
    switch (*p++) {
    case ';':
        while (p < end && *p != '\n') {
            p++;
        }
        *kind = T_COMMENT;
        return p;
    case '(':
    case '[':
        *kind = T_LPAREN;
        return p;
    case ')':
    case ']':
        *kind = T_RPAREN;
        return p;
    case '\'':
        *kind = T_QUOTE;
        return p;
    case '`':
        *kind = T_QUASIQUOTE;
        return p;
    case ',':
        *kind = T_UNQUOTE;
        return p;
    case '"':
        *kind = T_STRING;
        while (p < end && *p != '"') {
            if (*p == '\\') {
                *kind = T_STRING_WITH_ESCAPES;
                p++;
                assert(p < end);
            }
            p++;
        }
        assert(p < end && *p == '"');
        p++;
        return p;
    case '-':
    case '+':
        if (p < end && (isdigit(*p) || *p == '.')) {
            goto number;
        }
        if (p + 5 <= end
            && (strncmp(p, "inf.0", 5) == 0 || strncmp(p, "nan.0", 5) == 0)) {
            p += 5;
            assert(!(p < end && is_ident(*p)));
            *kind = T_NUMBER;
            return p;
        }
        break;
    case '.':
        if (p < end && isdigit(*p)) {
            goto number;
        }
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    number:
        while (p < end && isdigit(*p)) {
            p++;
        }
        if (*p == '.' || *p == '/') {
            p++;
            while (p < end && isdigit(*p)) {
                p++;
            }
        }
        if (*p == 'e') {
            p++;
            if (*p == '+' || *p == '-') {
                p++;
            }
            while (p < end && isdigit(*p)) {
                p++;
            }
        }
        // This ensures e.g. the function "-1+" is treated as an identifier.
        if (p < end && is_ident(*p)) {
            break;
        }
        *kind = T_NUMBER;
        return p;
    case '#':
        if (*p == '\'') {
            p++;
            *kind = T_SYNTAX;
            return p;
        }
        if (*p == '`') {
            p++;
            *kind = T_QUASISYNTAX;
            return p;
        }
        if (*p == ',') {
            p++;
            *kind = T_UNSYNTAX;
            return p;
        }
        if (*p == '(') {
            *kind = T_HASH;
            return p;
        }
        if (*p == 'x') {
            p++;
            while (p < end && is_hex(*p)) {
                p++;
            }
            assert(!(p < end && is_ident(*p)));
            *kind = T_NUMBER;
            return p;
        }
        *kind = T_LITERAL;
        if (*p == 't' || *p == 'f') {
            p++;
            return p;
        }
        assert(*p == '\\');
        p++;
        for (size_t i = 0; i < ARRAY_LEN(CHARACTER_NAMES); i++) {
            ssize_t len = strlen(CHARACTER_NAMES[i]);
            if (p + len <= end && strncmp(p, CHARACTER_NAMES[i], len) == 0) {
                p += len;
                return p;
            }
        }
        assert(isgraph(*p));
        p++;
        return p;
    case ':':
    case '?':
        while (p < end && (*p == '.' || isdigit(*p))) {
            p++;
        }
        // If there is still more, treat it as an identifier. For example, it
        // could be an identifier like "?c" used for symbolic derivation.
        if (p < end && is_ident(*p)) {
            break;
        }
        *kind = T_SICP_ID;
        return p;
    case "«"[0]:
        if (p < end && *p == "«"[1]) {
            while (p < end
                   && !(*p == "»"[0] && p + 1 < end && p[1] == "»"[1])) {
                p++;
            }
            assert(p < end);
            p += 2;
            *kind = T_METAVARIABLE;
            return p;
        }
        break;
    case "→"[0]:
        if (p + 1 < end && *p == "→"[1] && p[1] == "→"[2]) {
            while (p < end && *p != '\n') {
                p++;
            }
            *kind = T_CONSOLE;
            return p;
        }
        break;
    }
    // No other case matched, so it must be an identifier.
    while (p < end && is_ident(*p)) {
        p++;
    }
    *kind = T_IDENTIFIER;
    return p;
}

// Scanner state.
struct Scanner {
    // Most recently scanned token.
    struct Span token;
    // The token's kind.
    enum TokenKind kind;
    // End of the text being scanned.
    const char *end;
};

// Initializes the scanner to scan the given text.
static void scan_init(struct Scanner *scan, struct Span text) {
    scan->token.data = text.data;
    scan->token.len = 0;
    scan->end = SPAN_END(text);
}

// Scans a token, setting scan->token and scan->kind. Returns false on EOF.
static bool scan_next(struct Scanner *scan) {
    const char *start = SPAN_END(scan->token);
    assert(start <= scan->end);
    if (start == scan->end) {
        return false;
    }
    const char *end = token_end(start, scan->end, &scan->kind);
    scan->token.data = start;
    scan->token.len = end - start;
    return true;
}

// Returns true if the next token has the given kind, without consuming it.
static bool scan_peek(struct Scanner *scan, enum TokenKind kind) {
    struct Scanner peek = *scan;
    return scan_next(&peek) && peek.kind == kind;
}

#if DEBUG_TOKENS
// Dumps the latest scanned token to stderr.
static void scan_dump(struct Scanner *scan) {
    fprintf(stderr, "token: %s %.*s\n", TOKEN_KIND_NAMES[scan->kind],
            (int)scan->token.len, scan->token.data);
}
#endif

// Kinds of Scheme quotation.
enum Quote {
    // Not quoted: either the top level, or unquoted within a quasiquote.
    Q_NONE,
    // Regular quotation like 'X or (quote X).
    Q_QUOTE,
    // Syntax quotation like #'X or (syntax X).
    Q_SYNTAX,
};

#if DEBUG_QUOTES
// Map from Quote values to string names.
static const char *const QUOTE_NAMES[] = {
    [Q_NONE] = "none",
    [Q_QUOTE] = "quote",
    [Q_SYNTAX] = "syntax",
};
#endif

// Returns the quote the given token is associated with, or Q_NONE.
static enum Quote related_quote(enum TokenKind kind) {
    switch (kind) {
    case T_QUOTE:
    case T_QUASIQUOTE:
    case T_UNQUOTE:
        return Q_QUOTE;
    case T_SYNTAX:
    case T_QUASISYNTAX:
    case T_UNSYNTAX:
        return Q_SYNTAX;
    default:
        return Q_NONE;
    }
}

// Represents quotation and paren nesting state at a particular point.
struct QuoteState {
    // Quotation we are in at this point.
    enum Quote quote;
    // Number of unquotes to escape from the quasiquote, or 0 for non-quasi.
    int quasi;
    // Number of parens to close before exiting this quote.
    int depth;
    // False for '(this style), true for (quote that style).
    bool wrapped;
};

// Data structure for keeping track of quotation nesting.
struct QuoteTracker {
    // Index of the top of the stack.
    int i;
    // Stack of i+1 items.
    struct QuoteState stack[8];
    // True if the last token was T_LPAREN.
    bool last_was_lparen;
};

// Initializes a quote tracker for the top level.
static void qt_init(struct QuoteTracker *qt) {
    qt->i = 0;
    qt->stack[0] = (struct QuoteState){
        .quote = Q_NONE,
        .quasi = 0,
        .depth = 0,
        .wrapped = false,
    };
    qt->last_was_lparen = false;
}

// Pushes a new item onto the quote stack.
static void qt_push(struct QuoteTracker *qt, enum Quote quote) {
    qt->i++;
    assert(qt->i >= 1 && qt->i < (int)ARRAY_LEN(qt->stack));
    qt->stack[qt->i] = (struct QuoteState){
        .quote = quote,
        .quasi = qt->stack[qt->i - 1].quasi,
        .depth = 0,
        .wrapped = false,
    };
}

// Processes the next token, updating the quote stack.
static void qt_process(struct QuoteTracker *qt, struct Span token,
                       enum TokenKind kind) {
    bool last_was_lparen = qt->last_was_lparen;
    qt->last_was_lparen = kind == T_LPAREN;
    // Pop depthless items off the stack if we hit a datum. For example, this
    // will pop three times for '''X.
    if (qt->stack[qt->i].depth == 0 && is_datum(kind)) {
        while (qt->i > 0 && qt->stack[qt->i].depth == 0) {
            qt->i--;
        }
        return;
    }
    // Handle parens by incrementing or decrementing the depth.
    switch (kind) {
    case T_LPAREN:
        qt->stack[qt->i].depth++;
        return;
    case T_RPAREN:
        assert(qt->stack[qt->i].depth > 0);
        qt->stack[qt->i].depth--;
        if (qt->i > 0 && qt->stack[qt->i].depth == 0) {
            assert(qt->i > 0);
            qt->i--;
        }
        return;
    default:
        break;
    }
    enum Quote quote = qt->stack[qt->i].quote;
    int quasi = qt->stack[qt->i].quasi;
    // If we're in a non-quasi quote, return early. There's nothing to do but
    // wait until the depth reaches 0 and we exit the quote.
    if (quote != Q_NONE && quasi == 0) {
        return;
    }
    // If the last token was "(", try converting identifiers like "quote" into
    // the corresponding punctuation like T_QUOTE.
    bool converted = false;
    if (last_was_lparen) {
        converted = convert_quote_ident(&kind, token);
    }
    enum Quote related = related_quote(kind);
    switch (kind) {
    case T_QUOTE:
    case T_SYNTAX:
        // Only handle these quotes in Q_NONE. There's no need to keep track if
        // of them if we're already in a quote.
        if (quote == Q_NONE) {
            qt_push(qt, related);
        }
        break;
    case T_QUASIQUOTE:
    case T_QUASISYNTAX:
        // We need to handle repeated quasiquotes such as because they nest:
        // ``,,(+ 1 1) evaluates to 2 while ``,(+ 1 1) evaluates to `,(+ 1 1).
        if (quote == Q_NONE || quote == related) {
            qt_push(qt, related);
            qt->stack[qt->i].quasi++;
        }
        break;
    case T_UNQUOTE:
    case T_UNSYNTAX:
        assert(quote != Q_NONE);
        // Only handle unquotes if we're in the corresponding quasiquote.
        if (quote == related) {
            qt_push(qt, related);
            qt->stack[qt->i].quasi--;
            if (qt->stack[qt->i].quasi == 0) {
                qt->stack[qt->i].quote = Q_NONE;
            }
        }
        break;
    default:
        return;
    }
    // Since we returned in the default case above, if we're here we must have
    // just pushed onto the stack. If we had converted an identifier, we need to
    // transfer one paren's worth of depth from the old stack item to the new,
    // and remember this fact by setting wrapped to true.
    if (converted) {
        assert(qt->i >= 1);
        assert(qt->stack[qt->i - 1].depth > 0);
        assert(qt->stack[qt->i].depth == 0);
        qt->stack[qt->i - 1].depth--;
        qt->stack[qt->i].depth++;
        qt->stack[qt->i].wrapped = true;
    }
}

// Processes the next token and returns the quote it belongs to.
static enum Quote qt_next(struct QuoteTracker *qt, struct Span token,
                          enum TokenKind kind) {
    int prev_i = qt->i;
    struct QuoteState prev = qt->stack[qt->i];
    qt_process(qt, token, kind);
    if (qt->i < prev_i && prev.wrapped) {
        // We're in a case like the closing paren of (quote X). This is the one
        // time where we return the post-token state rather than the pre-token
        // state, because the closing paren is not part of the quoted data.
        return qt->stack[qt->i].quote;
    }
    // In all other cases, return the pre-token state. For example, in 'X, the
    // ' itself is in Q_NONE but it causes the next token (X) to be in Q_QUOTE.
    return prev.quote;
}

#if DEBUG_QUOTES
// Dumps the quote stack to stderr.
static void qt_dump(struct QuoteTracker *qt) {
    fprintf(stderr, "quote: ");
    for (int i = 0; i <= qt->i; i++) {
        if (i != 0) {
            putc(' ', stderr);
        }
        fprintf(stderr, "[%s %d (%d)]", QUOTE_NAMES[qt->stack[i].quote],
                qt->stack[i].quasi, qt->stack[i].depth);
    }
    fprintf(stderr, "\n");
}
#endif

// CSS classes used by docs/style.css.
static const char C_ATTENTION[] = "at";
static const char C_CONSOLE[] = "cs";
static const char C_CONSTANT[] = "cn";
static const char C_ERROR[] = "er";
static const char C_FUNCTION[] = "fu";
static const char C_KEYWORD[] = "kw";
static const char C_METAVARIABLE[] = "mv";
static const char C_OPERATOR[] = "op";
static const char C_QUOTED[] = "qu";

// Renders a Scheme comment to HTML.
static void render_comment(struct Highlighter *out, struct Span token) {
    if (span_eq(token, SPAN("; NOALIGN"))) {
        assert(out->pending_ws.data);
        hl_drop_ws(out);
        return;
    }
    hl_write(out, C_ATTENTION, token);
}

// Renders a string literal with escapes to HTML.
static void render_string_literal(struct Highlighter *out, struct Span token) {
    assert(token.data[0] == '\"' && token.data[token.len - 1] == '\"');
    size_t i, j;
    for (i = 0, j = 1; j < token.len - 1;) {
        if (token.data[j] != '\\') {
            j++;
            continue;
        }
        if (i < j) {
            hl_write(out, C_CONSTANT, SUBSPAN(token, i, j));
        }
        i = j;
        j++;
        assert(j < token.len - 1);
        if (token.data[j] == 'x') {
            j++;
            assert(j < token.len - 1);
            while (token.data[j] != ';') {
                assert(is_hex(token.data[j]));
                j++;
                assert(j < token.len - 1);
            }
            j++;
        } else if (strchr("abfnrtv\"\\", token.data[j])) {
            j++;
        } else {
            assert(false);
        }
        hl_write(out, C_ATTENTION, SUBSPAN(token, i, j));
        i = j;
    }
    hl_write(out, C_CONSTANT, SUBSPAN(token, i, token.len));
}

// Renders a Scheme identifier to HTML.
static void render_identifier(struct Highlighter *out, struct Span token) {
    if (IN_ARRAY(token, NORMAL_ASSERTIONS)) {
        hl_write(out, C_OPERATOR, token);
    } else if (IN_ARRAY(token, ERROR_ASSERTIONS)) {
        hl_write(out, C_ERROR, token);
    } else if (IN_ARRAY(token, SPECIAL_FORMS)) {
        hl_write(out, C_KEYWORD, token);
    } else if (IN_ARRAY(token, FUNCTIONS)) {
        hl_write(out, C_FUNCTION, token);
    } else if (span_eq(token, SPAN("true")) || span_eq(token, SPAN("false"))) {
        // Scheme uses #t and #f but the textbook uses true and false.
        hl_write(out, C_CONSTANT, token);
    } else {
        hl_write(out, NULL, token);
    }
}

// Renders a metavariable to HTML.
static void render_metavariable(struct Highlighter *out, struct Span token) {
    // Remove the enclosing "«" and "»".
    hl_write(out, C_METAVARIABLE, SUBSPAN(token, 2, token.len - 2));
}

// Renders a link for a SICP ID token.
static void render_sicp_id_link(struct Highlighter *out, struct Span token,
                                lua_State *L) {
    lua_pushvalue(L, -1);
    lua_pushlstring(L, token.data, token.len);
    lua_call(L, 1, 1);
    struct Span href;
    href.data = luaL_checklstring(L, -1, &href.len);
    hl_flush(out);
    buf_write(out->buf, SPAN("<a href=\""));
    buf_write(out->buf, href);
    buf_write(out->buf, SPAN("\">"));
    buf_write(out->buf, token);
    buf_write(out->buf, SPAN("</a>"));
    lua_pop(L, 1);
}

// Renders code, writing HTML with highlight spans. If sicp_id_links is true,
// makes anchor links for SICP IDs like the :1.2 or ?3.4 using the Lua function
// at the top of the stack to produce its href attribute.
static void render(struct Highlighter *out, struct QuoteTracker *qt,
                   struct Span text, lua_State *L, bool sicp_id_links) {
    struct Scanner scan;
    scan_init(&scan, text);
    while (scan_next(&scan)) {
#if DEBUG_QUOTES
        qt_dump(qt);
#endif
#if DEBUG_TOKENS
        scan_dump(&scan);
#endif
        switch (qt_next(qt, scan.token, scan.kind)) {
        case Q_QUOTE:
        case Q_SYNTAX:
            switch (scan.kind) {
            case T_WHITESPACE:
                hl_whitespace(out, scan.token);
                break;
            case T_COMMENT:
                render_comment(out, scan.token);
                break;
            case T_METAVARIABLE:
                render_metavariable(out, scan.token);
                break;
            default:
                hl_write(out, C_QUOTED, scan.token);
                break;
            }
            continue;
        case Q_NONE:
            // Handled below.
            break;
        }
        switch (scan.kind) {
        case T_WHITESPACE:
            hl_whitespace(out, scan.token);
            break;
        case T_COMMENT:
            render_comment(out, scan.token);
            break;
        case T_LPAREN:
            hl_write(out, NULL, scan.token);
            break;
        case T_RPAREN:
            hl_write(out, NULL, scan.token);
            break;
        case T_QUOTE:
        case T_QUASIQUOTE:
        case T_SYNTAX:
        case T_QUASISYNTAX:
            if (scan_peek(&scan, T_METAVARIABLE)) {
                hl_write(out, NULL, scan.token);
            } else {
                hl_write(out, C_QUOTED, scan.token);
            }
            break;
        case T_UNQUOTE:
        case T_UNSYNTAX:
        case T_HASH:
            // Should be inside quotes.
            assert(false);
            break;
        case T_IDENTIFIER:
            render_identifier(out, scan.token);
            break;
        case T_STRING:
        case T_NUMBER:
        case T_LITERAL:
            hl_write(out, C_CONSTANT, scan.token);
            break;
        case T_STRING_WITH_ESCAPES:
            render_string_literal(out, scan.token);
            break;
        case T_SICP_ID:
            if (sicp_id_links) {
                render_sicp_id_link(out, scan.token, L);
            } else {
                hl_write(out, NULL, scan.token);
            }
            break;
        case T_METAVARIABLE:
            render_metavariable(out, scan.token);
            break;
        case T_CONSOLE:
            hl_write(out, C_CONSOLE, scan.token);
            break;
        }
    }
}

// Highlights Scheme code in the string argument. Optionally takes a second
// table argument with "sicp_id_link" set to a function that takes an ID
// like ":1.2" or "?3.4" and returns a URL like "path/file.html#fragment".
int l_highlight(lua_State *L) {
    struct Span text;
    text.data = luaL_checklstring(L, 1, &text.len);
    bool sicp_id_links = false;
    if (lua_gettop(L) == 2 && !lua_isnil(L, 2)) {
        luaL_checktype(L, 2, LUA_TTABLE);
        lua_getfield(L, 2, "sicp_id_link");
        if (!lua_isnil(L, -1)) {
            luaL_checktype(L, -1, LUA_TFUNCTION);
            sicp_id_links = true;
        }
    }
#if DEBUG_INPUT
    fputs("highlight:\n\t", stderr);
    for (size_t i = 0; i < text.len; i++) {
        char c = text.data[i];
        if (c == '\n') {
            fputs("\n\t", stderr);
        } else {
            putc(c, stderr);
        }
    }
    putc('\n', stderr);
#endif
    struct Buffer buf;
    // Use double the input length as a guess for how much space we need.
    buf_init(&buf, text.len * 2);
    struct Highlighter hi;
    hl_init(&hi, &buf);
    struct QuoteTracker qt;
    qt_init(&qt);
    render(&hi, &qt, text, L, sicp_id_links);
    hl_flush(&hi);
    lua_pushlstring(L, buf.data, buf.len);
    return 1;
}

static const luaL_Reg library[] = {{"highlight", l_highlight}, {NULL, NULL}};

LUALIB_API int luaopen_schemehl(lua_State *L) {
    ASSERT_SORTED(NORMAL_ASSERTIONS);
    ASSERT_SORTED(ERROR_ASSERTIONS);
    ASSERT_SORTED(SPECIAL_FORMS);
    ASSERT_SORTED(FUNCTIONS);
    luaL_newlib(L, library);
    return 1;
}
