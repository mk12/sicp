// Copyright 2024 Mitchell Kember. Subject to the MIT License.

//! This Lua C library provides Scheme syntax highlighting. It is tailored to the
//! SICP Study project use case and does not adhere to any spec.
//!
//! Example usage from Lua:
//!
//!     schemehl = require("schemehl") -- loads schemehl.so from LUA_CPATH
//!
//!     print(schemehl.highlight("(define x 1)"))
//!
//!     print(schemehl.highlight("(paste (:1.2 foo) (?3.4 bar))", {
//!         sicp_id_link = function(id)
//!             -- id is ":1.2" or "?3.4"
//!             return "path/file.html#fragment"
//!         end
//!     }))
//!

const std = @import("std");
const assert = std.debug.assert;

const c = @cImport({
    @cInclude("lua5.4/lauxlib.h");
    @cInclude("lua5.4/lua.h");
});

// Debugging flags.
const debug_input = false;
const debug_tokens = false;
const debug_quotes = false;

const library = [_]c.luaL_Reg{
    .{ .name = "highlight", .func = l_highlight },
    .{ .name = null, .func = null },
};

export fn luaopen_schemehl(L: *c.lua_State) c_int {
    // Would use the luaL_newlib macro but @cImport doesn't understand it.
    c.luaL_checkversion(L);
    c.lua_createtable(L, 0, library.len - 1);
    c.luaL_setfuncs(L, &library, 0);
    return 1;
}

// CSS classes used by docs/style.css.
const Class = enum {
    attention,
    console,
    constant,
    @"error",
    function,
    keyword,
    metavariable,
    operator,
    quoted,

    fn name(self: Class) []const u8 {
        return switch (self) {
            .attention => "at",
            .console => "cs",
            .constant => "cn",
            .@"error" => "er",
            .function => "fu",
            .keyword => "kw",
            .metavariable => "mv",
            .operator => "op",
            .quoted => "qu",
        };
    }
};

// Returns true if the given CSS class is visible on whitespace, e.g. because it
// sets a background color or underline.
fn visibleOnWhitespace(class: ?Class) bool {
    return class == .metavariable;
}

// SICP assertion operators to highlight normally.
const normalAssertions = std.StaticStringMap(void).initComptime(.{
    .{"=$>"},
    .{"=>"},
    .{"=?>"},
    .{"~>"},
});

// SICP assertion operators to highlight as errors.
const errorAssertions = std.StaticStringMap(void).initComptime(.{
    .{"=!>"},
    .{"=>..."},
});

// Special forms to highlight.
const specialForms = std.StaticStringMap(void).initComptime(.{
    .{"and"},
    .{"begin"},
    .{"capture-output"},
    .{"case"},
    .{"catch"},
    .{"cond"},
    .{"cons-stream"},
    .{"define"},
    .{"define-record-type"},
    .{"define-syntax"},
    .{"delay"},
    .{"else"},
    .{"export"},
    .{"hide-output"},
    .{"if"},
    .{"import"},
    .{"lambda"},
    .{"let"},
    .{"let*"},
    .{"let*-values"},
    .{"let-syntax"},
    .{"let-values"},
    .{"letrec"},
    .{"letrec*"},
    .{"letrec-syntax"},
    .{"library"},
    .{"or"},
    .{"paste"},
    .{"quasiquote"},
    .{"quasisyntax"},
    .{"quote"},
    .{"set!"},
    .{"syntax"},
    .{"syntax-case"},
    .{"syntax-rules"},
    .{"unquote"},
    .{"unquote-splicing"},
    .{"unsyntax"},
    .{"unsyntax-splicing"},
    .{"with-eval"},
});

// Builtin functions to highlight.
const functions = std.StaticStringMap(void).initComptime(.{
    .{"!"},
    .{"*"},
    .{"+"},
    .{"-"},
    .{"-1+"},
    .{"/"},
    .{"1+"},
    .{"<"},
    .{"<="},
    .{"="},
    .{">"},
    .{">="},
    .{"abs"},
    .{"acos"},
    .{"append"},
    .{"apply"},
    .{"asin"},
    .{"assoc"},
    .{"assp"},
    .{"assq"},
    .{"assv"},
    .{"atan"},
    .{"boolean=?"},
    .{"boolean?"},
    .{"bound-identifier=?"},
    .{"caaaar"},
    .{"caaadr"},
    .{"caaar"},
    .{"caadar"},
    .{"caaddr"},
    .{"caadr"},
    .{"caar"},
    .{"cadaar"},
    .{"cadadr"},
    .{"cadar"},
    .{"caddar"},
    .{"cadddr"},
    .{"caddr"},
    .{"cadr"},
    .{"call-with-current-continuation"},
    .{"call-with-input-file"},
    .{"call-with-output-file"},
    .{"call-with-port"},
    .{"call-with-string-output-port"},
    .{"call-with-values"},
    .{"call/cc"},
    .{"car"},
    .{"cdaaar"},
    .{"cdaadr"},
    .{"cdaar"},
    .{"cdadar"},
    .{"cdaddr"},
    .{"cdadr"},
    .{"cdar"},
    .{"cddaar"},
    .{"cddadr"},
    .{"cddar"},
    .{"cdddar"},
    .{"cddddr"},
    .{"cdddr"},
    .{"cddr"},
    .{"cdr"},
    .{"ceiling"},
    .{"char->integer"},
    .{"char-alphabetic?"},
    .{"char-ci<=?"},
    .{"char-ci<?"},
    .{"char-ci=?"},
    .{"char-ci>=?"},
    .{"char-ci>?"},
    .{"char-downcase"},
    .{"char-foldcase"},
    .{"char-general-category"},
    .{"char-lower-case?"},
    .{"char-numeric?"},
    .{"char-ready?"},
    .{"char-title-case?"},
    .{"char-titlecase"},
    .{"char-upcase"},
    .{"char-upper-case?"},
    .{"char-whitespace?"},
    .{"char<=?"},
    .{"char<?"},
    .{"char<?c"},
    .{"char=?"},
    .{"char>=?"},
    .{"char>?"},
    .{"char?"},
    .{"complex?"},
    .{"condition"},
    .{"condition-accessor"},
    .{"condition-irritants"},
    .{"condition-message"},
    .{"condition-predicate"},
    .{"condition-who"},
    .{"condition?"},
    .{"cons"},
    .{"cos"},
    .{"current-error-port"},
    .{"current-input-port"},
    .{"current-output-port"},
    .{"datum->syntax"},
    .{"denominator"},
    .{"display"},
    .{"div-and-mod"},
    // Omit "div" here since it looks bad in the generic operation sections
    // where only it would be highlighted but not "add", "sub", and "mul".
    .{"div0"},
    .{"div0-and-mod0"},
    .{"eq?"},
    .{"equal-hash"},
    .{"equal?"},
    .{"eqv?"},
    .{"error"},
    .{"error?"},
    .{"eval"},
    .{"even?"},
    .{"exact"},
    .{"exact->inexact"},
    .{"exact-integer-sqrt"},
    .{"exact?"},
    // Omit "exp" here since we use it for expression parameters.
    .{"expt"},
    .{"filter"},
    .{"find"},
    .{"finite?"},
    .{"fixnum-width"},
    .{"fixnum?"},
    .{"floor"},
    .{"for-each"},
    .{"force"},
    .{"free-identifier=?"},
    .{"fx*"},
    .{"fx*/carry"},
    .{"fx+"},
    .{"fx+/carry"},
    .{"fx-"},
    .{"fx-/carry"},
    .{"fx<=?"},
    .{"fx<?"},
    .{"fx=?"},
    .{"fx>=?"},
    .{"fx>?"},
    .{"fxand"},
    .{"fxarithmetic-shift"},
    .{"fxarithmetic-shift-left"},
    .{"fxarithmetic-shift-right"},
    .{"fxbit-count"},
    .{"fxbit-field"},
    .{"fxbit-set?"},
    .{"fxcopy-bit"},
    .{"fxcopy-bit-field"},
    .{"fxdiv"},
    .{"fxdiv-and-mod"},
    .{"fxdiv0"},
    .{"fxdiv0-and-mod0"},
    .{"fxeven?"},
    .{"fxfirst-bit-set"},
    .{"fxif"},
    .{"fxior"},
    .{"fxlength"},
    .{"fxmax"},
    .{"fxmin"},
    .{"fxmod"},
    .{"fxmod0"},
    .{"fxnegative?"},
    .{"fxnot"},
    .{"fxodd?"},
    .{"fxpositive?"},
    .{"fxreverse-bit-field"},
    .{"fxrotate-bit-field"},
    .{"fxxor"},
    .{"fxzero?"},
    .{"gcd"},
    .{"hashtable-clear!"},
    .{"hashtable-contains?"},
    .{"hashtable-copy"},
    .{"hashtable-delete!"},
    .{"hashtable-entries"},
    .{"hashtable-equivalence-function"},
    .{"hashtable-hash-function"},
    .{"hashtable-keys"},
    .{"hashtable-mutable?"},
    .{"hashtable-ref"},
    .{"hashtable-set!"},
    .{"hashtable-size"},
    .{"hashtable-update!"},
    .{"hashtable?"},
    .{"identifier-syntax"},
    .{"identifier?"},
    .{"inexact"},
    .{"inexact->exact"},
    .{"inexact?"},
    .{"infinite?"},
    .{"input-port?"},
    .{"integer->char"},
    .{"integer-valued?"},
    .{"integer?"},
    .{"interaction-environment"},
    .{"irritants-condition?"},
    .{"lcm"},
    .{"least-fixnum"},
    .{"length"},
    .{"lexical-violation?"},
    .{"list"},
    .{"list->string"},
    .{"list->vector"},
    .{"list-ref"},
    .{"list-sort"},
    .{"list-tail"},
    .{"list?"},
    .{"log"},
    .{"make-assertion-violation"},
    .{"make-eq-hashtable"},
    .{"make-eqv-hashtable"},
    .{"make-error"},
    .{"make-hashtable"},
    .{"make-syntax-violation"},
    .{"map"},
    .{"max"},
    .{"member"},
    .{"memp"},
    .{"memq"},
    .{"memv"},
    .{"message-condition?"},
    .{"min"},
    .{"mod"},
    .{"mod0"},
    .{"modulo"},
    .{"nan?"},
    .{"negative?"},
    .{"newline"},
    .{"not"},
    .{"null?"},
    .{"number->string"},
    .{"number?"},
    .{"numerator"},
    .{"odd?"},
    .{"pair?"},
    .{"partition"},
    .{"peek-char"},
    .{"port?"},
    .{"positive?"},
    .{"procedure?"},
    .{"quotient"},
    .{"rational-valued?"},
    .{"rational?"},
    .{"rationalize"},
    .{"read"},
    .{"read-char"},
    .{"real-valued?"},
    .{"real?"},
    .{"remainder"},
    .{"remove"},
    .{"remp"},
    .{"remq"},
    .{"remv"},
    .{"reverse"},
    .{"round"},
    .{"set-car!"},
    .{"set-cdr!"},
    .{"sin"},
    .{"sqrt"},
    .{"string"},
    .{"string->list"},
    .{"string->number"},
    .{"string->symbol"},
    .{"string-append"},
    .{"string-ci-hash"},
    .{"string-ci<=?"},
    .{"string-ci<?"},
    .{"string-ci=?"},
    .{"string-ci>=?"},
    .{"string-ci>?"},
    .{"string-copy"},
    .{"string-count"},
    .{"string-downcase"},
    .{"string-fill!"},
    .{"string-foldcase"},
    .{"string-hash"},
    .{"string-length"},
    .{"string-ref"},
    .{"string-set!"},
    .{"string-titlecase"},
    .{"string-upcase"},
    .{"string<=?"},
    .{"string<?"},
    .{"string=?"},
    .{"string>=?"},
    .{"string>?"},
    .{"string?"},
    .{"substring"},
    .{"symbol->string"},
    .{"symbol-hash"},
    .{"symbol=?"},
    .{"symbol?"},
    .{"syntax->datum"},
    .{"syntax-violation"},
    .{"syntax-violation?"},
    .{"tan"},
    .{"truncate"},
    .{"values"},
    .{"vector"},
    .{"vector-fill!"},
    .{"vector-length"},
    .{"vector-ref"},
    .{"vector-set!"},
    .{"vector-sort"},
    .{"vector-sort!"},
    .{"vector?"},
    .{"violation?"},
    .{"warning?"},
    .{"who-condition?"},
    .{"with-exception-handler"},
    .{"with-syntax"},
    .{"write"},
    .{"write-char"},
    .{"zero?"},
});

// Wrapper around a buffer that writes <span> tags for highlighting and escapes
// special characters with HTML entities.
const Highlighter = struct {
    writer: std.ArrayList(u8).Writer,
    class: ?Class = null,
    pending_whitespace: ?[]const u8 = null,

    fn init(buffer: *std.ArrayList(u8)) Highlighter {
        return Highlighter{ .writer = buffer.writer() };
    }

    // Flushes any pending writes to the buffer. Must be called at the end.
    fn flush(self: *Highlighter) !void {
        try self.flushSpanTag();
        try self.flushWhitespace();
    }

    fn flushSpanTag(self: *Highlighter) !void {
        _ = self.class orelse return;
        try self.writer.writeAll("</span>");
        self.class = null;
    }

    fn flushWhitespace(self: *Highlighter) !void {
        const whitespace = self.pending_whitespace orelse return;
        try self.writer.writeAll(whitespace);
        self.pending_whitespace = null;
    }

    // Drops pending whitespace so that it won't get written.
    fn dropWhitespace(self: *Highlighter) void {
        self.pending_whitespace = null;
    }

    // Writes text highlighted with the given class, escaping special characters
    // with HTML entities.
    fn write(self: *Highlighter, class: ?Class, text: []const u8) !void {
        if (class != self.class or (visibleOnWhitespace(class) and self.pending_whitespace != null)) {
            try self.flush();
            if (class) |cl| {
                try self.writer.print("<span class=\"{s}\">", .{cl.name()});
                self.class = cl;
            }
        } else {
            try self.flushWhitespace();
        }
        assert(self.pending_whitespace == null);
        var i: usize = 0;
        var j: usize = 0;
        while (j < text.len) {
            const entity = switch (text[j]) {
                '<' => "&lt;",
                '>' => "&gt;",
                '&' => "&amp;",
                else => {
                    j += 1;
                    continue;
                },
            };
            try self.writer.writeAll(text[i..j]);
            try self.writer.writeAll(entity);
            j += 1;
            i = j;
        }
        try self.writer.writeAll(text[i..j]);
    }

    // Writes a span consisting only of whitespace. This is a special case so
    // that we can avoid closing and reopening span tags with the same class.
    fn writeWhitespace(self: *Highlighter, whitespace: []const u8) !void {
        assert(self.pending_whitespace == null);
        self.pending_whitespace = whitespace;
    }
};

// Returns true if c is a valid character in a Scheme identifier.
fn isIdent(char: u8) bool {
    if (std.ascii.isAlphanumeric(char)) return true;
    return switch (char) {
        '!', '$', '%', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '^', '_', '~' => true,
        else => false,
    };
}

// Names of characters that can be used in character literals such as #\space.
const characterNames = std.StaticStringMap(void).initComptime(.{
    .{"space"},
    .{"newline"},
    .{"tab"},
});

// Kinds of tokens.
const TokenKind = enum {
    // Whitespace.
    whitespace,
    // Comment, from ";" to the end of the line.
    comment,
    // Left parenthesis "(" or bracket "[". They are interchangeable in R6RS.
    lparen,
    // Right parenthesis ")" or bracket "]". They are interchangeable in R6RS.
    rparen,
    // Quote "'".
    quote,
    // Quasiquote "`".
    quasiquote,
    // Unquote ",".
    unquote,
    // Syntax quote "#'".
    syntax,
    // Syntax quasiquote "#`".
    quasisyntax,
    // Syntax unquote "#,".
    unsyntax,
    // Hash by itself, like in '#(a vector).
    hash,
    // Identifier.
    identifier,
    // String literal.
    string,
    // String literal with escape sequences.
    string_with_escapes,
    // Numeric literal.
    number,
    // Boolean like #t or character like #\a or #\space.
    literal,
    // SICP module identifier like :1.2 or ?3.4.
    sicp_id,
    // Metavariable, i.e. identifier surrounded with "«" and "»".
    metavariable,
    // Console output, from "→" to the end of the line.
    console,

    // Returns true if the token is a datum, i.e. a thing which has a value.
    fn isDatum(self: TokenKind) bool {
        return switch (self) {
            .whitespace, .comment, .lparen, .rparen, .quote, .quasiquote, .unquote, .syntax, .quasisyntax, .unsyntax, .hash, .console => false,
            else => true,
        };
    }

    // Returns the token kind that corresponds to a quoting identifier.
    fn convertQuoteIdent(self: TokenKind, token: []const u8) ?TokenKind {
        if (self != .identifier) return null;
        return std.StaticStringMap(TokenKind).initComptime(.{
            .{ "quote", .quote },
            .{ "quasiquote", .quasiquote },
            .{ "unquote", .unquote },
            .{ "syntax", .syntax },
            .{ "quasisyntax", .quasisyntax },
            .{ "unsyntax", .unsyntax },
        }).get(token);
    }

    // Returns the quote the token is associated with.
    fn relatedQuote(self: TokenKind) Quote {
        return switch (self) {
            .quote, .quasiquote, .unquote => .quote,
            .syntax, .quasisyntax, .unsyntax => .syntax,
            else => .none,
        };
    }
};

// Scanner for Scheme syntax.
const Scanner = struct {
    text: []const u8,
    offset: usize = 0,

    fn next(self: *Scanner) !?struct { token: []const u8, kind: TokenKind } {
        if (self.eof()) return null;
        const start = self.offset;
        const kind = try self.recognize();
        return .{ .token = self.text[start..self.offset], .kind = kind };
    }

    fn peek(self: *Scanner, kind: TokenKind) bool {
        var scanner = self.*;
        const item = (scanner.next() catch return false) orelse return false;
        return item.kind == kind;
    }

    fn recognize(self: *Scanner) !TokenKind {
        assert(!self.eof());
        const start = self.offset;
        self.eatWhile(std.ascii.isWhitespace);
        if (self.offset != start) return .whitespace;
        switch (self.eat().?) {
            ';' => {
                self.eatUntil('\n');
                return .comment;
            },
            '(', '[' => return .lparen,
            ')', ']' => return .rparen,
            '\'' => return .quote,
            '`' => return .quasiquote,
            ',' => return .unquote,
            '"' => {
                var kind = TokenKind.string;
                while (self.get()) |char| : (self.inc()) switch (char) {
                    '"' => break,
                    '\\' => {
                        kind = .string_with_escapes;
                        self.inc();
                    },
                    else => {},
                } else return error.UnclosedDoubleQuote;
                self.inc();
                return kind;
            },
            '-', '+' => if (!self.eof() and std.ascii.isDigit(self.get().?) and self.recognizeNumber()) {
                return .number;
            } else if (self.offset < self.text.len - 5) {
                const span = self.text[self.offset .. self.offset + 5];
                if (std.mem.eql(u8, span, "inf.0") or std.mem.eql(u8, span, "nan.0")) {
                    self.offset += 5;
                    return .number;
                }
            },
            '.' => if (self.get()) |char| if (std.ascii.isDigit(char) and self.recognizeNumber()) return .number,
            '0'...'9' => if (self.recognizeNumber()) return .number,
            '#' => if (self.eat()) |char| switch (char) {
                '\'' => return .syntax,
                '`' => return .quasisyntax,
                ',' => return .unsyntax,
                '(' => return .hash,
                'x' => {
                    self.eatWhile(std.ascii.isHex);
                    if (self.get()) |ch| assert(!isIdent(ch));
                    return .number;
                },
                't', 'f' => return .literal,
                '\\' => {
                    self.eatWhile(std.ascii.isAlphabetic);
                    return .literal;
                },
                else => return error.InvalidHashSyntax,
            },
            ':', '?' => {
                while (self.get()) |char| : (self.inc()) if (!(char == '.' or std.ascii.isDigit(char))) break;
                // If there's an identifier after, this could be something like
                // "?c" in the code used for symbolic differentiation.
                if (self.get()) |char| if (!isIdent(char)) return .sicp_id;
            },
            "«"[0] => if (self.get()) |char| if (char == "«"[1]) {
                const end = std.mem.indexOfPos(u8, self.text, self.offset, "»") orelse return error.MissingMetaClose;
                self.offset = end + "»".len;
                return .metavariable;
            },
            "→"[0] => if (self.get()) |ch1| if (ch1 == "→"[1]) if (self.get()) |ch2| if (ch2 == "→"[2]) {
                self.eatUntil('\n');
                return .console;
            },
            else => {},
        }
        // No other case matched, so it must be an identifier.
        self.eatWhile(isIdent);
        return .identifier;
    }

    fn recognizeNumber(self: *Scanner) bool {
        self.eatWhile(std.ascii.isDigit);
        if (self.get()) |char| if (char == '.' or char == '/') {
            self.inc();
            self.eatWhile(std.ascii.isDigit);
        };
        if (self.get()) |char| if (char == 'e') {
            self.inc();
            if (self.get()) |ch| if (ch == '+' or ch == '-') self.inc();
            self.eatWhile(std.ascii.isDigit);
        };
        // This ensures e.g. the function "-1+" is treated as an identifier.
        if (self.get()) |char| if (isIdent(char)) return false;
        return true;
    }

    fn eof(self: Scanner) bool {
        return self.offset == self.text.len;
    }

    fn get(self: Scanner) ?u8 {
        return if (self.eof()) null else self.text[self.offset];
    }

    fn inc(self: *Scanner) void {
        assert(self.offset < self.text.len);
        self.offset += 1;
    }

    fn eat(self: *Scanner) ?u8 {
        defer self.inc();
        return self.get();
    }

    fn eatUntil(self: *Scanner, end: u8) void {
        while (self.get()) |char| : (self.inc()) if (char != end) break;
    }

    fn eatWhile(self: *Scanner, predicate: anytype) void {
        while (self.get()) |char| : (self.inc()) if (!predicate(char)) break;
    }
};

// Kinds of Scheme quotation.
const Quote = enum {
    // Not quoted: either the top level, or unquoted within a quasiquote.
    none,
    // Regular quotation like 'X or (quote X).
    quote,
    // Syntax quotation like #'X or (syntax X).
    syntax,
};

// Represents quotation and paren nesting state at a particular point.
const QuoteState = struct {
    // Quotation we are in at this point.
    quote: Quote = .none,
    // Number of unquotes to escape from the quasiquote, or 0 for non-quasi.
    quasi: u8 = 0,
    // Number of parens to close before exiting this quote.
    depth: u8 = 0,
    // False for '(this style), true for (quote that style).
    wrapped: bool = false,
};

// Data structure for keeping track of quotation nesting.
const QuoteTracker = struct {
    stack: std.BoundedArray(QuoteState, 8),
    last_was_lparen: bool,

    fn init() QuoteTracker {
        var qt = QuoteTracker{ .stack = .{}, .last_was_lparen = false };
        qt.stack.append(QuoteState{}) catch unreachable;
        return qt;
    }

    fn top(self: *QuoteTracker) *QuoteState {
        return &self.stack.buffer[self.stack.len - 1];
    }

    fn second(self: *QuoteTracker) *QuoteState {
        return &self.stack.buffer[self.stack.len - 2];
    }

    fn len(self: *QuoteTracker) usize {
        return self.stack.len;
    }

    fn pop(self: *QuoteTracker) void {
        _ = self.stack.pop();
    }

    fn push(self: *QuoteTracker, quote: Quote) !void {
        try self.stack.append(QuoteState{
            .quote = quote,
            .quasi = self.top().quasi,
        });
    }

    fn process(self: *QuoteTracker, token: []const u8, kind: TokenKind) !void {
        const last_was_lparen = self.last_was_lparen;
        self.last_was_lparen = kind == .lparen;
        // Pop depthless items off the stack if we hit a datum. For example,
        // this will pop three times for '''X.
        if (self.top().depth == 0 and kind.isDatum()) {
            while (self.len() > 1 and self.top().depth == 0) self.pop();
            return;
        }
        // Handle parens by incrementing or decrementing the depth.
        switch (kind) {
            .lparen => {
                self.top().depth += 1;
                return;
            },
            .rparen => {
                const depth = &self.top().depth;
                assert(depth.* > 0);
                depth.* -= 1;
                if (self.len() > 1 and depth.* == 0) self.pop();
                return;
            },
            else => {},
        }
        const quote = self.top().quote;
        // If we're in a non-quasi quote, return early. There's nothing to do
        // but wait until the depth reaches 0 and we exit the quote.
        if (quote != .none and self.top().quasi == 0) return;
        // If the last token was "(", try converting identifiers like "quote"
        // into the corresponding punctuation like T_QUOTE.
        const converted = if (last_was_lparen) kind.convertQuoteIdent(token) else null;
        const new_kind = converted orelse kind;
        const related = new_kind.relatedQuote();
        switch (new_kind) {
            // Only handle these quotes in Q_NONE. There's no need to keep track if
            // of them if we're already in a quote.
            .quote, .syntax => if (quote == .none) try self.push(related),
            // We need to handle repeated quasiquotes such as because they nest:
            // ``,,(+ 1 1) evaluates to 2 while ``,(+ 1 1) evaluates to `,(+ 1 1).
            .quasiquote, .quasisyntax => if (quote == .none or quote == related) {
                try self.push(related);
                self.top().quasi += 1;
            },
            .unquote, .unsyntax => {
                assert(quote != .none);
                // Only handle unquotes if we're in the corresponding quasiquote.
                if (quote == related) {
                    try self.push(related);
                    self.top().quasi -= 1;
                    if (self.top().quasi == 0) self.top().quote = .none;
                }
            },
            else => return,
        }
        // Since we returned in the default case above, if we're here we must have
        // just pushed onto the stack. If we had converted an identifier, we need to
        // transfer one paren's worth of depth from the old stack item to the new,
        // and remember this fact by setting wrapped to true.
        if (converted) |_| {
            assert(self.len() >= 2);
            assert(self.second().depth > 0);
            assert(self.top().depth == 0);
            self.second().depth -= 1;
            self.top().depth += 1;
            self.top().wrapped = true;
        }
    }

    // Processes the next token and returns the quote it belongs to.
    fn next(self: *QuoteTracker, token: []const u8, kind: TokenKind) !Quote {
        const prev_len = self.len();
        const prev = self.top().*;
        try self.process(token, kind);
        if (self.len() < prev_len and prev.wrapped) {
            // We're in a case like the closing paren of (quote X). This is the one
            // time where we return the post-token state rather than the pre-token
            // state, because the closing paren is not part of the quoted data.
            return self.top().quote;
        }
        return prev.quote;
    }
};

fn renderComment(hl: *Highlighter, text: []const u8) !void {
    if (std.mem.eql(u8, text, "; NOALIGN")) {
        assert(hl.pending_whitespace != null);
        hl.dropWhitespace();
        return;
    }
    try hl.write(.attention, text);
}

fn renderStringLiteral(hl: *Highlighter, token: []const u8) !void {
    assert(token[0] == '"' and token[token.len - 1] == '"');
    var i: usize = 0;
    var j: usize = 1;
    while (j < token.len - 1) {
        if (token[j] == '\\') {
            j += 1;
            continue;
        }
        if (i < j) try hl.write(.constant, token[i..j]);
        i = j;
        j += 1;
        switch (token[j]) {
            'x' => {
                j += 1;
                while (token[j] != ';') {
                    assert(std.ascii.isHex(token[j]));
                    j += 1;
                }
                j += 1;
            },
            'a', 'b', 'f', 'n', 'r', 't', 'v', '"', '\\' => j += 1,
            else => unreachable,
        }
        try hl.write(.attention, token[i..j]);
        i = j;
    }
    try hl.write(.constant, token[i..]);
}

fn renderIdentifier(hl: *Highlighter, token: []const u8) !void {
    const class: ?Class = if (normalAssertions.get(token)) |_|
        .operator
    else if (errorAssertions.get(token)) |_|
        .@"error"
    else if (specialForms.get(token)) |_|
        .keyword
    else if (functions.get(token)) |_|
        .function
    else if (std.mem.eql(u8, token, "true") or std.mem.eql(u8, token, "false"))
        .constant
    else
        null;
    try hl.write(class, token);
}

fn renderMetavariable(hl: *Highlighter, token: []const u8) !void {
    // Remove the enclosing "«" and "»".
    try hl.write(.metavariable, token[2 .. token.len - 2]);
}

fn renderSicpIdLink(hl: *Highlighter, token: []const u8, L: ?*c.lua_State) !void {
    c.lua_pushvalue(L, -1);
    _ = c.lua_pushlstring(L, token.ptr, token.len);
    c.lua_callk(L, 1, 1, 0, null);
    var href_len: usize = undefined;
    const href_ptr = c.luaL_checklstring(L, -1, &href_len);
    const href = href_ptr[0..href_len];
    try hl.flush();
    try hl.writer.print("<a href=\"{s}\">{s}</a>", .{ href, token });
    c.lua_pop(L, 1);
}

// Renders code, writing HTML with highlight spans. If sicp_id_links is true,
// makes anchor links for SICP IDs like the :1.2 or ?3.4 using the Lua function
// at the top of the stack to produce its href attribute.
fn render(hl: *Highlighter, qt: *QuoteTracker, text: []const u8, L: ?*c.lua_State, sicp_id_links: bool) !void {
    var scanner = Scanner{ .text = text };
    while (try scanner.next()) |item| {
        const token = item.token;
        // if (debug_quotes) {}
        switch (try qt.next(token, item.kind)) {
            .quote, .syntax => {
                try switch (item.kind) {
                    .whitespace => hl.writeWhitespace(token),
                    .comment => renderComment(hl, token),
                    .metavariable => renderMetavariable(hl, token),
                    else => hl.write(.quoted, token),
                };
                continue;
            },
            // Handled below.
            .none => {},
        }
        try switch (item.kind) {
            .whitespace => hl.writeWhitespace(token),
            .comment => renderComment(hl, token),
            .lparen => hl.write(null, token),
            .rparen => hl.write(null, token),
            .quote, .quasiquote, .syntax, .quasisyntax => hl.write(
                if (scanner.peek(.metavariable)) null else .quoted,
                token,
            ),
            // Should be inside quotes.
            .unquote, .unsyntax, .hash => unreachable,
            .identifier => renderIdentifier(hl, token),
            .string, .number, .literal => hl.write(.constant, token),
            .string_with_escapes => renderStringLiteral(hl, token),
            .sicp_id => if (sicp_id_links)
                renderSicpIdLink(hl, token, L)
            else
                hl.write(null, token),
            .metavariable => renderMetavariable(hl, token),
            .console => hl.write(.console, token),
        };
    }
}

// Highlights Scheme code in the string argument. Optionally takes a second
// table argument with "sicp_id_link" set to a function that takes an ID
// like ":1.2" or "?3.4" and returns a URL like "path/file.html#fragment".
fn l_highlight(L: ?*c.lua_State) callconv(.C) c_int {
    var text_len: usize = undefined;
    const text_ptr = c.luaL_checklstring(L, 1, &text_len);
    const text = text_ptr[0..text_len];
    var sicp_id_links = false;
    if (c.lua_gettop(L) == 2 and !c.lua_isnil(L, 2)) {
        c.luaL_checktype(L, 2, c.LUA_TTABLE);
        _ = c.lua_getfield(L, 2, "sicp_id_link");
        if (!c.lua_isnil(L, -1)) {
            c.luaL_checktype(L, -1, c.LUA_TFUNCTION);
            sicp_id_links = true;
        }
    }
    if (debug_input) {
        std.debug.print("highlight:\n\t");
        for (text) |char| switch (char) {
            '\n' => std.debug.print("\n\t"),
            else => std.debug.print("{c}", .{char}),
        };
        std.debug.print("\n");
    }
    var buffer = std.ArrayList(u8).initCapacity(std.heap.c_allocator, text.len * 2) catch |err|
        return fail(L, "allocating buffer", err);
    var hl = Highlighter.init(&buffer);
    var qt = QuoteTracker.init();
    render(&hl, &qt, text, L, sicp_id_links) catch |err| return fail(L, "rendering", err);
    hl.flush() catch |err| return fail(L, "flushing", err);
    _ = c.lua_pushlstring(L, buffer.items.ptr, buffer.items.len);
    return 1;
}

fn fail(L: ?*c.lua_State, msg: []const u8, err: anytype) c_int {
    std.debug.print("ntsp.zig: {s}: {s}\n", .{ msg, @errorName(err) });
    c.lua_pushnil(L);
    return 1;
}
