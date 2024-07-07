// Copyright 2024 Mitchell Kember. Subject to the MIT License.

const std = @import("std");

// Maximum number of columns allowed by the style guide.
const maxColumns = 80;
// Maximum paren nesting depth.
const maxDepth = 64;

fn printUsage(file: std.fs.File) void {
    file.writer().print(
        \\Usage: {s} FILE ...
        \\
        \\Line Scheme code
        \\
        \\Arguments:
        \\    FILE  Scheme file or Markdown file (lints code blocks)
        \\
    , .{std.os.argv[0]}) catch unreachable;
}

pub fn main() void {
    if (std.os.argv.len < 2) {
        printUsage(std.io.getStdErr());
        std.process.exit(1);
    }
    const arg1 = std.mem.span(std.os.argv[1]);
    if (std.mem.eql(u8, arg1, "-h") or std.mem.eql(u8, arg1, "--help")) {
        printUsage(std.io.getStdOut());
        return;
    }
    var failed = false;
    for (std.os.argv[1..]) |path| {
        var linter = Linter{ .filename = std.mem.span(path) };
        linter.lintFile();
        failed = failed or linter.failed;
    }
    if (failed) std.process.exit(1);
}

// Bit flags specifying indentation rules for an operator. The indentation of a
// line is determined by the operator belonging to the most recent (as of the
// start of the line) open paren that remains unclosed.
//
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
// Multiple operands per line are also allowed, though this should only be used
// in special cases (e.g. import lists, format arguments):
//
//     (operator operand1 operand2
//               operand3 operand4)
//
const IndentRules = packed struct {
    // When the special bit is set, the body (if present) must be indented by
    // two spaces from the open paren:
    //
    //     (operator operand
    //       body
    //       ...)
    //
    // Operands, if any, must be on the same line as the operator.
    special: bool = false,

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
    wrapper: bool = false,

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
    uniform: bool = false,
};

// Map from operand names to indentation rules.
const indentRulesMap = std.StaticStringMap(IndentRules).initComptime(.{
    // Exceptional cases.
    .{ "SICP", .{ .wrapper = true } },
    .{ "begin", .{ .special = true, .uniform = true } },
    .{ "cond", .{ .special = true, .uniform = true } },
    .{ "library", .{ .special = true, .wrapper = true } },

    // Special forms.
    .{ "Chapter", .{ .special = true } },
    .{ "Exercise", .{ .special = true } },
    .{ "Section", .{ .special = true } },
    .{ "case", .{ .special = true } },
    .{ "define", .{ .special = true } },
    .{ "define-record-type", .{ .special = true } },
    .{ "define-syntax", .{ .special = true } },
    .{ "lambda", .{ .special = true } },
    .{ "let", .{ .special = true } },
    .{ "let*", .{ .special = true } },
    .{ "let-syntax", .{ .special = true } },
    .{ "let-values", .{ .special = true } },
    .{ "letrec", .{ .special = true } },
    .{ "parameterize", .{ .special = true } },
    .{ "syntax-case", .{ .special = true } },
    .{ "syntax-rules", .{ .special = true } },
    .{ "unless", .{ .special = true } },
    .{ "when", .{ .special = true } },
    .{ "with-eval", .{ .special = true } },
    .{ "with-mutex", .{ .special = true } },
    .{ "with-syntax", .{ .special = true } },
});

// Looks up the indentation rules for the given operator.
fn lookupIndentRules(line: []const u8, operator: []const u8) IndentRules {
    var rules = indentRulesMap.get(operator) orelse return .{};
    // We only recognize wrapper forms occurring at the top level (column 1,
    // meaning the open paren is on column 0).
    if (@intFromPtr(operator.ptr) - @intFromPtr(line.ptr) != 1) rules.wrapper = false;
    return rules;
}

// Enumeration of the nested blocks used for importing.
//
// Normal import blocks:
//
//     (Section :1.2.3 "Some Title"
//       (use (:1.1 some proc) (?1.5 another one)))
//
// Paste import blocks:
//
//     (paste (:1.1 some proc) (?1.5 another one))
//
const ImportBlock = enum {
    // Not an import block.
    none,
    // Chapter/Section/Exercise block at the top level.
    sec,
    // The (use ...) block inside .sec.
    sec_use,
    // One of the (ID NAME ...) blocks inside .use.
    sec_use_arg,
    // A (paste ...) block at the top level.
    paste,
    // One of the (ID NAME ...) blocks inside .paste.
    paste_arg,

    // Returns true for (ID NAME ...) blocks.
    fn isInside(self: ImportBlock) bool {
        return self == .sec_use_arg or self == .paste_arg;
    }

    // Returns true for blocks that contain inside() blocks.
    fn isOutside(self: ImportBlock) bool {
        return self == .sec_use or self == .paste;
    }

    // Returns this block's parent.
    fn parent(self: ImportBlock) ImportBlock {
        return switch (self) {
            .none => unreachable,
            .sec => .none,
            .sec_use => .sec,
            .sec_use_arg => .sec_use,
            .paste => .none,
            .paste_arg => .paste,
        };
    }
};

// Looks up the new import block given the current one and an operator.
fn lookupImportBlock(current: ImportBlock, line: []const u8, operator: []const u8) ImportBlock {
    const column = operator.ptr - line.ptr;
    switch (current) {
        .none => if (column == 1) {
            if (std.mem.eql(u8, operator, "Chapter") or
                std.mem.eql(u8, operator, "Section") or
                std.mem.eql(u8, operator, "Exercise"))
                return .sec;
            if (std.mem.eql(u8, operator, "paste")) return .paste;
        },
        .sec => if (column == 3 and std.mem.eql(u8, operator, "use")) return .secUse,
        .secUse => return .secUseArg,
        .paste => return .pasteArg,
        else => {},
    }
    return .none;
}

// Returns the order of two Chapter/Section/Exercise ids.
fn idOrder(a: []const u8, b: []const u8) std.math.Order {
    // The Chapter/Section sigil ':' (0x3a) is less than the Exercise sigil '?' (0x3f).
    if (a[0] != b[0]) return std.math.order(a[0], b[0]);
    var cmp = std.math.Order.eq;
    const min = @min(a.len, b.len);
    for (1..min) |i| if (a[i] == '.' and b[i] == '.') {
        if (cmp != .eq) return cmp;
        cmp = .eq;
    } else {
        if (a[i] == '.') return .lt;
        if (b[i] == '.') return .gt;
        if (cmp == .eq) cmp = std.math.order(a[i], b[i]);
    };
    // Simulate appending a '.' at the end.
    const ca = if (min == a.len) '.' else a[min];
    const cb = if (min == b.len) '.' else b[min];
    if (ca == '.' and cb == '.' and cmp != .eq) return cmp;
    // Prefer the shorter (less specific) coming first.
    return std.math.order(a.len, b.len);
}

// Linter state for a single file.
const Linter = struct {
    // File currently being linted.
    filename: []const u8,
    // One-based line number.
    lineno: u16 = 1,
    // True if there were any errors.
    failed: bool = false,
    // Length of the previous line, excluding newline.
    prev_length: u16 = 0,
    // Number of blank lines in a row seen.
    prev_blanks: u16 = 0,
    // True if we are inside a string.
    in_string: bool = false,
    // Number of wrapper forms encountered.
    num_wrappers: u16 = 0,
    // If the last open paren was quoted, its alignment column (allowed as an
    // alternative to the 1st-operand/2-space alignment). Otherwise, -1.
    quoted_align: ?u8 = null,
    // Stack of alignments, one for each unclosed parenthesis. A new line is
    // expected to be indented by stack.get(stack.len - 1) spaces.
    stack: std.BoundedArray(u8, 64) = .{},
    // The import block we are currently inside, or IB_NONE.
    import_mode: ImportBlock = .none,
    // The value of import_mode at the start of the previous line.
    prev_import_mode: ImportBlock = .none,
    // String containing the last id in the import block.
    last_import_id: ?[]const u8 = null,
    // String containing the last name in the import block.
    last_import_name: ?[]const u8 = null,

    fn lintFile(self: *Linter) void {
        var file = std.fs.cwd().openFile(self.filename, .{}) catch |err| return self.failNoLocation("{s}", .{@errorName(err)});
        var input = std.io.bufferedReader(file.reader());
        var line_buf: [128]u8 = undefined;
        if (std.mem.endsWith(u8, self.filename, ".md")) {
            var mode: enum { text, non_scheme, scheme } = .text;
            while (true) {
                var output = std.io.fixedBufferStream(&line_buf);
                input.reader().streamUntilDelimiter(output.writer(), '\n', line_buf.len) catch |err| switch (err) {
                    error.EndOfStream => break,
                    error.StreamTooLong => if (mode != .text) return self.failNoColumn("line too long", .{}),
                    else => return self.failNoColumn("{s}", .{@errorName(err)}),
                };
                const line = output.getWritten();
                switch (mode) {
                    .text => if (std.mem.eql(u8, line, "```") or std.mem.eql(u8, line, "```scheme")) {
                        mode = .scheme;
                    } else if (std.mem.startsWith(u8, line, "```")) {
                        mode = .non_scheme;
                    },
                    .non_scheme => if (std.mem.eql(u8, line, "```")) {
                        mode = .text;
                    },
                    .scheme => if (std.mem.eql(u8, line, "```")) {
                        mode = .text;
                    } else {
                        self.lintLine(line);
                        self.prev_length = @intCast(line.len);
                    },
                }
                self.lineno += 1;
            }
        } else {
            while (true) {
                var output = std.io.fixedBufferStream(&line_buf);
                input.reader().streamUntilDelimiter(output.writer(), '\n', line_buf.len) catch |err| switch (err) {
                    error.EndOfStream => break,
                    error.StreamTooLong => return self.failNoColumn("line too long", .{}),
                    else => return self.failNoColumn("{s}", .{@errorName(err)}),
                };
                const line = output.getWritten();
                self.lintLine(line);
                self.prev_length = @intCast(line.len);
                self.lineno += 1;
            }
        }
    }

    fn lintLine(self: *Linter, line: []const u8) void {
        // Step 1. Check basic line length, whitespace, and comments.
        if (line.len == 0) {
            if (!self.in_string and self.prev_blanks == 1) self.fail(0, "multiple blank lines", .{});
            self.prev_blanks += 1;
            return;
        }
        self.prev_blanks = 0;
        const utf8_len = std.unicode.utf8CountCodepoints(line) catch return self.failNoColumn("invalid UTF-8", .{});
        if (utf8_len > maxColumns) self.fail(maxColumns - 1, "line too long: {} > {}", .{ utf8_len, maxColumns });
        if (line[line.len - 1] == ' ') self.fail(line.len - 1, "trailing whitespace", .{});
        if (line[0] == ';') {
            var i: usize = 1;
            while (i < line.len and line[i] == ';') i += 1;
            if (i > 3) self.fail(0, "too many semicolons", .{});
            if (i == 3 and self.lineno != 1) self.fail(0, "';;;' only allowed on first line copyright", .{});
            if (i < line.len and line[i] != ' ') self.fail(i, "missing space after ';'", .{});
            return;
        }

        // Steps 2. Check spacing, alignment, and import ordering.
        const no_align = std.mem.endsWith(u8, line, "; NOALIGN");
        var can_pack_inside = self.import_mode.isInside();
        var can_pack_outside = self.prev_import_mode != .none and !self.prev_import_mode.isInside() and self.import_mode.isOutside();
        var prev: u8 = 0; // previous character
        var escaped = false; // last character was unescaped backslash
        var two_spaces = false; // saw two spaces in a row
        var word_start: usize = 0; // start of the last word (used for imports)
        var last_open: usize = 0; // column of the last '(' (used for imports)
        var mode: enum { indent, normal, operator, string, comment, comment_space } = if (self.in_string) .string else .indent;
        self.prev_import_mode = self.import_mode;
        for (line, 0..) |char, i| {
            if (char == '\t') self.fail(i, "illegal tab character", .{});
            switch (mode) {
                .indent, .normal, .operator => {
                    if (mode == .indent) {
                        if (char != ' ' and !no_align and i != self.indent()) {
                            // TODO: off by one with stack len?
                            if (i == 0 and self.stack.len == self.num_wrappers) {
                                // Returning to zero indentation after wrapper opening.
                                self.setIndent(0);
                            } else if (@as(u8, @intCast(i)) == self.quoted_align) {
                                // Quoted form is data, not code.
                                self.setIndent(self.quoted_align.?);
                                self.quoted_align.? -= 1;
                            } else {
                                self.fail(i, "incorrect indentation", .{});
                            }
                        }
                        mode = .normal;
                    }
                    if (two_spaces and char != ' ' and char != ';') {
                        two_spaces = false;
                        self.fail(i, "unexpected two spaces in a row", .{});
                    }
                    switch (char) {
                        '"' => {
                            mode = .string;
                            self.in_string = true;
                        },
                        ';' => {
                            mode = .comment;
                            if (prev != ' ') self.fail(i, "expected space before ';'", .{});
                        },
                        '(', '[' => if (!escaped) {
                            mode = .operator;
                            self.stack.append(@intCast(i + 1)) catch |err| switch (err) {
                                error.Overflow => self.fail(i, "exceeded maximum nesting depth ({})", .{maxDepth}),
                            };
                            if (i > 0) switch (prev) {
                                ' ', '#', '\'', '(', ',', '@', '[', '`' => {},
                                else => self.fail(i, "expected space before '{c}'", .{char}),
                            };
                            // We only use square brackets for =?> and =$>.
                            self.quoted_align = if (prev == '\'' or (self.quoted_align != null and prev == '(') or char == '[') @intCast(i + 1) else null;
                            last_open = i;
                        },
                        ')', ']' => if (!escaped) {
                            mode = .normal;
                            if (i != 0 and self.stack.len == self.num_wrappers) self.fail(i, "expected ')' at start of line for wrapper", .{});
                            if (prev == ' ') self.fail(i, "unexpected space before ')'", .{});
                            if (self.import_mode != .none) {
                                if (self.import_mode.isInside()) {
                                    const name = line[word_start..i];
                                    if (idOrder(self.last_import_name.?, name) != .lt)
                                        self.fail(word_start, "incorrect import name ordering: {s} >= {s}", .{ self.last_import_name.?, name });
                                    self.last_import_name = null;
                                    if (can_pack_outside) {
                                        can_pack_outside = false;
                                        var would_be = self.prev_length + 1 + (i - last_open + 1);
                                        for (line[i + 1 ..]) |c| if (c == char) {
                                            would_be += 1;
                                        } else break;
                                        if (would_be <= maxColumns) self.fail(last_open, "pack imports on previous line: {s}", .{line[last_open..i]});
                                    }
                                    if (can_pack_inside) {
                                        can_pack_inside = false;
                                        var would_be = self.prev_length + 1 + (i - word_start + 1);
                                        for (line[i + 1 ..]) |c| if (c == char) {
                                            would_be += 1;
                                        } else break;
                                        if (would_be <= maxColumns) self.fail(last_open, "pack import on previous line: {s}", .{line[word_start..i]});
                                    }
                                } else if (self.import_mode.isOutside()) {
                                    self.last_import_id = null;
                                }
                                self.import_mode = self.import_mode.parent();
                            }
                            _ = self.stack.popOrNull() orelse self.fail(i, "unmatched '{c}'", .{char});
                        },
                        // TODO \n not in string
                        ' ', '\n' => {
                            if (prev == ' ' and char == ' ') two_spaces = true;
                            if (mode == .operator) {
                                mode = .normal;
                                const operator = line[word_start..i];
                                const rules = lookupIndentRules(line, operator);
                                if (rules.wrapper) self.num_wrappers += 1;
                                var j = i;
                                while (j < line.len and line[j] == ' ') j += 1;
                                // TODO
                            }
                        },
                        else => {},
                    }
                },
                .string => if (!escaped and char == '"') {
                    mode = .normal;
                    self.in_string = false;
                },
                .comment, .comment_space => {
                    if (mode == .comment and char == ';') mode = .comment_space;
                    if (char != ' ') self.fail(i, "expected space after ';'", .{});
                },
            }
            if (mode == .normal and prev == ' ' and char != ' ') word_start = i;
            prev = char;
            escaped = if (char == '\\') !escaped else false;
        }
    }

    fn indent(self: Linter) u8 {
        return self.stack.get(self.stack.len - 1);
    }

    fn setIndent(self: *Linter, value: u8) void {
        self.stack.set(self.stack.len - 1, value);
    }

    fn failNoLocation(self: *Linter, comptime format: []const u8, args: anytype) void {
        self.failImpl(" " ++ format, args);
    }

    fn failNoColumn(self: *Linter, comptime format: []const u8, args: anytype) void {
        self.failImpl("{}: " ++ format, .{self.lineno} ++ args);
    }

    fn fail(self: *Linter, column: usize, comptime format: []const u8, args: anytype) void {
        self.failImpl("{}:{}: " ++ format, .{ self.lineno, column + 1 } ++ args);
    }

    fn failImpl(self: *Linter, comptime format: []const u8, args: anytype) void {
        std.io.getStdErr().writer().print("{s}:" ++ format ++ "\n", .{self.filename} ++ args) catch unreachable;
        self.failed = true;
    }
};
