// Copyright 2024 Mitchell Kember. Subject to the MIT License.

const std = @import("std");

// Maximum number of columns allowed by the style guide.
const maxColumns = 80;

// Maximum paren nesting depth.
const maxDepth = 64;

// Skip alignment checks on lines ending with this comment.
const noAlignComment = "; NOALIGN\n";

// Lines in a Markdown file indicating the start of Scheme code. The explicit
// "```scheme" is only used in README.md for GitHub rendering.
const markdownSchemeStart1 = "```scheme\n";
const markdownSchemeStart2 = "```\n";

// Line prefix in a Markdown file that starts a code block. We need to recognize
// these to avoid treating the ending "```" as MARKDOWN_SCHEME_START_2.
const markdownCodeStartPrefix = "```";

// Line in a Markdown file indicating the end of a code block.
const markdownCodeEnd = "```\n";

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
    if (operator.ptr - line.ptr != 1) rules.wrapper = false;
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
    quoted_align: i16 = 0,
    // Stack of alignments, one for each unclosed parenthesis. A new line is
    // expected to be indented by stack.get(stack.len() - 1) spaces.
    stack: std.BoundedArray(u8, 64) = .{},
    // The import block we are currently inside, or IB_NONE.
    import_mode: ImportBlock = .none,
    // The value of import_mode at the start of the previous line.
    prev_import_mode: ImportBlock = .none,
    // String containing the last id in the import block.
    last_import_id: []const u8 = "",
    // String containing the last name in the import block.
    last_import_name: []const u8 = "",

    fn fail(self: *Linter, column: u16, comptime format: []const u8, args: anytype) void {
        std.io.getStdErr().writer().print("{s}:{}:{}:" ++ format ++ "\n", .{ self.filename, self.lineno, column } ++ args) catch unreachable;
        self.failed = true;
    }

    fn lintLine(self: *Linter, line: []const u8) void {
        _ = self; // autofix
        _ = line; // autofix
        //
    }
};

// Reads and lints the given file. If it is a Markdown file (".md" extension),
// only lints Scheme code in fenced code blocks.
fn lint(path: []const u8) bool {
    _ = path; // autofix
    //
}

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
    if (std.mem.eql(u8, std.os.argv[1], "-h") or std.mem.eql(u8, std.os.argv[1], "--help")) {
        printUsage(std.io.getStdOut());
        return;
    }
    var failed = false;
    for (std.os.argv[1..]) |path| if (!lint(path)) {
        failed = true;
    };
    if (failed) std.process.exit(1);
}
