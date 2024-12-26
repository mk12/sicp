// Copyright 2024 Mitchell Kember. Subject to the MIT License.

const std = @import("std");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.docgen);

fn printUsage(file: std.fs.File) void {
    file.writer().print(
        \\Usage: {s} OUT_FILE
        \\
        \\Generate OUT_FILE from Markdown and Scheme sources
        \\
        \\This is part of a custom static site generator for the SICP Study project.
        \\For more information, see the "Website" section in README.md.
        \\
        \\Arguments:
        \\    OUT_FILE  Path matching docs/**/*.html
        \\
    , .{std.os.argv[0]}) catch unreachable;
}

pub fn main() void {
    if (std.os.argv.len != 2) {
        printUsage(std.io.getStdErr());
        std.process.exit(1);
    }
    const arg = std.mem.span(std.os.argv[1]);
    if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
        printUsage(std.io.getStdOut());
        return;
    }
    if (std.fs.path.dirname(arg)) |dirname| std.fs.cwd().makeDir(dirname) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.io.getStdErr().writer().print("{s}: {s}", .{ dirname, @errorName(err) }) catch unreachable;
            std.process.exit(1);
        },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    gen(arena.allocator(), arg) catch |err| {
        std.io.getStdErr().writer().print("{s}: {s}", .{ arg, @errorName(err) }) catch unreachable;
        std.process.exit(1);
    };
}

fn gen(allocator: Allocator, output: []const u8) !void {
    if (std.mem.eql(u8, output, "docs/index.html")) return genIndex(allocator, output);
    if (std.mem.startsWith(u8, output, "docs/text")) {
        if (std.mem.eql(u8, output, "docs/text/index.html")) return genTextIndex(allocator, output);
    }
}

// Generates docs/index.html.
fn genIndex(allocator: Allocator, output: []const u8) !void {
    var proc = try pandoc(allocator, .{
        .input = "notes/index.md",
        .output = output,
        .dest = output,
        .title = "SICP Study",
    });
    try wait(&proc);
}

// Generates docs/text/index.html.
fn genTextIndex(allocator: Allocator, output: []const u8) !void {
    var scanner = try MarkdownScanner.init(output);
    var proc = try pandoc(allocator, .{
        .dest = output,
        .title = "SICP Notes",
        .navigation = .{
            .up = "../index.html",
            .prev = null,
            .next = "highlight.html",
        },
    });
    while (try scanner.scan()) |line|
        if (scanner.noHeading()) try proc.stdin.?.writeAll(line) else break;
    // render heading
    // while scan md copy md
    // toc renderer
    // finish pandoc
    try wait(&proc);
}

// Options for invoking pandoc.
const PandocOpts = struct {
    // Path to the input file. Uses stdin if null.
    input: ?[]const u8 = null,
    // Path to the output file. Uses stdout if null.
    output: ?[]const u8 = null,
    // The final destination path, used for construct the -M id=... parameter.
    dest: []const u8,
    // Contents of <title>...</title>.
    title: []const u8,
    // Links to up/prev/next page.
    navigation: ?struct {
        up: []const u8,
        prev: ?[]const u8,
        next: ?[]const u8,
    } = null,
};

// Invokes pandoc, printing the command to stderr before executing it.
fn pandoc(allocator: Allocator, opts: PandocOpts) !std.process.Child {
    var argv = std.BoundedArray([]const u8, 15){};
    try argv.append("pandoc");
    if (opts.output) |path| try argv.appendSlice(&.{ "-o", path });
    try argv.append("-dpandoc/config.yml");
    try argv.append("-M");
    try argv.append(try std.fmt.allocPrint(allocator, "id={s}", .{idFromDestPath(opts.dest)}));
    try argv.append("-M");
    try argv.append(try std.fmt.allocPrint(allocator, "title={s}", .{opts.title}));
    if (opts.navigation) |navigation| {
        try argv.append("-M");
        try argv.append(try std.fmt.allocPrint(allocator, "up={s}", .{navigation.up}));
        if (navigation.prev) |prev| {
            try argv.append("-M");
            try argv.append(try std.fmt.allocPrint(allocator, "prev={s}", .{prev}));
        }
        if (navigation.next) |next| {
            try argv.append("-M");
            try argv.append(try std.fmt.allocPrint(allocator, "next={s}", .{next}));
        }
    }
    if (opts.input) |path| try argv.append(path);
    try std.io.getStdErr().writer().print("{}\n", .{formatArgv(argv.slice())});
    var proc = std.process.Child.init(argv.slice(), allocator);
    try proc.spawn();
    return proc;
}

fn idFromDestPath(dest: []const u8) []const u8 {
    return dest[std.mem.indexOfScalar(u8, dest, '/').? + 1 .. std.mem.lastIndexOfScalar(u8, dest, '.').?];
}

fn wait(proc: *std.process.Child) !void {
    const term = try proc.wait();
    if (term == .Exited and term.Exited == 0) return;
    switch (term) {
        inline else => |code, tag| {
            try std.io.getStdErr().writer().print(
                "{}: {s}: {}\n",
                .{ formatArgv(proc.argv), @tagName(tag), code },
            );
            return error.ProcessFailed;
        },
    }
}

fn formatArgv(argv: []const []const u8) std.fmt.Formatter(formatArgvFn) {
    return .{ .data = argv };
}

fn formatArgvFn(argv: []const []const u8, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = f;
    _ = options;
    for (argv, 0..) |arg, i| {
        if (i != 0) try writer.writeByte(' ');
        try if (std.mem.indexOfAny(u8, arg, " '\"") == null)
            writer.writeAll(arg)
        else
            writer.print("\"{}\"", .{std.zig.fmtEscapes(arg)});
    }
}

// Markdown line scanner.
const MarkdownScanner = struct {
    // The Markdown file.
    file: std.fs.File,
    reader: std.io.BufferedReader(4096, std.fs.File.Reader),
    // Current line.
    line: std.BoundedArray(u8, 128) = .{},
    // True if we are inside a fenced code block.
    code: bool = false,
    // Current sector within the document.
    sector: Sector = .{},
    // If this line is a heading, 1/2/... for h1/h2/..., otherwise 0.
    level: u4 = 0,

    fn init(path: []const u8) !MarkdownScanner {
        const file = try std.fs.cwd().openFile(path, .{});
        return .{ .file = file, .reader = std.io.bufferedReader(file.reader()) };
    }

    fn deinit(self: MarkdownScanner) void {
        self.file.close();
    }

    fn noHeading(self: MarkdownScanner) bool {
        return self.sector.toInt() == 0;
    }

    fn scan(self: *MarkdownScanner) !?[]const u8 {
        const prev_blank = self.line.len <= 1;
        self.level = 0;
        if (!try readLine(self.reader.reader(), &self.line)) return null;
        const line = self.line.slice();
        if (std.mem.startsWith(u8, line, "```")) {
            self.code = !self.code;
        } else if (!self.code and prev_blank) {
            if (std.mem.indexOfNone(u8, line, "#")) |i| if (line[i] == ' ') {
                self.level = @intCast(i);
                self.sector = self.sector.next(self.level);
            };
        }
        return line;
    }
};

// A document sector represents a place within the textbook.
// An index of zero means that heading has not been encountered yet.
const Sector = packed struct(u64) {
    chapter: u8 = 0,
    section: u8 = 0,
    subsection: u8 = 0,
    subsubsection: u8 = 0,
    exercise: u8 = 0,
    _padding: u24 = 0,

    fn toInt(self: Sector) u64 {
        return std.mem.littleToNative(u64, @bitCast(self));
    }

    fn fromInt(val: u64) Sector {
        return @bitCast(std.mem.nativeToLittle(u64, val));
    }

    fn next(self: Sector, level: u4) Sector {
        // TODO(https://github.com/ziglang/zig/issues/6903): Remove?
        const one: u64 = 1;
        const mask = (one << level * 8) - 1;
        const inc = one << (level - 1) * 8;
        return fromInt(self.toInt() & mask + inc);
    }
};

// Reads an entire line and stores as much as possible bounded_array. Returns false on EOF.
fn readLine(reader: anytype, bounded_array: anytype) !bool {
    bounded_array.len = 0;
    reader.streamUntilDelimiter(bounded_array.writer(), '\n', bounded_array.buffer.len) catch |err| switch (err) {
        error.EndOfStream => return false,
        else => {
            if (err == error.StreamTooLong) try reader.skipUntilDelimiterOrEof('\n');
            return err;
        },
    };
    return true;
}

test {
    _ = std.testing.refAllDecls(@This());
}
