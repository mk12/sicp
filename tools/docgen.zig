// Copyright 2024 Mitchell Kember. Subject to the MIT License.

const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const Allocator = mem.Allocator;

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

pub fn main() !void {
    if (std.os.argv.len != 2) {
        printUsage(std.io.getStdErr());
        std.process.exit(1);
    }
    const arg = mem.span(std.os.argv[1]);
    if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
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
    const SIG = std.posix.SIG;
    killPandocOnSignals(&.{ SIG.HUP, SIG.INT, SIG.QUIT, SIG.TERM, SIG.ABRT });
    defer Pandoc.kill();
    gen(arena.allocator(), arg) catch |err| {
        std.io.getStdErr().writer().print("{s}: {s}", .{ arg, @errorName(err) }) catch unreachable;
        return err;
    };
}

fn gen(allocator: Allocator, output: []const u8) !void {
    if (mem.eql(u8, output, "docs/index.html")) return genIndex(allocator, output);
    if (mem.startsWith(u8, output, "docs/text/")) {
        if (mem.eql(u8, output, "docs/text/index.html")) return genTextIndex(allocator, output);
    }
    if (mem.startsWith(u8, output, "docs/lecture/")) {
        if (mem.eql(u8, output, "docs/lecture/index.html")) return genLectureIndex(allocator, output);
    }
}

// Generates docs/index.html.
fn genIndex(allocator: Allocator, output: []const u8) !void {
    var pandoc = try Pandoc.spawn(allocator, .{
        .input = "notes/index.md",
        .output = output,
        .path = output,
        .title = "SICP Study",
    });
    try pandoc.wait();
}

// Generates docs/text/index.html.
fn genTextIndex(allocator: Allocator, output: []const u8) !void {
    var md = try MarkdownScanner.init("notes/text.md");
    defer md.deinit();
    var pandoc = try Pandoc.spawn(allocator, .{
        .path = output,
        .title = "SICP Notes",
        .navigation = .{ .up = "../index.html", .prev = null, .next = "highlight.html" },
    });
    const writer = pandoc.stdin.?.writer();
    try renderHeading(writer, .h1, null, Heading{ .title = "Textbook Notes" });
    while (try md.scan() and md.heading == null) try writer.writeAll(md.get());
    var toc = TocRenderer{};
    try toc.renderStart(writer);
    try toc.renderItem(writer, 1, Heading{ .title = "Highlights" }, "highlight.html", .{});
    while (!md.eof) : (try md.next()) if (md.heading) |heading| switch (heading.level) {
        .h1 => break,
        .h2 => try toc.renderItem(writer, 1, heading.value, "front.html#{}", .{formatLower(heading.value.title.?)}),
        else => {},
    };
    while (!md.eof) : (try md.next()) if (md.heading) |heading| switch (heading.level) {
        .h1 => try toc.renderItem(writer, 1, heading.value, "{}/index.html", .{md.position.get(.h1)}),
        .h2 => try toc.renderItem(writer, 2, heading.value, "{}/{}.html", .{ md.position.get(.h1), md.position.get(.h2) }),
        else => {},
    };
    try toc.renderEnd(writer);
    try pandoc.finish(allocator, output);
}

// Generates docs/lecture/index.html.
fn genLectureIndex(allocator: Allocator, output: []const u8) !void {
    var md = try MarkdownScanner.init("notes/lecture.md");
    defer md.deinit();
    var pandoc = try Pandoc.spawn(allocator, .{
        .path = output,
        .title = "SICP Lecture Notes",
        .navigation = .{ .up = "../index.html", .prev = null, .next = "highlight.html" },
    });
    const writer = pandoc.stdin.?.writer();
    try renderHeading(writer, .h1, null, Heading{ .title = "Lecture Notes" });
    while (try md.scan() and md.heading == null) try writer.writeAll(md.get());
    var toc = TocRenderer{};
    try toc.renderStart(writer);
    try toc.renderItem(writer, 1, Heading{ .title = "Highlights" }, "highlight.html", .{});
    while (!md.eof) : (try md.next()) if (md.heading) |heading| if (heading.level == .h1) {
        try toc.renderItem(writer, 1, heading.value, "{}.html", .{formatLower(heading.value.title.?)});
    };
    try toc.renderEnd(writer);
    try pandoc.finish(allocator, output);
}

// A pandoc process.
const Pandoc = struct {
    var global_pandoc_proc: ?std.process.Child = null;
    var global_pandoc_proc_set = std.atomic.Value(bool).init(false);

    stdin: ?std.io.BufferedWriter(4096, std.fs.File.Writer),
    stdout: ?std.io.BufferedReader(4096, std.fs.File.Reader),

    // Options for invoking pandoc.
    const Opts = struct {
        // Path to the input file. Uses stdin if null.
        input: ?[]const u8 = null,
        // Path to the output file. Uses stdout if null.
        output: ?[]const u8 = null,
        // The final destination path, used for construct the -M id=... parameter.
        path: []const u8,
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
    fn spawn(allocator: Allocator, opts: Opts) !Pandoc {
        var argv = std.BoundedArray([]const u8, 15){};
        try argv.append("pandoc");
        if (opts.output) |path| try argv.appendSlice(&.{ "-o", path });
        try argv.append("-dpandoc/config.yml");
        try argv.append("-M");
        try argv.append(try std.fmt.allocPrint(allocator, "id={s}", .{idFromPath(opts.path)}));
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
        assert(global_pandoc_proc == null);
        global_pandoc_proc = std.process.Child.init(argv.slice(), allocator);
        global_pandoc_proc_set.store(true, .release);
        var proc = &global_pandoc_proc.?;
        if (opts.input == null) proc.stdin_behavior = .Pipe;
        if (opts.output == null) proc.stdout_behavior = .Pipe;
        try proc.spawn();
        return Pandoc{
            .stdin = if (opts.input == null) std.io.bufferedWriter(proc.stdin.?.writer()) else null,
            .stdout = if (opts.input == null) std.io.bufferedReader(proc.stdout.?.reader()) else null,
        };
    }

    fn getProc(self: *Pandoc) *std.process.Child {
        _ = self;
        return &global_pandoc_proc.?;
    }

    // Waits for the process to finish.
    fn wait(self: *Pandoc) !void {
        try self.closeStdin();
        const term = try self.getProc().wait();
        if (term == .Exited and term.Exited == 0) return;
        switch (term) {
            inline else => |code, tag| {
                try std.io.getStdErr().writer().print(
                    "{}: {s}: {}\n",
                    .{ formatArgv(self.getProc().argv), @tagName(tag), code },
                );
                return error.ProcessFailed;
            },
        }
    }

    // Post-processes HTML output and writes to the given path, then waits for exit.
    fn finish(self: *Pandoc, allocator: Allocator, output: []const u8) !void {
        try self.closeStdin();
        const file = try std.fs.cwd().createFile(output, .{});
        defer file.close();
        var writer = std.io.bufferedWriter(file.writer());
        try postprocessHtml(allocator, self.stdout.?.reader(), writer.writer());
        try writer.flush();
        try self.wait();
    }

    fn closeStdin(self: *Pandoc) !void {
        if (self.stdin) |*stdin| {
            try stdin.flush();
            self.stdin = null;
        }
        const proc = self.getProc();
        if (proc.stdin) |*stdin| {
            stdin.close();
            proc.stdin = null;
        }
    }

    fn kill() void {
        if (global_pandoc_proc_set.load(.acquire)) _ = global_pandoc_proc.?.kill() catch {};
    }
};

fn killPandocOnSignals(signals: []const u6) void {
    const action = std.posix.Sigaction{
        .handler = .{ .handler = signalHandler },
        .mask = std.posix.empty_sigset,
        .flags = 0,
    };
    for (signals) |signal| {
        var old: std.posix.Sigaction = undefined;
        std.posix.sigaction(signal, null, &old);
        if (old.handler.handler == std.posix.SIG.IGN) continue;
        std.posix.sigaction(signal, &action, null);
    }
}

export fn signalHandler(signum: i32) void {
    Pandoc.kill();
    const action = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.empty_sigset,
        .flags = 0,
    };
    std.posix.sigaction(@intCast(signum), &action, null);
    std.posix.raise(@intCast(signum)) catch {};
}

fn idFromPath(dest: []const u8) []const u8 {
    return dest[mem.indexOfScalar(u8, dest, '/').? + 1 .. mem.lastIndexOfScalar(u8, dest, '.').?];
}

fn postprocessHtml(allocator: Allocator, reader: anytype, writer: anytype) !void {
    var buffer = std.ArrayListUnmanaged(u8){};
    while (true) {
        buffer.clearRetainingCapacity();
        reader.streamUntilDelimiter(buffer.writer(allocator), '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        const line = buffer.items;
        const e2 = 0xe2;
        comptime assert("–"[0] == e2 and "”"[0] == e2);
        const close_code = "</code>";
        const close_em = "</em>";
        const spaced_en_dash = " – ";
        var i: usize = 0;
        while (mem.indexOfAnyPos(u8, line, i, &.{ '/', e2 })) |j| i = blk: {
            switch (line[j]) {
                '/' => if (j >= 1 and mem.startsWith(u8, line[j - 1 ..], close_code)) {
                    try writer.writeAll(line[i .. j - 1]);
                    try writer.writeAll(close_code);
                    // For words partially made up of code, like `cons`ing.
                    if (i < line.len and std.ascii.isAlphabetic(line[i])) try writer.writeAll("&hairsp;");
                    break :blk j - 1 + close_code.len;
                } else if (j >= 1 and mem.startsWith(u8, line[j - 1 ..], close_em)) {
                    try writer.writeAll(line[i .. j - 1]);
                    try writer.writeAll(close_em);
                    if (i < line.len) switch (line[i]) {
                        ':', ';' => switch (line[i - 1]) {
                            'd', 'r', 't' => try writer.writeAll("&hairsp;"),
                            else => {},
                        },
                        else => {},
                    };
                    break :blk j - 1 + close_em.len;
                },
                e2 => if (j >= 1 and mem.startsWith(u8, line[j - 1 ..], spaced_en_dash)) {
                    try writer.writeAll(line[i .. j - 1]);
                    // Replace an open-set en dash with a closed-set em dash.
                    try writer.writeAll("—");
                    break :blk j - 1 + spaced_en_dash.len;
                } else if (mem.startsWith(u8, line[j..], "”") and j + "”".len < line.len) switch (line[j + "”".len]) {
                    '.', ',' => |ch| {
                        try writer.writeByte(ch);
                        try writer.writeAll("<span class=\"tuck\">”</span>");
                        break :blk j + "”.".len;
                    },
                    else => {},
                },
                else => unreachable,
            }
            try writer.writeAll(line[i .. j + 1]);
            break :blk j + 1;
        };
        try writer.writeAll(line[i..]);
        try writer.writeByte('\n');
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
        try if (mem.indexOfAny(u8, arg, " '\"") == null)
            writer.writeAll(arg)
        else
            writer.print("\"{}\"", .{std.zig.fmtEscapes(arg)});
    }
}

// Represents a counter for each hierarchical level in a single integer.
fn Position(comptime Level: type) type {
    return struct {
        value: u64 = 0,
        const Self = @This();
        fn get(self: Self, level: Level) usize {
            const i = @intFromEnum(level) - 1;
            return (self.value >> @intCast(i * 8)) & 0xff;
        }
        fn increment(self: *Self, level: Level) void {
            const i = @intFromEnum(level) - 1;
            // TODO(https://github.com/ziglang/zig/issues/6903): Remove?
            const one: u64 = 1;
            const mask = (one << @intCast((i + 1) * 8)) - 1;
            const inc = one << @intCast(i * 8);
            self.value = (self.value & mask) + inc;
        }
    };
}

// Markdown line scanner.
const MarkdownScanner = struct {
    file: std.fs.File,
    eof: bool = false,
    reader: std.io.BufferedReader(4096, std.fs.File.Reader),
    line: std.BoundedArray(u8, 1024) = .{},

    // True if we are inside a fenced code block.
    code: bool = false,
    // Set if the current line is a heading.
    heading: ?struct { level: Level, value: Heading } = null,
    // Current position within the document.
    position: Position(Level) = .{},

    const Level = enum(u8) { h1 = 1, h2, h3, h4 };

    fn init(path: []const u8) !MarkdownScanner {
        const file = try std.fs.cwd().openFile(path, .{});
        return .{ .file = file, .reader = std.io.bufferedReader(file.reader()) };
    }

    fn deinit(self: MarkdownScanner) void {
        self.file.close();
    }

    fn get(self: *MarkdownScanner) []const u8 {
        return self.line.slice();
    }

    fn scan(self: *MarkdownScanner) !bool {
        try self.next();
        return !self.eof;
    }

    fn next(self: *MarkdownScanner) !void {
        assert(!self.eof);
        const prev_blank = self.line.len <= 1;
        self.heading = null;
        self.line.clear();
        if (!try readLine(self.reader.reader(), self.line.writer())) {
            self.eof = true;
            return;
        }
        const line = self.get();
        if (mem.startsWith(u8, line, "```")) {
            self.code = !self.code;
        } else if (!self.code and prev_blank) if (mem.indexOfNone(u8, line, "#")) |i| if (i > 0 and line[i] == ' ') {
            const level: Level = @enumFromInt(i);
            const value = blk: {
                const start = i + 1;
                var j = start;
                if (std.ascii.isDigit(line[j])) {
                    while (j < line.len and line[j] != ':') j += 1;
                    if (j + 2 < line.len and line[j + 1] == ' ')
                        break :blk Heading{ .label = line[start..j], .title = line[j + 2 .. line.len - 1] };
                }
                break :blk Heading{ .title = line[start .. line.len - 1] };
            };
            self.heading = .{ .level = level, .value = value };
            self.position.increment(level);
        };
    }
};

// Table of contents renderer.
const TocRenderer = struct {
    // Nesting depth of <ul> tags.
    depth: u8 = 0,

    fn renderStart(self: *TocRenderer, writer: anytype) !void {
        assert(self.depth == 0);
        try renderHeading(writer, .h2, "contents", Heading{ .title = "Contents" });
        try writer.writeAll("<nav aria-labelledby=\"contents\">");
    }

    fn renderEnd(self: *TocRenderer, writer: anytype) !void {
        assert(self.depth >= 1);
        for (1..self.depth) |_| try writer.writeAll("</ul></li>");
        try writer.writeAll("</ul></nav>\n");
        self.* = undefined;
    }

    fn renderItem(self: *TocRenderer, writer: anytype, depth: u8, heading: Heading, comptime href_fmt: []const u8, href_args: anytype) !void {
        assert(depth >= 1);
        switch (@as(i16, @intCast(depth)) - self.depth) {
            0 => try writer.writeAll("</li>"),
            1 => try writer.writeAll("<ul class=\"toc\">"),
            2...std.math.maxInt(i16) => unreachable,
            std.math.minInt(i16)...-1 => |delta| for (0..@abs(delta)) |_| try writer.writeAll("</ul></li>"),
        }
        self.depth = depth;
        try writer.writeAll("<li class=\"toc__item\">");
        try writer.print("<span class=\"toc__label\">{s}</span>", .{heading.label orelse ""});
        // Put a space between <span> and <a> so that they don't run
        // together in alternative stylesheets like Safari Reader.
        try writer.writeByte(' ');
        try writer.print("<a href=\"" ++ href_fmt ++ "\">{s}</a>", href_args ++ .{heading.title.?});
    }
};

const HeadingLevel = enum(u8) { h1 = 1, h2, h3 };

const Heading = struct {
    title: ?[]const u8,
    label: ?[]const u8 = null,
};

// Like renderHeadingHref but with no href for the title.
fn renderHeading(writer: anytype, level: HeadingLevel, id: ?[]const u8, heading: Heading) !void {
    return renderHeadingHref(writer, level, id, heading, null, .{});
}

// Renders a heading to writer. If href_fmt is present, renders heading.title as a link.
fn renderHeadingHref(writer: anytype, level: HeadingLevel, id: ?[]const u8, heading: Heading, comptime href_fmt: ?[]const u8, href_args: anytype) !void {
    if (id) |the_id| {
        try writer.print(
            \\<h{0} id="{1s}" class="anchor">
            ++
            \\<a class="anchor__link link" href="#{1s}" aria-hidden="true">#</a>
        , .{ @intFromEnum(level), the_id });
        try writer.writeByte(' ');
    } else {
        try writer.print("<h{}>", .{@intFromEnum(level)});
    }
    if (heading.label) |label| {
        const big = if (level == .h1 and label.len == 1) " number--big" else "";
        try writer.print("<span class=\"number{s}\">{s}</span>", .{ big, label });
        try writer.writeByte(' ');
    }
    if (href_fmt) |fmt| {
        try writer.print(
            \\<a class="link" href="
        ++ fmt ++
            \\">{s}<span class="nowrap">&NoBreak;
        , href_args ++ .{heading.title.?});
        try writer.writeAll(
            \\<svg class="external" width="24" height="24" aria-hidden="true">
            ++
            \\<use xlink:href="#external"/></svg></span></a>
        );
    } else {
        try writer.writeAll(heading.title.?);
    }
    try writer.print("</h{}>\n", .{@intFromEnum(level)});
}

// Reads an entire line, including the newline. Returns false on EOF.
fn readLine(reader: anytype, writer: anytype) !bool {
    reader.streamUntilDelimiter(writer, '\n', null) catch |err| switch (err) {
        error.EndOfStream => return false,
        else => return err,
    };
    try writer.writeByte('\n');
    return true;
}

fn formatLower(str: []const u8) std.fmt.Formatter(formatLowerFn) {
    return .{ .data = str };
}

fn formatLowerFn(str: []const u8, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = f;
    _ = options;
    for (str) |c| try writer.writeByte(std.ascii.toLower(c));
}
