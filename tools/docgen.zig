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
    const arg1 = std.mem.span(std.os.argv[1]);
    if (std.mem.eql(u8, arg1, "-h") or std.mem.eql(u8, arg1, "--help")) {
        printUsage(std.io.getStdOut());
        return;
    }
    if (std.fs.path.dirname(arg1)) |dirname| std.fs.cwd().makeDir(dirname) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.io.getStdErr().writer().print("{s}: {s}", .{ dirname, @errorName(err) }) catch unreachable;
            std.process.exit(1);
        },
    };
    gen(arg1) catch |err| {
        std.io.getStdErr().writer().print("{s}: {s}", .{ arg1, @errorName(err) }) catch unreachable;
        std.process.exit(1);
    };
}

fn gen(output: [:0]const u8) !void {
    if (std.mem.eql(u8, output, "docs/index.html")) return genIndex(output);
    if (std.mem.startsWith(u8, output, "docs/text")) {
        //
    }
}

// Generates docs/index.html.
fn genIndex(output: [:0]const u8) !void {
    try pandoc(.{
        .input = "notes/index.md",
        .output = output,
        .dest = output,
        .title = "SICP Study",
    });
}

// Generates docs/text/index.html.
fn genTextIndex(output: [:0]const u8) !void {
    _ = output; // autofix
    //
}

// Options for invoking pandoc.
const PandocOpts = struct {
    // Path to the input file.
    input: [:0]const u8,
    // Path to the output file.
    output: [:0]const u8,
    // Destination path. Usually output is "/dev/stdout" so that docgen can do
    // further post-processing, while dest is the actual HTML path. It is only
    // used for construct the -M id=... parameter; the file is not opened.
    dest: []const u8,
    // Contents of <title>...</title>.
    title: []const u8,
    // Links to up/prev/next page.
    navigation: ?struct {
        up: []const u8,
        prev: []const u8,
        next: []const u8,
    } = null,
};

// Invokes pandoc, printing the command to stderr before executing it. Normally
// does not return since it replaces the current process. Returns an error if
// pandoc is not found in PATH.
fn pandoc(opts: PandocOpts) !void {
    // Use a stack buffer since heap allocations between fork and exec are illegal.
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();
    var argv = std.ArrayList(?[*:0]const u8).init(allocator);
    try argv.append("pandoc");
    try argv.append("-o");
    try argv.append(opts.output);
    try argv.append("-dpandoc/config.yml");
    try argv.append("-M");
    try argv.append(try std.fmt.allocPrintZ(allocator, "id={s}", .{idFromDestPath(opts.dest)}));
    try argv.append("-M");
    const title_index = argv.items.len;
    try argv.append(try std.fmt.allocPrintZ(allocator, "title={s}", .{opts.title}));
    if (opts.navigation) |navigation| {
        try argv.append("-M");
        try argv.append(try std.fmt.allocPrintZ(allocator, "up={s}", .{navigation.up}));
        try argv.append("-M");
        try argv.append(try std.fmt.allocPrintZ(allocator, "prev={s}", .{navigation.prev}));
        try argv.append("-M");
        try argv.append(try std.fmt.allocPrintZ(allocator, "next={s}", .{navigation.next}));
    }
    try argv.append(opts.input);
    const stderr = std.io.getStdErr().writer();
    for (argv.items, 0..) |item, i| {
        const space = if (i != 0) " " else "";
        // Quote the title argument since it has spaces.
        const quote = if (i == title_index) "'" else "";
        try stderr.print("{s}{s}{s}{s}", .{ space, quote, item.?, quote });
    }
    try stderr.writeByte('\n');
    // This cast is copied from std.process.execve.
    const envp = @as([*:null]const ?[*:0]const u8, @ptrCast(std.os.environ.ptr));
    return std.posix.execvpeZ("pandoc", try argv.toOwnedSliceSentinel(null), envp);
}

fn idFromDestPath(dest: []const u8) []const u8 {
    return dest[std.mem.indexOfScalar(u8, dest, '/').? + 1 .. std.mem.lastIndexOfScalar(u8, dest, '.').?];
}

test {
    _ = std.testing.refAllDecls(@This());
}
