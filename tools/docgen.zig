// Copyright 2024 Mitchell Kember. Subject to the MIT License.

const std = @import("std");

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
    const dirname = std.fs.path.dirname(arg1);
    std.fs.cwd().makeDir(dirname) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.io.getStdErr().writer().print("{s}: {s}", .{ dirname, @errorName(err) });
            std.process.exit(1);
        },
    };
    gen(arg1);
}

fn gen(output: []const u8) void {
    if (std.mem.eql(u8, output, "docs/index.html")) return genIndex(output);
    if (std.mem.startsWith(u8, output, "docs/text")) {
        //
    }
}

fn genIndex(output: []const u8) void {
    _ = output; // autofix
}
