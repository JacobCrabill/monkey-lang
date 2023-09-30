const std = @import("std");
const Lexer = @import("lexer.zig");
const Repl = @import("repl.zig");

const File = std.fs.File;
const GPA = std.heap.GeneralPurposeAllocator;
const os = std.os;
const DebugGPA = GPA(.{ .safety = true, .retain_metadata = true });

pub fn main() !void {
    var gpa = DebugGPA{};
    var alloc = gpa.allocator();

    // Get command-line arguments
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var repl = Repl.makeRepl(alloc, stdin, stdout);

    if (args.len < 2) {
        // No args; jump into REPL
        try repl.start();
    } else {
        // Assume first arg is a .mon file to run

        // Read file into memory
        var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        var realpath = try std.fs.realpath(args[1], &path_buf);
        var md_file: File = try std.fs.openFileAbsolute(realpath, .{});
        var md_text = try md_file.readToEndAlloc(alloc, 1e9);

        try repl.runOnce(md_text);
    }
}
