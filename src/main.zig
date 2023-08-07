const std = @import("std");
const Lexer = @import("lexer.zig");
const Repl = @import("repl.zig");

const GPA = std.heap.GeneralPurposeAllocator;
const DebugGPA = GPA(.{ .safety = true, .retain_metadata = true });

pub fn main() !void {
    var gpa = DebugGPA{};
    var alloc = gpa.allocator();
    //var alloc = std.heap.c_allocator;

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var repl = Repl.makeRepl(alloc, stdin, stdout);
    try repl.start();
}
