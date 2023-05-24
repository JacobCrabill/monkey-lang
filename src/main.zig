const std = @import("std");
const Lexer = @import("lexer.zig");
const Repl = @import("repl.zig");

const GPA = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPA{};
    var alloc = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var repl = Repl.makeRepl(alloc, stdin, stdout);
    try repl.start();
}
