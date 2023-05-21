const std = @import("std");
const Lexer = @import("lexer.zig");
const Repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var repl = Repl.makeRepl(stdin, stdout);
    try repl.start();
}
