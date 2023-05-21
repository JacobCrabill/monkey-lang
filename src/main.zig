const std = @import("std");
const Lexer = @import("lexer.zig");

pub fn main() !void {
    std.debug.print("Hello, World!\n", .{});

    try repl();
}

pub fn repl() !void {
    // TODO
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [1048]u8 = undefined;

    while (true) {
        try stdout.print(">> ", .{});
        const input = (try nextLine(stdin, &buffer)).?;
        var lex = Lexer.Lexer.init(input);

        var tok = lex.nextToken();
        while (tok.kind != .EOF) {
            try stdout.print("{any}\n", .{tok});

            // Allow the user to exit without ctrl-c
            if (tok.kind == .EXIT) {
                return;
            }

            tok = lex.nextToken();
        }
    }
}

fn nextLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}
