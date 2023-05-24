const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Program = @import("ast.zig").Program;

const Allocator = std.mem.Allocator;

pub fn makeRepl(alloc: Allocator, input: anytype, output: anytype) Repl(@TypeOf(input), @TypeOf(output)) {
    return Repl(@TypeOf(input), @TypeOf(output)).init(alloc, input, output);
}

pub fn Repl(comptime InStream: type, comptime OutStream: type) type {
    return struct {
        const Self = @This();
        alloc: Allocator,
        input: InStream,
        output: OutStream,

        // Create
        pub fn init(alloc: Allocator, input: InStream, output: OutStream) Self {
            return Self{
                .alloc = alloc,
                .input = input,
                .output = output,
            };
        }

        // Run the REPL
        pub fn start(self: *Self) !void {
            try self.output.print("Welcome to Monkey!\n", .{});
            try self.output.print("  version: 0.0.1\n", .{});

            var buffer: [1048]u8 = undefined;

            while (true) {
                try self.output.print(">> ", .{});
                const input = (try self.nextLine(&buffer)).?;
                var lex = Lexer.init(input);
                var parser = Parser.init(self.alloc, &lex);
                defer parser.deinit();

                // Parse and print the statement(s)
                var prog: Program = try parser.parseProgram();
                defer prog.deinit();
                try prog.printStatements(self.output);

                // Print the tokens
                // var tok = lex.nextToken();
                // while (tok.kind != .EOF) {
                //     try self.output.print("{any}\n", .{tok});

                //     // Allow the user to exit without ctrl-c
                //     if (tok.kind == .EXIT) {
                //         return;
                //     }

                //     tok = lex.nextToken();
                // }
            }
        }

        fn nextLine(self: *Self, buffer: []u8) !?[]const u8 {
            var line = (try self.input.readUntilDelimiterOrEof(
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
    };
}

pub const MONKEY_FACE =
    \\            __,__
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\  \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./  |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
    \\
;
