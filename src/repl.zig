const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Program = @import("ast.zig").Program;
const Eval = @import("evaluator.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn makeRepl(alloc: Allocator, input: anytype, output: anytype) Repl(@TypeOf(input), @TypeOf(output)) {
    return Repl(@TypeOf(input), @TypeOf(output)).init(alloc, input, output);
}

pub fn Repl(comptime InStream: type, comptime OutStream: type) type {
    return struct {
        const Self = @This();
        alloc: Allocator,
        input: InStream,
        output: OutStream,
        max_input_size: usize = 65535,
        commands: ArrayList([]const u8),
        should_exit: bool = false,

        // Create
        pub fn init(alloc: Allocator, input: InStream, output: OutStream) Self {
            return Self{
                .alloc = alloc,
                .input = input,
                .output = output,
                .commands = ArrayList([]const u8).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.commands.items) |*str| {
                self.alloc.free(str.*);
            }
            self.commands.deinit();
        }

        // Run the REPL
        pub fn start(self: *Self) !void {
            try self.output.print("Welcome to Monkey!\n", .{});
            try self.output.print("  version: 0.1.0\n", .{});

            var evaluator = Eval.Evaluator.init(self.alloc);
            defer evaluator.deinit();

            while (!self.should_exit) {
                try self.output.print(">> ", .{});
                const input = (try self.nextLine()).?;

                // Check for special commands handled by the REPL
                if (self.handleInput(input))
                    continue;

                var lex = Lexer.init(input);
                var parser = Parser.init(self.alloc, &lex);
                defer parser.deinit();

                // Parse and print the statement(s)
                var prog: Program = try parser.parseProgram();
                defer prog.deinit();

                //const result = evaluator.evalProgram(prog);
                //try result.print(self.output);
                try self.output.print("\n", .{});
            }
        }

        fn nextLine(self: *Self) !?[]const u8 {
            var line = (try self.input.readUntilDelimiterOrEofAlloc(
                self.alloc,
                '\n',
                self.max_input_size,
            )) orelse return null;

            // trim annoying windows-only carriage return character
            if (@import("builtin").os.tag == .windows) {
                return std.mem.trimRight(u8, line, "\r");
            } else {
                return line;
            }
        }

        fn handleInput(self: *Self, input: []const u8) bool {
            if (std.mem.startsWith(u8, input, "exit") or std.mem.startsWith(u8, input, "quit")) {
                self.should_exit = true;
                return true;
            }
            return false;
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
