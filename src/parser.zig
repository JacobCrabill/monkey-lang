const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig");
const Tokens = @import("token.zig");

const Program = ast.Program;
const Statement = ast.Statement;
const LetStatement = ast.LetStatement;
const Token = Tokens.Token;
const TokenType = Tokens.TokenType;

const Parser = struct {
    const Self = @This();
    alloc: Allocator,
    lexer: *Lexer.Lexer,
    cur_token: Token = undefined,
    next_token: Token = undefined,
    errors: ArrayList([]const u8),

    // Create and initialize a new Parser
    pub fn init(alloc: Allocator, lexer: *Lexer.Lexer) Self {
        var parser = Parser{
            .alloc = alloc,
            .lexer = lexer,
            .errors = ArrayList([]const u8).init(alloc),
        };
        parser.nextToken();
        parser.nextToken();
        return parser;
    }

    /// Release all resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    /// Check if the current token is the given type
    pub fn curTokenIs(self: Self, kind: TokenType) bool {
        return self.cur_token.kind == kind;
    }

    /// Check if the next token is of type 'kind'
    pub fn nextTokenIs(self: Self, kind: TokenType) bool {
        return self.next_token.kind == kind;
    }

    /// Expect that the next token is of type 'kind'; advance token if so
    pub fn expectPeek(self: *Self, kind: TokenType) bool {
        if (self.nextTokenIs(kind)) {
            self.nextToken();
            return true;
        }

        self.peekError(kind);
        return false;
    }

    /// Advance the lexer by one token
    pub fn nextToken(self: *Self) void {
        self.cur_token = self.next_token;
        self.next_token = self.lexer.nextToken();
    }

    pub fn getErrors(self: Self) []const []const u8 {
        return self.errors.items;
    }

    pub fn peekError(self: *Self, kind: TokenType) void {
        const fmt = "Expected next token to be {any}, got {any} instead";
        const msg = std.fmt.allocPrint(self.alloc, fmt, .{ kind, self.cur_token.kind }) catch {
            @panic("ERROR: Cannot alloc for string format!");
        };
        self.errors.append(msg) catch {
            @panic("ERROR: Cannot alloc for new parser error!");
        };
    }

    /// Parse the tokens into a Program of statements
    pub fn parseProgram(self: *Self) !Program {
        var prog = Program.init(self.alloc);

        while (!self.curTokenIs(.EOF)) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                try prog.append(s);
            }
            self.nextToken();
        }

        return prog;
    }

    /// Parse a single statement
    pub fn parseStatement(self: *Self) ?Statement {
        switch (self.cur_token.kind) {
            .LET => {
                const let_statement = self.parseLetStatement();
                if (let_statement) |s| {
                    return Statement{ .let_statement = s };
                } else {
                    return null;
                }
            },
            else => return null,
        }
    }

    pub fn parseLetStatement(self: *Self) ?LetStatement {
        var ls = LetStatement{
            .token = self.cur_token,
            .ident = undefined,
            .value = undefined,
        };

        if (!self.expectPeek(.IDENT))
            return null;

        ls.ident = self.cur_token;

        if (!self.expectPeek(.EQUAL))
            return null;

        // TODO: We're continuing until the semicolon (or EOF)
        // TODO: No expressions (yet....)
        while (!(self.curTokenIs(.SEMI) or self.curTokenIs(.EOF))) {
            self.nextToken();
        }

        return ls;
    }
};

const TestError = error{
    CompareFailed,
    ParserError,
};

pub fn checkParseErrors(parser: Parser) TestError!void {
    if (parser.errors.items.len == 0)
        return;

    std.debug.print("[ERROR] Parser had errors:\n", .{});
    for (parser.errors.items) |err| {
        std.debug.print("parser error: '{s}'\n", .{err});
    }

    return TestError.ParserError;
}

test "let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 3);

    const expected_idents = [_]ast.Identifier{
        ast.Identifier.init(.IDENT, "x"),
        ast.Identifier.init(.IDENT, "y"),
        ast.Identifier.init(.IDENT, "foobar"),
    };

    for (expected_idents, 0..) |ident, i| {
        const statement = prog.statements.items[i];
        var ls = statement.let_statement;
        const literal = ls.ident.literal;

        // Compare token literal
        std.testing.expect(std.mem.eql(u8, ident.value, literal)) catch {
            std.debug.print("Expected: {s}, got: {s}\n", .{ ident.value, literal });
            return TestError.CompareFailed;
        };
    }
}
