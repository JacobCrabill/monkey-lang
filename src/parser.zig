const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig");
const Tokens = @import("token.zig");

const Program = ast.Program;
const Statement = ast.Statement;
const ST = ast.StatementType;
const Token = Tokens.Token;
const TokenType = Tokens.TokenType;
const LetStatement = ast.LetStatement;
const ReturnStatement = ast.ReturnStatement;
const ExpressionStatement = ast.ExpressionStatement;

const Operators = enum(u8) {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // function()
};

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
        return switch (self.cur_token.kind) {
            .LET => self.parseLetStatement(),
            .RETURN => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseLetStatement(self: *Self) ?Statement {
        var ls = LetStatement{
            .token = self.cur_token,
            .ident = Token{},
            .value = null,
        };

        if (!self.expectPeek(.IDENT))
            return null;

        ls.ident = self.cur_token;

        if (!self.expectPeek(.EQUAL))
            return null;

        self.nextToken();

        ls.value = self.parseExpression(.LOWEST);

        // TODO: We're continuing until the semicolon (or EOF)
        // TODO: No expressions (yet....)
        while (!(self.curTokenIs(.SEMI) or self.curTokenIs(.EOF))) {
            self.nextToken();
        }

        return Statement{ .let_statement = ls };
    }

    pub fn parseReturnStatement(self: *Self) ?Statement {
        var rs = ReturnStatement{
            .token = self.cur_token,
            .value = null,
        };

        self.nextToken();

        rs.value = self.parseExpression(.LOWEST);

        // TODO: Parse expression
        while (!(self.curTokenIs(.SEMI) or self.curTokenIs(.EOF)))
            self.nextToken();

        return Statement{ .return_statement = rs };
    }

    pub fn parseExpressionStatement(self: *Self) ?Statement {
        var es = ExpressionStatement{
            .token = self.cur_token,
            .value = null,
        };

        es.value = self.parseExpression(.LOWEST);

        if (self.nextTokenIs(.SEMI))
            self.nextToken();

        return Statement{ .expression_statement = es };
    }

    fn parseExpression(self: *Self, _: Operators) ?ast.Expression {
        // Try parsing the operator in the prefix position, and return the result
        if (self.parsePrefix(self.cur_token.kind)) |expr| {
            return expr;
        }
        return null;
    }

    fn parsePrefix(self: *Self, kind: TokenType) ?ast.Expression {
        return switch (kind) {
            .IDENT => self.parseIdentifier(),
            .INT => self.parseIntegerLiteral(),
            .BANG => self.parsePrefixExpression(),
            .MINUS => self.parsePrefixExpression(),
            else => null, // TODO: append error "no prefix parser for {s}"
        };
    }

    fn parsePrefixExpression(self: *Self) ast.Expression {
        var expr = ast.PrefixExpression{
            .alloc = self.alloc,
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .right = null,
        };

        self.nextToken();

        // Allocate, because pointer
        if (self.parseExpression(.PREFIX)) |pfx_expr| {
            expr.createRight() catch unreachable;
            expr.right.?.* = pfx_expr;
        }

        return ast.Expression{ .prefix_expr = expr };
    }

    fn parseIdentifier(self: *Self) ast.Expression {
        const ident = ast.Identifier.init(self.cur_token.kind, self.cur_token.literal);
        return ast.Expression{ .identifier = ident };
    }

    fn parseIntegerLiteral(self: *Self) ast.Expression {
        // TODO: Catch error and append message to errors
        // 'Could not parse {s} as integer"
        const lit = ast.IntegerLiteral.init(self.cur_token);
        return ast.Expression{ .integer_literal = lit };
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

        try std.testing.expect(ls.token.kind == .LET);

        // Compare token literal
        std.testing.expect(std.mem.eql(u8, ident.value, literal)) catch {
            std.debug.print("Expected: {s}, got: {s}\n", .{ ident.value, literal });
            return TestError.CompareFailed;
        };
    }
}

test "return statements" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 3);

    for (prog.statements.items) |statement| {
        var rs = statement.return_statement;
        try std.testing.expect(rs.token.kind == .RETURN);
    }
}

test "print ast" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\return 993322;
    ;

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 3);

    std.debug.print("Program AST:\n", .{});
    std.debug.print("------------\n", .{});
    try prog.print(std.io.getStdErr().writer());
    std.debug.print("------------\n", .{});
}

test "identifier expresssion" {
    const input = "foobar;";

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 1);

    const stmt: Statement = prog.statements.items[0];
    const expr: ExpressionStatement = stmt.expression_statement;
    try std.testing.expect(expr.token.kind == .IDENT);
    try std.testing.expect(std.mem.eql(u8, expr.token.literal, "foobar"));
    //try std.testing.expect(std.mem.eql(u8, expr.value.literal, "foobar"));
}

test "prefix expressions" {
    const input =
        \\!5;
        \\-15;
    ;

    const Result = struct {
        kind: TokenType,
        operator: []const u8,
        value: i64,
    };
    const expected = [_]Result{
        .{
            .kind = .BANG,
            .operator = "!",
            .value = 5,
        },
        .{
            .kind = .MINUS,
            .operator = "-",
            .value = 15,
        },
    };

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);
    try prog.print(std.io.getStdErr().writer());

    try std.testing.expect(prog.statements.items.len == 2);

    for (prog.statements.items, expected) |stmt, res| {
        const expr_s: ExpressionStatement = stmt.expression_statement;
        const expr: ast.Expression = expr_s.value.?;
        const pfx: ast.PrefixExpression = expr.prefix_expr;
        try std.testing.expect(pfx.token.kind == res.kind);
        try std.testing.expect(std.mem.eql(u8, pfx.operator, res.operator));

        const integer_literal: ast.IntegerLiteral = pfx.right.?.integer_literal;
        try std.testing.expect(integer_literal.value == res.value);
    }
}
