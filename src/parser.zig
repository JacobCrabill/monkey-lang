const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Map = std.AutoArrayHashMap;
const WriteError = std.os.WriteError;

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
    SUM, // + or -
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // function()
};

pub fn precedenceMap(token_type: TokenType) Operators {
    return switch (token_type) {
        .EQ => .EQUALS,
        .NEQ => .EQUALS,
        .LT => .LESSGREATER,
        .GT => .LESSGREATER,
        .PLUS => .SUM,
        .MINUS => .SUM,
        .SLASH => .PRODUCT,
        .STAR => .PRODUCT,
        else => .LOWEST,
    };
}

pub fn isOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .PLUS => true,
        .MINUS => true,
        .SLASH => true,
        .STAR => true,
        .EQ => true,
        .NEQ => true,
        .LT => true,
        .GT => true,
        else => false,
    };
}

const Parser = struct {
    const Self = @This();
    alloc: Allocator,
    lexer: *Lexer.Lexer,
    cur_token: Token = undefined,
    next_token: Token = undefined,
    errors: ArrayList([]const u8),
    expressions: ArrayList(ast.Expression),

    // Create and initialize a new Parser
    pub fn init(alloc: Allocator, lexer: *Lexer.Lexer) Self {
        var parser = Parser{
            .alloc = alloc,
            .lexer = lexer,
            .errors = ArrayList([]const u8).init(alloc),
            .expressions = ArrayList(ast.Expression).init(alloc),
        };
        parser.nextToken();
        parser.nextToken();
        return parser;
    }

    /// Release all resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        self.expressions.deinit();
    }

    /// Check if the current token is the given type
    pub fn curTokenIs(self: Self, kind: TokenType) bool {
        return self.cur_token.kind == kind;
    }

    /// Check if the next token is of type 'kind'
    pub fn nextTokenIs(self: Self, kind: TokenType) bool {
        return self.next_token.kind == kind;
    }

    /// Check the operator precedence of the next token
    pub fn peekPrecedence(self: Self) Operators {
        return precedenceMap(self.next_token.kind);
    }

    /// Check the operator precedence of the current token
    pub fn curPrecedence(self: Self) Operators {
        return precedenceMap(self.cur_token.kind);
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

    fn parseExpression(self: *Self, precedence: Operators) ?ast.Expression {
        const iprec: u8 = @enumToInt(precedence);

        // Try parsing the operator in the prefix position, and return the result
        var left: ast.Expression = self.parsePrefix(self.cur_token.kind) orelse return null;

        while (!(self.curTokenIs(.SEMI) or self.curTokenIs(.EOF)) and iprec < @enumToInt(self.peekPrecedence())) {
            if (isOperator(self.next_token.kind)) {
                self.nextToken();
                // Copy 'left' onto the heap; make the next InfixExpression the new 'left'
                var pleft: *ast.Expression = self.expressions.addOne() catch unreachable;
                pleft.* = left;
                left = self.parseInfixExpression(pleft);
            } else {
                break;
            }
        }

        return left;
    }

    // -------- Expressions --------

    fn parsePrefix(self: *Self, kind: TokenType) ?ast.Expression {
        return switch (kind) {
            .IDENT => self.parseIdentifier(),
            .INT => self.parseIntegerLiteral(),
            .BANG => self.parsePrefixExpression(),
            .MINUS => self.parsePrefixExpression(),
            .TRUE => self.parseBoolean(),
            .FALSE => self.parseBoolean(),
            .LPAREN => self.parseGroupedExpression(),
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

        if (self.parseExpression(.PREFIX)) |pfx_expr| {
            // Allocate, because pointer
            expr.createRight() catch unreachable;
            expr.right.?.* = pfx_expr;
        }

        return ast.Expression{ .prefix_expr = expr };
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) ast.Expression {
        var expr = ast.InfixExpression{
            .alloc = self.alloc,
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .left = left,
            .right = null,
        };

        const precedence = self.curPrecedence();
        self.nextToken();

        if (self.parseExpression(precedence)) |ifx_expr| {
            // Allocate, because pointer
            expr.createRight() catch unreachable;
            expr.right.?.* = ifx_expr;
        } else {
            // TODO: make parser error
            std.debug.print("Could not parse right expression for {any}\n", .{left.*});
        }

        return ast.Expression{ .infix_expr = expr };
    }

    fn parseGroupedExpression(self: *Self) ast.Expression {
        self.nextToken();

        // TODO: Error handling!
        var exp = self.parseExpression(.LOWEST).?;

        if (!self.expectPeek(.RPAREN)) {
            @panic("Invalid grouped expression!");
        }

        return exp;
    }

    // -------- Identifiers and Literals --------

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

    fn parseBoolean(self: *Self) ast.Expression {
        return ast.Expression{
            .boolean_literal = ast.BooleanLiteral{
                .token = self.cur_token,
                .value = self.curTokenIs(.TRUE),
            },
        };
    }
};

// --------------------------------- Tests ---------------------------------

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

    try std.testing.expect(prog.statements.items.len == expected.len);

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

test "infix expressions" {
    const input =
        \\1 + 5;
        \\1 - 5;
        \\1 * 5;
        \\1 / 5;
        \\1 > 5;
        \\1 < 5;
        \\1 == 5;
        \\1 != 5;
    ;

    const Result = struct {
        const Self = @This();
        input: []const u8,
        lval: i64,
        operator: []const u8,
        rval: i64,
        pub fn init(in: []const u8, lval: i64, op: []const u8, rval: i64) Self {
            return .{
                .input = in,
                .lval = lval,
                .operator = op,
                .rval = rval,
            };
        }
    };
    const expected = [_]Result{
        Result.init("1 + 5;", 1, "+", 5),
        Result.init("1 - 5;", 1, "-", 5),
        Result.init("1 * 5;", 1, "*", 5),
        Result.init("1 / 5;", 1, "/", 5),
        Result.init("1 > 5;", 1, ">", 5),
        Result.init("1 < 5;", 1, "<", 5),
        Result.init("1 == 5;", 1, "==", 5),
        Result.init("1 != 5;", 1, "!=", 5),
    };

    // TODO: Construct expected AST, write AST comparison fn

    var lex = Lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);
    defer parser.deinit();

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);
    try prog.print(std.io.getStdErr().writer());

    try std.testing.expect(prog.statements.items.len == expected.len);

    for (prog.statements.items, expected) |stmt, res| {
        const expr: ast.Expression = stmt.expression_statement.value.?;
        const lval = expr.infix_expr.left.?.integer_literal.value;
        const rval = expr.infix_expr.right.?.integer_literal.value;

        try std.testing.expect(lval == res.lval);
        try std.testing.expect(rval == res.rval);
    }
}

const TestData = struct {
    input: []const u8 = undefined,
    output: []const u8 = undefined,
};

fn testProgram(test_data: []const TestData, comptime buf_size: usize) !void {
    for (test_data) |data| {
        var buf: [buf_size]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        const stream = fbs.writer();
        const input = data.input;
        const output = data.output;

        // Process the input
        var lex = Lexer.Lexer.init(input);
        var parser = Parser.init(std.testing.allocator, &lex);
        defer parser.deinit();

        var prog: Program = try parser.parseProgram();
        defer prog.deinit();

        // Check the output
        try checkParseErrors(parser);
        try prog.print(stream);

        try std.testing.expectEqualSlices(u8, output, fbs.getWritten());
    }
}

test "operator precedence" {
    const test_data = [_]TestData{
        .{ .input = "-a * b", .output = "((-a) * b);\n" },
        .{ .input = "!-a", .output = "(!(-a));\n" },
        .{ .input = "!-a", .output = "(!(-a));\n" },
        .{ .input = "a + b + c", .output = "((a + b) + c);\n" },
        .{ .input = "a + b - c", .output = "((a + b) - c);\n" },
        .{ .input = "a * b * c", .output = "((a * b) * c);\n" },
        .{ .input = "a * b / c", .output = "((a * b) / c);\n" },
        .{ .input = "a + b / c", .output = "(a + (b / c));\n" },
        .{ .input = "a + b * c + d / e - f", .output = "(((a + (b * c)) + (d / e)) - f);\n" },
        .{ .input = "3 + 4 * -5 * 5", .output = "(3 + ((4 * (-5)) * 5));\n" },
        .{ .input = "5 > 4 == 3 < 4", .output = "((5 > 4) == (3 < 4));\n" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .output = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n" },
        .{ .input = "(3 + 4) * 5 == 3 * 1 + 4 * 5", .output = "(((3 + 4) * 5) == ((3 * 1) + (4 * 5)));\n" },
    };

    try testProgram(&test_data, 256);
}

test "boolean literals" {
    const test_data = [_]TestData{
        .{ .input = "true;", .output = "true;\n" },
        .{ .input = "false;", .output = "false;\n" },
        .{ .input = "let foobar = true;", .output = "let foobar = true;\n" },
        .{ .input = "let barfoo = false;", .output = "let barfoo = false;\n" },
    };

    try testProgram(&test_data, 256);
}
