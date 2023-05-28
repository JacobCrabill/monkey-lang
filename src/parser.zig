const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Map = std.AutoArrayHashMap;
const WriteError = std.os.WriteError;

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/expressions.zig");
    usingnamespace @import("ast/statements.zig");
};
const Lexer = @import("lexer.zig").Lexer;
const Tokens = @import("tokens.zig");

const Program = ast.Program;
const Statement = ast.Statement;
const ST = ast.StatementType;
const Token = Tokens.Token;
const TokenType = Tokens.TokenType;
const LetStatement = ast.LetStatement;
const ReturnStatement = ast.ReturnStatement;
const ExpressionStatement = ast.ExpressionStatement;

pub fn logSourceInfo(src: std.builtin.SourceLocation) void {
    std.debug.print("{s}:{d}:{d}: {s}\n", .{
        src.file,
        src.line,
        src.column,
        src.fn_name,
    });
}

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
        .LPAREN => .CALL,
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
        .LPAREN => true,
        else => false,
    };
}

pub const Parser = struct {
    const Self = @This();
    alloc: Allocator,
    lexer: *Lexer,
    cur_token: Token = undefined,
    next_token: Token = undefined,
    errors: ArrayList([]const u8),

    // Create and initialize a new Parser
    pub fn init(alloc: Allocator, lexer: *Lexer) Self {
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
    fn curTokenIs(self: Self, kind: TokenType) bool {
        return self.cur_token.kind == kind;
    }

    /// Check if the next token is of type 'kind'
    fn nextTokenIs(self: Self, kind: TokenType) bool {
        return self.next_token.kind == kind;
    }

    /// Check the operator precedence of the next token
    fn peekPrecedence(self: Self) Operators {
        return precedenceMap(self.next_token.kind);
    }

    /// Check the operator precedence of the current token
    fn curPrecedence(self: Self) Operators {
        return precedenceMap(self.cur_token.kind);
    }

    /// Expect that the next token is of type 'kind'; advance token if so
    fn expectPeek(self: *Self, kind: TokenType) bool {
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
        const msg = std.fmt.allocPrint(self.alloc, fmt, .{ kind, self.next_token.kind }) catch {
            @panic("ERROR: Cannot alloc for string format!");
        };
        self.errors.append(msg) catch {
            @panic("ERROR: Cannot alloc for new parser error!");
        };
    }

    fn makeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.alloc, fmt, args) catch {
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
    fn parseStatement(self: *Self) ?Statement {
        return switch (self.cur_token.kind) {
            .LET => self.parseLetStatement(),
            .RETURN => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseLetStatement(self: *Self) ?Statement {
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

        if (self.nextTokenIs(.SEMI))
            self.nextToken();

        return Statement{ .let_statement = ls };
    }

    fn parseReturnStatement(self: *Self) ?Statement {
        var rs = ReturnStatement{
            .token = self.cur_token,
            .value = null,
        };

        self.nextToken();

        rs.value = self.parseExpression(.LOWEST);

        while (self.nextTokenIs(.SEMI))
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

    fn parseBlockStatement(self: *Self) ?ast.BlockStatement {
        var block = ast.BlockStatement.init(self.alloc, self.cur_token);

        self.nextToken();

        while (!(self.curTokenIs(.RBRACE) or self.curTokenIs(.EOF))) {
            if (self.parseStatement()) |stmt| {
                block.statements.append(stmt) catch unreachable;
            }
            self.nextToken();
        }

        return block;
    }

    // -------- Expressions --------

    fn parseExpression(self: *Self, precedence: Operators) ?ast.Expression {
        const iprec: u8 = @enumToInt(precedence);

        // Try parsing the operator in the prefix position, and return the result
        var left: ast.Expression = self.parsePrefix(self.cur_token.kind) orelse return null;

        while (!(self.curTokenIs(.SEMI) or self.curTokenIs(.EOF)) and iprec < @enumToInt(self.peekPrecedence())) {
            if (isOperator(self.next_token.kind)) {
                self.nextToken();
                // Copy 'left' onto the heap; make the next InfixExpression the new 'left'
                var pleft: *ast.Expression = self.alloc.create(ast.Expression) catch unreachable;
                pleft.* = left;
                if (self.parseInfix(pleft)) |new_left| {
                    left = new_left;
                } else {
                    left.deinit();
                    self.makeError("Error parsing new Infix expression", .{});
                    logSourceInfo(@src());
                    return null;
                }
            } else {
                return left;
            }
        }

        return left;
    }

    fn parsePrefix(self: *Self, kind: TokenType) ?ast.Expression {
        return switch (kind) {
            .IDENT => self.parseIdentifier(),
            .INT => self.parseIntegerLiteral(),
            .BANG => self.parsePrefixExpression(),
            .MINUS => self.parsePrefixExpression(),
            .TRUE => self.parseBoolean(),
            .FALSE => self.parseBoolean(),
            .LPAREN => self.parseGroupedExpression(),
            .IF => self.parseIfExpression(),
            .FUNCTION => self.parseFnExpression(),
            else => {
                self.makeError("No prefix parser for type {any}", .{kind});
                return null;
            },
        };
    }

    fn parseInfix(self: *Self, left: *ast.Expression) ?ast.Expression {
        return switch (self.cur_token.kind) {
            .FUNCTION => self.parseCallExpression(left),
            .LPAREN => self.parseCallExpression(left),
            else => self.parseInfixExpression(left),
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
            self.makeError("Could not parse right expression for {any}", .{left.*});
        }

        return ast.Expression{ .infix_expr = expr };
    }

    fn parseGroupedExpression(self: *Self) ?ast.Expression {
        self.nextToken();

        // TODO: Error handling!
        var exp: ?ast.Expression = null;
        if (self.parseExpression(.LOWEST)) |new_exp| {
            exp = new_exp;
        }

        if (!self.expectPeek(.RPAREN)) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(self: *Self) ?ast.Expression {
        var ifexp = ast.IfExpression{
            .alloc = self.alloc,
            .token = self.cur_token,
            .condition = null,
            .consequence = null,
            .alternative = null,
        };

        if (!self.expectPeek(.LPAREN)) {
            return null;
        }

        self.nextToken();
        if (self.parseExpression(.LOWEST)) |exp| {
            ifexp.condition = self.alloc.create(ast.Expression) catch unreachable;
            ifexp.condition.?.* = exp;
        } else {
            ifexp.deinit();
            return null;
        }

        if (!self.expectPeek(.RPAREN)) {
            ifexp.deinit();
            return null;
        }

        if (!self.expectPeek(.LBRACE)) {
            ifexp.deinit();
            return null;
        }

        if (self.parseBlockStatement()) |block| {
            ifexp.consequence = self.alloc.create(ast.BlockStatement) catch unreachable;
            ifexp.consequence.?.* = block;
        } else {
            ifexp.deinit();
            return null;
        }

        if (self.nextTokenIs(.ELSE)) {
            self.nextToken();

            if (!self.expectPeek(.LBRACE)) {
                ifexp.deinit();
                return null;
            }

            if (self.parseBlockStatement()) |block| {
                ifexp.alternative = self.alloc.create(ast.BlockStatement) catch unreachable;
                ifexp.alternative.?.* = block;
            }
        }

        return ast.Expression{ .if_expr = ifexp };
    }

    fn parseFnExpression(self: *Self) ?ast.Expression {
        var fnexp = ast.FnExpression{
            .alloc = self.alloc,
            .token = self.cur_token,
            .parameters = ArrayList(ast.Identifier).init(self.alloc),
            .block = null,
        };

        if (!self.expectPeek(.LPAREN)) {
            return null;
        }

        self.nextToken();
        while (self.nextTokenIs(.COMMA)) {
            fnexp.parameters.append(ast.Identifier.init(self.cur_token)) catch unreachable;
            self.nextToken();
            self.nextToken();
        }

        const cur_token = self.cur_token;
        if (!self.expectPeek(.RPAREN)) {
            fnexp.deinit();
            return null;
        }
        fnexp.parameters.append(ast.Identifier.init(cur_token)) catch unreachable;

        if (!self.expectPeek(.LBRACE)) {
            fnexp.deinit();
            return null;
        }

        if (self.parseBlockStatement()) |block| {
            fnexp.block = self.alloc.create(ast.BlockStatement) catch unreachable;
            fnexp.block.?.* = block;
        } else {
            fnexp.deinit();
            return null;
        }

        return ast.Expression{ .fn_expr = fnexp };
    }

    fn parseCallExpression(self: *Self, left: *ast.Expression) ?ast.Expression {
        var callexp = ast.CallExpression{
            .alloc = self.alloc,
            .token = self.cur_token,
            .function = left,
            .args = ArrayList(ast.Expression).init(self.alloc),
        };

        if (self.nextTokenIs(.RPAREN)) {
            self.nextToken();
            return ast.Expression{ .call_expr = callexp };
        }

        // Parse Arguments List

        self.nextToken();
        if (self.parseExpression(.LOWEST)) |exp| {
            callexp.args.append(exp) catch unreachable;
        } else {
            self.makeError("Unable to parse argument '{s}' to function '{s}'", .{
                self.cur_token.literal,
                left.tokenLiteral(),
            });
            logSourceInfo(@src());
        }

        while (self.nextTokenIs(.COMMA)) {
            self.nextToken();
            self.nextToken();
            if (self.parseExpression(.LOWEST)) |exp| {
                callexp.args.append(exp) catch unreachable;
            } else {
                self.makeError("Unable to parse argument '{s}' to function '{s}'", .{
                    self.cur_token.literal,
                    left.tokenLiteral(),
                });
                logSourceInfo(@src());
            }
        }

        if (!self.expectPeek(.RPAREN)) {
            logSourceInfo(@src());
            callexp.deinit();
            return null;
        }

        return ast.Expression{ .call_expr = callexp };
    }

    // -------- Identifiers and Literals --------

    fn parseIdentifier(self: *Self) ast.Expression {
        const ident = ast.Identifier.init(self.cur_token);
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
        \\let foobar = bar + 10;
        \\let foobar = add(x, y) + 10;
    ;

    var lex = Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();
    defer parser.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 4);

    const expected_idents = [_]ast.Identifier{
        ast.Identifier.init(Token.init(.IDENT, "x")),
        ast.Identifier.init(Token.init(.IDENT, "y")),
        ast.Identifier.init(Token.init(.IDENT, "foobar")),
        ast.Identifier.init(Token.init(.IDENT, "foobar")),
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

    var lex = Lexer.init(input);
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

    var lex = Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    try std.testing.expect(prog.statements.items.len == 3);

    // Can replace the buffer with stdout or stderr if desired
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try prog.print(stream);
}

test "identifier expresssion" {
    const input = "foobar;";

    var lex = Lexer.init(input);
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

    var lex = Lexer.init(input);
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

    var lex = Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &lex);
    defer parser.deinit();

    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try checkParseErrors(parser);

    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try prog.print(stream);

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
        var lex = Lexer.init(input);
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

test "if expressions" {
    const test_data = [_]TestData{
        .{
            .input = "if (foo) { -1 * 5 };",
            .output = "if foo {\n((-1) * 5);\n};\n", // TODO
        },
        .{
            .input = "if (x < y) { x } else { y };",
            .output = "if (x < y) {\nx;\n} else {\ny;\n};\n",
        },
        .{
            .input = "if (5 - 3 < 4) { return true; } else { return false };",
            .output = "if ((5 - 3) < 4) {\nreturn true;\n} else {\nreturn false;\n};\n",
        },
    };

    try testProgram(&test_data, 2048);
}

test "fn expressions" {
    const test_data = [_]TestData{
        .{
            .input = "fn (x, y) {let a = 5 + 3; return a; };",
            .output = "fn(x, y) {\nlet a = (5 + 3);\nreturn a;\n};\n",
        },
    };

    try testProgram(&test_data, 2048);
}

test "call expressions" {
    const test_data = [_]TestData{
        .{ .input = "add(x, y);", .output = "add(x, y);\n" },
        .{ .input = "add(5 - 3 * 4, false, foo);", .output = "add((5 - (3 * 4)), false, foo);\n" },
    };

    try testProgram(&test_data, 2048);
}

pub fn main() !void {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    var gpa = GPA{};
    var alloc = gpa.allocator();

    const buf_size: usize = 2048;
    const test_data = [_]TestData{
        .{ .input = "add(x + y, false);", .output = "add((x + y), false);\n" },
        .{ .input = "add();", .output = "add();\n" },
        .{ .input = "add(x);", .output = "add(x);\n" },
        .{ .input = "add(x, y);", .output = "add(x, y);\n" },
        .{ .input = "add(5 - 3 * 4, false, foo);", .output = "add((5 - (3 * 4)), false, foo);\n" },
    };

    for (test_data) |data| {
        var buf: [buf_size]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        const stream = fbs.writer();
        const input = data.input;
        const output = data.output;

        // Process the input
        var lex = Lexer.init(input);
        var parser = Parser.init(alloc, &lex);
        defer parser.deinit();

        var prog: Program = try parser.parseProgram();
        defer prog.deinit();

        // Check the output
        try checkParseErrors(parser);
        try prog.print(stream);

        try std.testing.expectEqualSlices(u8, output, fbs.getWritten());
    }
}
