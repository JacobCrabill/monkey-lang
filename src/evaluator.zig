const std = @import("std");

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/expressions.zig");
    usingnamespace @import("ast/statements.zig");
};
const obj = @import("object.zig");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TokenType = @import("tokens.zig").TokenType;
const Scope = @import("scope.zig").Scope;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Program = ast.Program;
const Expression = ast.Expression;
const Statement = ast.Statement;
const Object = obj.Object;
const ObjectType = obj.ObjectType;
const NullObject = obj.NullObject;

pub const Evaluator = struct {
    const Self = @This();
    alloc: Allocator,
    is_return_value: bool,

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .is_return_value = false,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn reset(self: *Self) void {
        self.is_return_value = false;
    }

    pub fn evalProgram(self: *Self, prog: Program, scope: *Scope) Object {
        return self.evalStatements(prog.statements.items, scope);
    }

    pub fn evalStatements(self: *Self, statements: []Statement, scope: *Scope) Object {
        if (statements.len == 0)
            return NullObject;

        var results = ArrayList(Object).init(self.alloc);
        defer {
            for (results.items) |*item| item.deinit();
            results.deinit();
        }

        for (statements) |stmt| {
            var result = self.evalStatement(stmt, scope);
            if (self.is_return_value or result == ObjectType.error_msg) {
                self.is_return_value = false;
                return result;
            }
            results.append(result) catch unreachable;
        }

        var res = results.pop();
        return res;
    }

    pub fn evalStatement(self: *Self, statement: Statement, scope: *Scope) Object {
        return switch (statement) {
            .expression_statement => |exps| self.evalExpressionStatement(exps, scope),
            .block_statement => |blocks| self.evalStatements(blocks.statements.items, scope),
            .return_statement => |rets| self.evalReturnStatement(rets, scope),
            .let_statement => |lets| self.evalLetStatement(lets, scope),
        };
    }

    pub fn evalExpressions(self: *Self, expressions: []Expression, scope: *Scope) ArrayList(Object) {
        var results = ArrayList(Object).init(self.alloc);
        for (expressions) |exp| {
            results.append(self.evalExpression(exp, scope)) catch unreachable;
        }
        return results;
    }

    pub fn evalExpression(self: *Self, expression: Expression, scope: *Scope) Object {
        return switch (expression) {
            .integer_literal => |i| obj.makeInteger(i.value),
            .boolean_literal => |b| obj.makeBoolean(b.value),
            .prefix_expr => |p| self.evalPrefixExpression(p, scope),
            .infix_expr => |i| self.evalInfixExpression(i, scope),
            .if_expr => |i| self.evalIfExpression(i, scope),
            .identifier => |i| self.evalIdentifier(i, scope),
            .fn_expr => |f| self.evalFnExpression(f, scope),
            .call_expr => |c| self.evalCallExpression(c, scope),
        };
    }

    fn evalExpressionStatement(self: *Self, statement: ast.ExpressionStatement, scope: *Scope) Object {
        if (statement.value) |exp| {
            return self.evalExpression(exp, scope);
        } else {
            return self.makeError("Empty expression in statement {any}", .{statement});
        }
    }

    fn evalReturnStatement(self: *Self, statement: ast.ReturnStatement, scope: *Scope) Object {
        self.is_return_value = true;
        if (statement.value) |exp| {
            return self.evalExpression(exp, scope);
        } else {
            return self.makeError("Empty expression in statement {any}", .{statement});
        }
    }

    fn evalLetStatement(self: *Self, statement: ast.LetStatement, scope: *Scope) Object {
        if (statement.value) |exp| {
            const value = self.evalExpression(exp, scope);
            // self.stack.set(statement.ident.literal, value);
            scope.set(statement.ident.literal, value);
            return value;
        } else {
            return self.makeError("Empty expression in statement {any}", .{statement});
        }
    }

    fn evalPrefixExpression(self: *Self, prefix_expression: ast.PrefixExpression, scope: *Scope) Object {
        if (prefix_expression.right) |right| {
            const result: Object = self.evalExpression(right.*, scope);

            if (result == ObjectType.error_msg)
                return result;

            return switch (prefix_expression.token.kind) {
                .MINUS => self.evalMinusPrefix(result, scope),
                .BANG => self.evalBangPrefix(result, scope),
                .LPAREN => NullObject,
                else => self.makeError("Invalid prefix operator: '{s}'", .{prefix_expression.operator}),
            };
        }

        return self.makeError("Empty rval in expression: {any}", .{prefix_expression});
    }

    fn evalInfixExpression(self: *Self, infix_expression: ast.InfixExpression, scope: *Scope) Object {
        if (infix_expression.left == null or infix_expression.right == null) {
            return NullObject;
        }

        const left: Object = self.evalExpression(infix_expression.left.?.*, scope);
        if (left == ObjectType.error_msg)
            return left;

        const right: Object = self.evalExpression(infix_expression.right.?.*, scope);
        if (right == ObjectType.error_msg)
            return right;

        if (@intFromEnum(left) != @intFromEnum(right)) {
            return self.makeError("Invalid expression: '{s} {s} {s}'", .{
                infix_expression.left.?.*.tokenLiteral(),
                infix_expression.operator,
                infix_expression.right.?.*.tokenLiteral(),
            });
        }

        return switch (left) {
            .integer => self.evalIntegerInfix(left, right, infix_expression.token.kind, scope),
            .boolean => self.evalBooleanInfix(left, right, infix_expression.token.kind, scope),
            else => self.makeError("Invalid expression: '{s} {s} {s}'", .{
                infix_expression.left.?.*.tokenLiteral(),
                infix_expression.operator,
                infix_expression.right.?.*.tokenLiteral(),
            }),
        };
    }

    fn evalIfExpression(self: *Self, if_expression: ast.IfExpression, scope: *Scope) Object {
        // We require a condition
        if (if_expression.condition == null)
            return NullObject;

        // We also require something to do based on that condition
        if (if_expression.consequence == null and if_expression.alternative == null)
            return NullObject;

        const condition: Object = self.evalExpression(if_expression.condition.?.*, scope);

        if (condition == ObjectType.error_msg)
            return condition;

        if (condition != ObjectType.boolean)
            return NullObject;

        if (condition.boolean.value) {
            if (if_expression.consequence) |pcons| {
                return self.evalStatements(pcons.statements.items, scope);
            }
        } else {
            if (if_expression.alternative) |palt| {
                return self.evalStatements(palt.statements.items, scope);
            }
        }

        return NullObject;
    }

    fn evalIdentifier(self: *Self, identifier: ast.Identifier, scope: *Scope) Object {
        if (scope.get(identifier.value)) |ident| {
            return ident;
        }

        return self.makeError("Unknown identifier: {s}", .{identifier.value});
    }

    fn evalFnExpression(self: *Self, fn_exp: ast.FnExpression, scope: *Scope) Object {
        if (fn_exp.block == null)
            return NullObject;

        return .{ .function = obj.Function.init(self.alloc, fn_exp.parameters, fn_exp.block.?, scope.*) };
    }

    fn evalCallExpression(self: *Self, call_expr: ast.CallExpression, scope: *Scope) Object {
        if (call_expr.function == null)
            return self.makeError("Empty function in call expression", .{});

        var args: ArrayList(Object) = self.evalExpressions(call_expr.args.items, scope);
        defer {
            for (args.items) |*arg| arg.deinit();
            args.deinit();
        }

        if (args.items.len == 1 and args.items[0] == ObjectType.error_msg) {
            return args.items[0];
        }

        return self.applyFunction(call_expr.function.?.*, args.items, scope);
    }

    fn applyFunction(self: *Self, fn_expression: Expression, args: []Object, scope: *Scope) Object {
        // Get the (assumed) function expression
        var fn_obj: Object = self.evalExpression(fn_expression, scope);
        if (fn_obj != ObjectType.function)
            return self.makeError("Cannot apply function to expression: {any}", .{fn_expression});

        var function: obj.Function = fn_obj.function.clone();
        defer function.deinit();

        // Setup the function's scope
        var fn_scope: Scope = function.scope.clone();
        defer fn_scope.deinit();

        // Setup arguments
        const nargs: usize = function.parameters.items.len;
        if (nargs != args.len) {
            return self.makeError("Expected {d} arguments, got {d}", .{ nargs, args.len });
        }

        for (function.parameters.items, args) |ident, arg| {
            fn_scope.set(ident.value, arg);
        }

        // Evaluate the function
        return self.evalStatements(function.body.statements.items, &fn_scope);
    }

    fn evalMinusPrefix(self: *Self, object: Object, _: *Scope) Object {
        return switch (object) {
            .integer => |i| return obj.makeInteger(-i.value),
            else => self.makeError("Invalid expression: -{any}", .{object}),
        };
    }

    fn evalBangPrefix(_: *Self, object: Object, _: *Scope) Object {
        return switch (object) {
            .boolean => |b| return obj.makeBoolean(!b.value),
            .none => return obj.makeBoolean(true),
            else => return obj.makeBoolean(false),
        };
    }

    fn evalIntegerInfix(self: *Self, left: Object, right: Object, operator: TokenType, _: *Scope) Object {
        return switch (operator) {
            .PLUS => obj.makeInteger(left.integer.value + right.integer.value),
            .MINUS => obj.makeInteger(left.integer.value - right.integer.value),
            .STAR => obj.makeInteger(left.integer.value * right.integer.value),
            .SLASH => obj.makeInteger(@divFloor(left.integer.value, right.integer.value)),
            .EQ => obj.makeBoolean(left.integer.value == right.integer.value),
            .NEQ => obj.makeBoolean(left.integer.value != right.integer.value),
            .LT => obj.makeBoolean(left.integer.value < right.integer.value),
            .GT => obj.makeBoolean(left.integer.value > right.integer.value),
            else => self.makeError("Invalid operator {any} for IntegerInfix", .{operator}),
        };
    }

    fn evalBooleanInfix(self: *Self, left: Object, right: Object, operator: TokenType, _: *Scope) Object {
        return switch (operator) {
            .EQ => obj.makeBoolean(left.boolean.value == right.boolean.value),
            .NEQ => obj.makeBoolean(left.boolean.value != right.boolean.value),
            else => self.makeError("Invalid operator {any} for BooleanInfix", .{operator}),
        };
    }

    fn makeEvalError(_: Self, comptime msg: []const u8, args: anytype) void {
        std.io.getStdErr().writer().print("Eval Error: ", .{}) catch unreachable;
        std.io.getStdErr().writer().print(msg, args) catch unreachable;
        std.io.getStdErr().writer().print("\n", .{}) catch unreachable;
    }

    fn makeError(self: *Self, comptime fmt: []const u8, args: anytype) Object {
        return .{ .error_msg = obj.ErrorMessage.init(self.alloc, fmt, args) };
    }
};

// ---------------- Unit Test Helper Functions ----------------

fn testEval(alloc: Allocator, input: []const u8) ?Object {
    var lex = Lexer.init(input);
    var parser = Parser.init(alloc, &lex);
    defer parser.deinit();

    var prog: Program = parser.parseProgram() catch return null;
    defer prog.deinit();

    var scope = Scope.init(alloc);
    var evaluator = Evaluator.init(alloc);
    defer scope.deinit();
    defer evaluator.deinit();
    return evaluator.evalProgram(prog, &scope);
}

fn compareIntegers(object: Object, value: i64) bool {
    return switch (object) {
        .integer => |i| i.value == value,
        else => false,
    };
}

fn compareBooleans(object: Object, value: bool) bool {
    return switch (object) {
        .boolean => |i| i.value == value,
        else => false,
    };
}

// ---------------- Unit Tests ----------------

test "eval integers" {
    const TestData = struct {
        input: []const u8,
        value: i64,
    };

    const data = [_]TestData{
        .{ .input = "5", .value = 5 },
        .{ .input = "10", .value = 10 },
        .{ .input = "-42", .value = -42 },
        .{ .input = "-1", .value = -1 },
        .{ .input = "-1 + 5", .value = 4 },
        .{ .input = "5 + -5", .value = 0 },
        .{ .input = "5 + -(5 * 4) + 1/1", .value = -14 },
        .{ .input = "(0 - 4 * 3) / 3 + 4", .value = 0 },
        .{ .input = "if (5 - 2 == 1) { true } else { 0; }", .value = 0 },
        .{ .input = "if (2 != 3 - 2) { 10; } else { 0; }", .value = 10 },
        .{ .input = "if (2 != 4 - 2) { 10; } else { return 0; return 10; }", .value = 0 },
        .{ .input = "if (2 != 3 - 2) { return 10; return 12; } else { 0; }", .value = 10 },
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        defer result.deinit();
        try std.testing.expect(compareIntegers(result, d.value));
    }
}

test "eval booleans" {
    const TestData = struct {
        input: []const u8,
        value: bool,
    };

    const data = [_]TestData{
        .{ .input = "true", .value = true },
        .{ .input = "false", .value = false },
        .{ .input = "!false", .value = true },
        .{ .input = "!true", .value = false },
        .{ .input = "!!true", .value = true },
        .{ .input = "5 > 4", .value = true },
        .{ .input = "5 < 4", .value = false },
        .{ .input = "5 != 4", .value = true },
        .{ .input = "5 == (4 + 1)", .value = true },
        .{ .input = "(5 * 1) == (4 + 1)", .value = true },
        .{ .input = "if (5 - 4 == 1) { true } else { 0; }", .value = true },
        .{ .input = "if (5 - 5 == 5) { true } else { false; }", .value = false },
        .{ .input = "return 5 != 4", .value = true },
        .{ .input = "return 5 - 1/1 == 4*(2/2)", .value = true },
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        defer result.deinit();
        try std.testing.expect(compareBooleans(result, d.value));
    }
}

test "eval functions" {
    const TestData = struct {
        input: []const u8,
        value: i64,
    };

    const data = [_]TestData{
        .{ .input = "let add = fn(x,y) { return x + y; }; add(1,2);", .value = 3 },
        .{
            .input =
            \\let add = fn(x,y) { return x + y; };
            \\let addTwo = fn(x) { return add(x, 2); };
            \\addTwo(9);
            \\
            ,
            .value = 11,
        },
        //.{
        //    .input =
        //    \\let add = fn(x,y) { return x + y; };
        //    \\let addTwo = fn(x) { return add(x, 2); };
        //    \\let addThree = fn(x) { return add(1, addTwo(x, 2)); };
        //    \\addThree(39);
        //    \\
        //    ,
        //    .value = 42,
        //},
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        defer result.deinit();
        try std.testing.expect(compareIntegers(result, d.value));
    }
}

// TODO: Test expecting errors

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();

    const input = "let a = fn(x, y) { return x + y; };\na(2, 3) + 1;\n";
    const output = std.io.getStdErr().writer();

    var lex = Lexer.init(input);
    var parser = Parser.init(alloc, &lex);
    defer parser.deinit();

    // Parse and print the statement(s)
    var prog: Program = try parser.parseProgram();
    defer prog.deinit();

    try output.print("Program:\n", .{});
    try prog.print(output);
    try output.print("-- End Program\n", .{});

    var scope = Scope.init(alloc);
    var evaluator = Evaluator.init(alloc);
    defer scope.deinit();
    defer evaluator.deinit();
    const result = evaluator.evalProgram(prog, &scope);
    try result.print(output);
    try output.print("\n", .{});
}
