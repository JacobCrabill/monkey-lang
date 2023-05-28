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

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Program = ast.Program;
const Expression = ast.Expression;
const Statement = ast.Statement;
const Object = obj.Object;
const ObjectType = obj.ObjectType;
const NullObject = obj.NullObject;

pub fn evalProgram(prog: Program) Object {
    return evalStatements(prog.statements.items);
}

pub fn evalStatements(statements: []Statement) Object {
    var result: Object = NullObject;

    for (statements) |stmt| {
        result = evalStatement(stmt);
    }

    return result;
}

pub fn evalStatement(statement: Statement) Object {
    return switch (statement) {
        .expression_statement => |exps| blk: {
            if (exps.value) |exp| {
                break :blk evalExpression(exp);
            } else {
                break :blk NullObject;
            }
        },
        else => NullObject,
    };
}

pub fn evalExpression(expression: Expression) Object {
    return switch (expression) {
        .integer_literal => |i| obj.makeInteger(i.value),
        .boolean_literal => |b| obj.makeBoolean(b.value),
        .prefix_expr => |p| evalPrefixExpression(p),
        .infix_expr => |i| evalInfixExpression(i),
        else => NullObject,
    };
}

fn evalPrefixExpression(prefix_expression: ast.PrefixExpression) Object {
    if (prefix_expression.right) |right| {
        const result: Object = evalExpression(right.*);
        return switch (prefix_expression.token.kind) {
            .MINUS => evalMinusPrefix(result),
            .BANG => evalBangPrefix(result),
            .LPAREN => NullObject,
            else => NullObject,
        };
    }

    return NullObject;
}

fn evalInfixExpression(infix_expression: ast.InfixExpression) Object {
    if (infix_expression.left == null or infix_expression.right == null) {
        return NullObject;
    }

    const left: Object = evalExpression(infix_expression.left.?.*);
    const right: Object = evalExpression(infix_expression.right.?.*);

    if (@enumToInt(left) != @enumToInt(right))
        return NullObject;

    return switch (left) {
        .integer => evalIntegerInfix(left, right, infix_expression.token.kind),
        else => NullObject,
    };
}

fn evalMinusPrefix(object: Object) Object {
    return switch (object) {
        .integer => |i| return obj.makeInteger(-i.value),
        else => NullObject,
    };
}

fn evalBangPrefix(object: Object) Object {
    return switch (object) {
        .boolean => |b| return obj.makeBoolean(!b.value),
        .none => return obj.makeBoolean(true),
        else => return obj.makeBoolean(false),
    };
}

fn evalIntegerInfix(left: Object, right: Object, operator: TokenType) Object {
    return switch (operator) {
        .PLUS => obj.makeInteger(left.integer.value + right.integer.value),
        .MINUS => obj.makeInteger(left.integer.value - right.integer.value),
        .STAR => obj.makeInteger(left.integer.value * right.integer.value),
        .SLASH => obj.makeInteger(@divFloor(left.integer.value, right.integer.value)),
        else => NullObject,
    };
}

// ---------------- Unit Test Helper Functions ----------------

fn testEval(alloc: Allocator, input: []const u8) ?Object {
    var lex = Lexer.init(input);
    var parser = Parser.init(alloc, &lex);
    defer parser.deinit();

    var prog: Program = parser.parseProgram() catch return null;
    defer prog.deinit();

    return evalProgram(prog);
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
    };

    for (data) |d| {
        const result: Object = testEval(std.testing.allocator, d.input).?;
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
        //.{ .input = "5 > 4", .value = true },
        //.{ .input = "5 < 4", .value = false },
        //.{ .input = "5 != 4", .value = true },
        //.{ .input = "5 == (4 + 1)", .value = true },
        //.{ .input = "(5 * 1) == (4 + 1)", .value = true },
    };

    for (data) |d| {
        const result: Object = testEval(std.testing.allocator, d.input).?;
        try std.testing.expect(compareBooleans(result, d.value));
    }
}
