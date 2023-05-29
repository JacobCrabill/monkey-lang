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

pub const Evaluator = struct {
    const Self = @This();
    alloc: Allocator,
    is_return_value: bool,
    errors: ArrayList([]const u8), // heap-allocated error messages

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .is_return_value = false,
            .errors = ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.errors) |err| {
            self.alloc.free(err);
        }
        self.errors.deinit();
    }

    pub fn reset(self: *Self) void {
        self.is_return_value = false;

        for (self.errors) |err| {
            self.alloc.free(err);
        }
        self.errors.clearAndFree();
    }

    pub fn makeEvalError(_: Self, comptime msg: []const u8, args: anytype) void {
        std.io.getStdErr().writer().print("Eval Error: ", .{}) catch unreachable;
        std.io.getStdErr().writer().print(msg, args) catch unreachable;
        std.io.getStdErr().writer().print("\n", .{}) catch unreachable;
    }

    fn makeError(self: *Self, comptime fmt: []const u8, args: anytype) Object {
        const msg = std.fmt.allocPrint(self.alloc, fmt, args) catch unreachable;
        self.errors.append(msg) catch unreachable;
        return .{
            .error_msg = obj.ErrorMessage{ .message = msg },
        };
    }

    pub fn evalProgram(self: *Self, prog: Program) Object {
        return self.evalStatements(prog.statements.items);
    }

    pub fn evalStatements(self: *Self, statements: []Statement) Object {
        var result: Object = NullObject;

        for (statements) |stmt| {
            result = self.evalStatement(stmt);
            if (self.is_return_value or result == ObjectType.error_msg) {
                self.is_return_value = false;
                return result;
            }
        }

        return result;
    }

    pub fn evalStatement(self: *Self, statement: Statement) Object {
        return switch (statement) {
            .expression_statement => |exps| blk: {
                if (exps.value) |exp| {
                    break :blk self.evalExpression(exp);
                } else {
                    break :blk self.makeError("Empty expression in statement {any}", .{exps});
                }
            },
            .block_statement => |blocks| self.evalStatements(blocks.statements.items),
            .return_statement => |rets| blk: {
                self.is_return_value = true;
                if (rets.value) |exp| {
                    break :blk self.evalExpression(exp);
                } else {
                    break :blk self.makeError("Empty expression in statement {any}", .{rets});
                }
            },
            else => self.makeError("Invalid statement: {any}", .{statement}),
        };
    }

    pub fn evalExpression(self: *Self, expression: Expression) Object {
        return switch (expression) {
            .integer_literal => |i| obj.makeInteger(i.value),
            .boolean_literal => |b| obj.makeBoolean(b.value),
            .prefix_expr => |p| self.evalPrefixExpression(p),
            .infix_expr => |i| self.evalInfixExpression(i),
            .if_expr => |i| self.evalIfexpression(i),
            else => self.makeError("Invalid expression {any}", .{expression}),
        };
    }

    fn evalPrefixExpression(self: *Self, prefix_expression: ast.PrefixExpression) Object {
        if (prefix_expression.right) |right| {
            const result: Object = self.evalExpression(right.*);

            if (result == ObjectType.error_msg)
                return result;

            return switch (prefix_expression.token.kind) {
                .MINUS => self.evalMinusPrefix(result),
                .BANG => self.evalBangPrefix(result),
                .LPAREN => NullObject,
                else => self.makeError("Invalid prefix operator: '{s}'", .{prefix_expression.operator}),
            };
        }

        return self.makeError("Empty rval in expression: {any}", .{prefix_expression});
    }

    fn evalInfixExpression(self: *Self, infix_expression: ast.InfixExpression) Object {
        if (infix_expression.left == null or infix_expression.right == null) {
            return NullObject;
        }

        const left: Object = self.evalExpression(infix_expression.left.?.*);
        if (left == ObjectType.error_msg)
            return left;

        const right: Object = self.evalExpression(infix_expression.right.?.*);
        if (right == ObjectType.error_msg)
            return right;

        if (@enumToInt(left) != @enumToInt(right)) {
            return self.makeError("Invalid expression: '{s} {s} {s}'", .{
                infix_expression.left.?.*.tokenLiteral(),
                infix_expression.operator,
                infix_expression.right.?.*.tokenLiteral(),
            });
        }

        return switch (left) {
            .integer => self.evalIntegerInfix(left, right, infix_expression.token.kind),
            .boolean => self.evalBooleanInfix(left, right, infix_expression.token.kind),
            else => self.makeError("Invalid expression: '{s} {s} {s}'", .{
                infix_expression.left.?.*.tokenLiteral(),
                infix_expression.operator,
                infix_expression.right.?.*.tokenLiteral(),
            }),
        };
    }

    fn evalIfexpression(self: *Self, if_expression: ast.IfExpression) Object {
        // We require a condition
        if (if_expression.condition == null)
            return NullObject;

        // We also require something to do based on that condition
        if (if_expression.consequence == null and if_expression.alternative == null)
            return NullObject;

        const condition: Object = self.evalExpression(if_expression.condition.?.*);

        if (condition == ObjectType.error_msg)
            return condition;

        if (condition != ObjectType.boolean)
            return NullObject;

        if (condition.boolean.value) {
            if (if_expression.consequence) |pcons| {
                return self.evalStatements(pcons.statements.items);
            }
        } else {
            if (if_expression.alternative) |palt| {
                return self.evalStatements(palt.statements.items);
            }
        }

        return NullObject;
    }

    fn evalMinusPrefix(self: *Self, object: Object) Object {
        return switch (object) {
            .integer => |i| return obj.makeInteger(-i.value),
            else => self.makeError("Invalid expression: -{any}", .{object}),
        };
    }

    fn evalBangPrefix(_: *Self, object: Object) Object {
        return switch (object) {
            .boolean => |b| return obj.makeBoolean(!b.value),
            .none => return obj.makeBoolean(true),
            else => return obj.makeBoolean(false),
        };
    }

    fn evalIntegerInfix(self: *Self, left: Object, right: Object, operator: TokenType) Object {
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

    fn evalBooleanInfix(self: *Self, left: Object, right: Object, operator: TokenType) Object {
        return switch (operator) {
            .EQ => obj.makeBoolean(left.boolean.value == right.boolean.value),
            .NEQ => obj.makeBoolean(left.boolean.value != right.boolean.value),
            else => self.makeError("Invalid operator {any} for BooleanInfix", .{operator}),
        };
    }
};

// ---------------- Unit Test Helper Functions ----------------

fn testEval(alloc: Allocator, input: []const u8) ?Object {
    var lex = Lexer.init(input);
    var parser = Parser.init(alloc, &lex);
    defer parser.deinit();

    var prog: Program = parser.parseProgram() catch return null;
    defer prog.deinit();

    var evaluator = Evaluator.init(alloc);
    return evaluator.evalProgram(prog);
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
        const result: Object = testEval(std.testing.allocator, d.input).?;
        try std.testing.expect(compareBooleans(result, d.value));
    }
}

// TODO: Test expecting errors
