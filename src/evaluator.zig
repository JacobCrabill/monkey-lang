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
const Builtins = @import("builtins.zig");

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
            .string_literal => |s| obj.makeString(s.value),
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
            scope.set(statement.ident.literal, value);
            return value;
        } else {
            return self.makeError("Empty expression in statement {any}", .{statement});
        }
    }

    fn evalPrefixExpression(self: *Self, prefix_expression: ast.PrefixExpression, scope: *Scope) Object {
        if (prefix_expression.right) |right| {
            var result: Object = self.evalExpression(right.*, scope);
            defer result.deinit();

            if (result == ObjectType.error_msg)
                return result.clone();

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

        var left: Object = self.evalExpression(infix_expression.left.?.*, scope);
        defer left.deinit();
        if (left == ObjectType.error_msg)
            return left;

        var right: Object = self.evalExpression(infix_expression.right.?.*, scope);
        defer right.deinit();
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
            .string => self.evalStringInfix(left, right, infix_expression.token.kind, scope),
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

        var condition: Object = self.evalExpression(if_expression.condition.?.*, scope);
        defer condition.deinit();

        if (condition == ObjectType.error_msg)
            return condition.clone();

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

        if (Builtins.makeBuiltin(identifier.value)) |builtin| {
            return obj.makeBuiltin(builtin);
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
            // TODO: wtf is this a sefault?? should be a leak...
            //for (args.items) |*arg| arg.deinit();
            args.deinit();
        }

        if (args.items.len == 1 and args.items[0] == ObjectType.error_msg) {
            const res: Object = args.items[0].clone();
            return res;
        }

        return self.applyFunction(call_expr.function.?.*, args.items, scope);
    }

    fn applyFunction(self: *Self, fn_expression: Expression, args: []Object, scope: *Scope) Object {
        // Get the (assumed) function expression
        var fn_obj: Object = self.evalExpression(fn_expression, scope);
        //defer fn_obj.deinit(); // TODO: double-free here

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

    fn evalStringInfix(self: *Self, left: Object, right: Object, operator: TokenType, _: *Scope) Object {
        return switch (operator) {
            .PLUS => obj.concatStrings(self.alloc, left.string.value, right.string.value),
            else => self.makeError("Invalid operator {any} for StringInfix", .{operator}),
        };
    }

    fn makeEvalError(_: Self, comptime msg: []const u8, args: anytype) void {
        var stderr = std.io.getStdErr().writer();
        stderr.print("Eval Error: ", .{}) catch unreachable;
        stderr.print(msg, args) catch unreachable;
        stderr.print("\n", .{}) catch unreachable;
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

fn compareObjects(expected: Object, received: Object) bool {
    //if (expected != received) // Check type of object first
    //    return false;

    return switch (expected) {
        .integer => |i| received == .integer and i.value == received.integer.value,
        .boolean => |b| received == .boolean and b.value == received.boolean.value,
        .string => |s| received == .string and std.mem.eql(u8, s.value, received.string.value),
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

test "eval strings" {
    const TestData = struct {
        input: []const u8,
        value: []const u8,
    };

    const data = [_]TestData{
        .{ .input = "let foo = \"Hello, World!\"", .value = "Hello, World!" },
        .{ .input = "\"this is a string\"", .value = "this is a string" },
        .{ .input = "\"Hello, \" + \"World!\"", .value = "Hello, World!" },
        .{ .input = "\"this\" + \" is a \" + \"string\"", .value = "this is a string" },
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        defer result.deinit();
        switch (result) {
            .string => |s| try std.testing.expectEqualSlices(u8, s.value, d.value),
            else => {
                std.debug.print("Error: Expected string, got:\n", .{});
                try result.print(std.io.getStdErr().writer());
                return error.WrongResultType;
            },
        }
    }
}

test "eval functions" {
    std.testing.log_level = std.log.Level.debug;
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
            ,
            .value = 11,
        },
        .{
            .input =
            \\let add = fn(x,y) { return x + y; };
            \\let addTwo = fn(x) { return add(x, 2); };
            \\let addThree = fn(x) { return add(1, addTwo(x)); };
            \\addThree(add(39,0));
            ,
            .value = 42,
        },
        .{
            .input =
            \\let adder = fn(x) {
            \\  fn(y) { x + y };
            \\};
            \\let addTwo = adder(2);
            \\addTwo(5);
            ,
            .value = 7,
        },
        // TDOO: Memory leak or double-free on this one:
        //.{
        //    .input =
        //    \\let adder = fn(x) { fn(y) { x + y } };
        //    \\adder(1)(3);
        //    ,
        //    .value = 4,
        //},
        // TODO: Recursive functions not working yet
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        std.testing.expect(compareIntegers(result, d.value)) catch {
            // NOTE: stderr and stdout are only available if doing 'zig test src/evaluator.zig'
            //       rather than 'zig build test-evaluator'
            std.debug.print("Test failed: ", .{});
            try result.print(std.io.getStdErr().writer());
            return error.TestExpectedEqual;
        };
        result.deinit();
    }
}

test "eval built-ins" {
    std.testing.log_level = std.log.Level.debug;
    const TestData = struct {
        input: []const u8,
        value: Object,
    };

    const alloc = std.testing.allocator;
    const data = [_]TestData{
        .{ .input = "len(\"\")", .value = obj.makeInteger(0) },
        .{ .input = "len(\"abc\")", .value = obj.makeInteger(3) },
        .{ .input = "len(\"Hello, World!\")", .value = obj.makeInteger(13) },
        .{ .input = "len(1)", .value = obj.makeError(alloc, "Cannot call 'len' on integers") },
    };

    for (data) |d| {
        var result: Object = testEval(std.testing.allocator, d.input).?;
        std.testing.expect(compareObjects(result, d.value)) catch {
            // NOTE: stderr and stdout are only available if doing 'zig test src/evaluator.zig'
            //       rather than 'zig build test-evaluator'
            std.debug.print("Test failed: ", .{});
            try result.print(std.io.getStdErr().writer());
            return error.TestExpectedEqual;
        };
        result.deinit();
    }
}
