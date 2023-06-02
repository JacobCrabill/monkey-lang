const std = @import("std");
const Tokens = @import("../tokens.zig");
const Statements = @import("statements.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const WriteError = std.os.WriteError;
const TokenType = Tokens.TokenType;
const Token = Tokens.Token;
const Statement = Statements.Statement;
const BlockStatement = Statements.BlockStatement;

pub const Expression = union(enum) {
    const Self = @This();
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    boolean_literal: BooleanLiteral,
    prefix_expr: PrefixExpression,
    infix_expr: InfixExpression,
    if_expr: IfExpression,
    fn_expr: FnExpression,
    call_expr: CallExpression,

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            inline else => |*e| e.deinit(),
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        switch (self) {
            inline else => |s| try s.print(stream),
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return switch (self) {
            inline else => |e| e.token.literal,
        };
    }

    pub fn clone(self: Self) Self {
        return switch (self) {
            inline else => |e| e.clone(),
        };
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn deinit(_: Self) void {}

    pub fn init(token: Token) Self {
        return .{
            .token = token,
            .value = token.literal,
        };
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{s}", .{self.value});
    }

    pub fn clone(self: Self) Self {
        return Self{
            .token = self.token,
            .value = undefined, // TODO
        };
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Token,
    value: i64,

    pub fn deinit(_: Self) void {}

    pub fn init(token: Token) Self {
        //const value: i64 = std.fmt.parseInt(i64, token.literal, 10) orelse return null;
        const value: i64 = std.fmt.parseInt(i64, token.literal, 10) catch 0;
        return .{
            .token = token,
            .value = value,
        };
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{d}", .{self.value});
    }
};

pub const BooleanLiteral = struct {
    const Self = @This();
    token: Token,
    value: bool,

    pub fn deinit(_: Self) void {}

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{s}", .{self.token.literal});
    }
};

pub const PrefixExpression = struct {
    const Self = @This();
    alloc: Allocator,
    token: Token,
    operator: []const u8,
    right: ?*Expression,

    pub fn createRight(self: *Self) !void {
        self.right = try self.alloc.create(Expression);
    }

    pub fn deinit(self: *Self) void {
        if (self.right) |*pright| {
            pright.*.deinit();
            self.alloc.destroy(pright.*);
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("({s}", .{self.operator});
        if (self.right) |right| {
            try right.print(stream);
        } else {
            try stream.print("null", .{});
        }
        try stream.print(")", .{});
    }
};

pub const InfixExpression = struct {
    const Self = @This();
    alloc: Allocator,
    token: Token,
    left: ?*Expression,
    operator: []const u8,
    right: ?*Expression,

    pub fn createLeft(self: *Self) !void {
        self.left = try self.alloc.create(Expression);
    }

    pub fn createRight(self: *Self) !void {
        self.right = try self.alloc.create(Expression);
    }

    pub fn deinit(self: *Self) void {
        if (self.left) |*pleft| {
            pleft.*.deinit();
            self.alloc.destroy(pleft.*);
        }
        if (self.right) |*pright| {
            pright.*.deinit();
            self.alloc.destroy(pright.*);
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        // Left
        try stream.print("(", .{});
        if (self.left) |left| {
            try left.print(stream);
        } else {
            try stream.print("null", .{});
        }

        // Operator
        try stream.print(" {s} ", .{self.operator});

        // Right
        if (self.right) |right| {
            try right.print(stream);
        } else {
            try stream.print("null", .{});
        }
        try stream.print(")", .{});
    }
};

pub const IfExpression = struct {
    const Self = @This();
    token: Token,
    alloc: Allocator,
    condition: ?*Expression,
    consequence: ?*BlockStatement,
    alternative: ?*BlockStatement,

    pub fn deinit(self: *Self) void {
        if (self.condition) |*pcond| {
            pcond.*.deinit();
            self.alloc.destroy(pcond.*);
        }
        if (self.consequence) |*pcons| {
            pcons.*.deinit();
            self.alloc.destroy(pcons.*);
        }
        if (self.alternative) |*palt| {
            palt.*.deinit();
            self.alloc.destroy(palt.*);
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("if ", .{});
        if (self.condition) |cond| {
            try cond.print(stream);
        }

        try stream.print(" ", .{});
        if (self.consequence) |cons| {
            try cons.print(stream);
        }

        if (self.alternative) |alt| {
            try stream.print(" else ", .{});
            try alt.print(stream);
        }
    }
};

pub const FnExpression = struct {
    const Self = @This();
    token: Token,
    alloc: Allocator,
    parameters: ArrayList(Identifier),
    block: ?*BlockStatement,

    pub fn deinit(self: *Self) void {
        self.parameters.deinit();
        if (self.block) |*pblk| {
            pblk.*.deinit();
            self.alloc.destroy(pblk.*);
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("fn(", .{});
        for (self.parameters.items, 0..) |param, i| {
            try stream.print("{s}", .{param.value});
            if (i < self.parameters.items.len - 1)
                try stream.print(", ", .{});
        }
        try stream.print(") ", .{});

        if (self.block) |blk| {
            try blk.print(stream);
        }
    }
};

pub const CallExpression = struct {
    const Self = @This();
    alloc: Allocator,
    token: Token,
    function: ?*Expression,
    args: ArrayList(Expression),

    pub fn deinit(self: *Self) void {
        if (self.function) |*pfunc| {
            pfunc.*.deinit();
            self.alloc.destroy(pfunc.*);
        }
        for (self.args.items) |*arg| {
            arg.deinit();
        }
        self.args.deinit();
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        if (self.function) |func| {
            try func.print(stream);
        }
        try stream.print("(", .{});
        for (self.args.items, 0..) |arg, i| {
            try arg.print(stream);
            if (i < self.args.items.len - 1) {
                try stream.print(", ", .{});
            }
        }
        try stream.print(")", .{});
    }
};
