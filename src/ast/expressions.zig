const std = @import("std");
const Tokens = @import("../tokens.zig");

const Allocator = std.mem.Allocator;
const WriteError = std.os.WriteError;
const TokenType = Tokens.TokenType;
const Token = Tokens.Token;

pub const Expression = union(enum) {
    const Self = @This();
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    boolean_literal: BooleanLiteral,
    prefix_expr: PrefixExpression,
    infix_expr: InfixExpression,

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            .prefix_expr => |*pfx| pfx.deinit(),
            .infix_expr => |*ifx| ifx.deinit(),
            else => {},
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        switch (self) {
            inline else => |s| try s.print(stream),
        }
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(kind: TokenType, value: []const u8) Self {
        return .{
            .token = Token.init(kind, value),
            .value = value,
        };
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{s}", .{self.value});
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Token,
    value: i64,

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
        if (self.right) |*right| {
            right.*.deinit();
            self.alloc.destroy(right.*);
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
    lalloc: bool = false,
    ralloc: bool = false,

    pub fn createLeft(self: *Self) !void {
        self.left = try self.alloc.create(Expression);
        self.lalloc = true;
    }

    pub fn createRight(self: *Self) !void {
        self.right = try self.alloc.create(Expression);
        self.ralloc = true;
    }

    pub fn deinit(self: *Self) void {
        if (self.left) |*left| {
            left.*.deinit();
            if (self.lalloc) {
                self.alloc.destroy(left.*);
                self.lalloc = false;
            }
        }
        if (self.right) |*right| {
            right.*.deinit();
            if (self.ralloc) {
                self.alloc.destroy(right.*);
                self.ralloc = false;
            }
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
