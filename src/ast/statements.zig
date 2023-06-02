const std = @import("std");
const Tokens = @import("../tokens.zig");
const Expressions = @import("expressions.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const WriteError = std.os.WriteError;
const TokenType = Tokens.TokenType;
const Token = Tokens.Token;
const Expression = Expressions.Expression;

pub const StatementType = enum(u8) {
    let_statement,
    return_statement,
    expression_statement,
    block_statement,
};

pub const Statement = union(StatementType) {
    const Self = @This();
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,

    pub fn print(self: Self, stream: anytype) WriteError!void {
        switch (self) {
            inline else => |s| try s.print(stream),
        }
    }

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            inline else => |*ps| ps.*.deinit(),
        }
    }

    pub fn clone(self: Self) Self {
        var copy: Statement = switch (self) {
            inline else => |s| s.clone(),
        };
        return copy;
    }
};

pub const ReturnStatement = struct {
    const Self = @This();
    token: Token,
    value: ?Expression,

    pub fn deinit(self: *Self) void {
        if (self.value) |*expr| {
            expr.deinit();
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{s} ", .{self.token.literal});
        if (self.value) |value| {
            try value.print(stream);
        }
    }

    pub fn clone(self: Self) Self {
        return Self{
            .token = self.token,
            .value = self.value.clone(),
        };
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();
    token: Token,
    value: ?Expression,

    pub fn deinit(self: *Self) void {
        if (self.value) |*value| {
            value.deinit();
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        if (self.value) |value| {
            try value.print(stream);
        } else {
            try stream.print("null", .{});
        }
    }

    pub fn clone(self: Self) Self {
        return Self{
            .token = self.token,
            .value = self.value.?.clone() orelse null,
        };
    }
};

pub const LetStatement = struct {
    const Self = @This();
    token: Token,
    ident: Token, // Identifier
    value: ?Expression,

    pub fn deinit(self: *Self) void {
        if (self.value) |*value| {
            value.deinit();
        }
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        const let = self.token.literal;
        const name = self.ident.literal;
        try stream.print("{s} ", .{let});
        try stream.print("{s} = ", .{name});
        if (self.value) |value| {
            try value.print(stream);
        }
    }

    pub fn clone(self: Self) Self {
        return Self{
            .token = self.token,
            .ident = self.ident,
            .value = self.value.?.clone() orelse null,
        };
    }
};

pub const BlockStatement = struct {
    const Self = @This();
    alloc: Allocator,
    token: Token,
    statements: ArrayList(Statement),

    pub fn init(alloc: Allocator, token: Token) Self {
        return .{
            .alloc = alloc,
            .token = token,
            .statements = ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.statements.items) |*s| {
            s.*.deinit();
        }
        self.statements.deinit();
    }

    pub fn print(self: Self, stream: anytype) WriteError!void {
        try stream.print("{{\n", .{});
        for (self.statements.items) |s| {
            try s.print(stream);
            try stream.print(";\n", .{});
        }
        try stream.print("}}", .{});
    }

    pub fn clone(self: Self) Self {
        var copy = Self{
            .alloc = self.alloc,
            .token = self.token,
            .statements = ArrayList(Statement).initCapacity(self.alloc, self.statements.items.len),
        };

        for (self.statements.items) |stmt| {
            copy.statements.append(stmt.clone());
        }

        return copy;
    }
};
