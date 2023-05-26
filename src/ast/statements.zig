const std = @import("std");
const Tokens = @import("../tokens.zig");
const Expressions = @import("expressions.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const WriteError = std.os.WriteError;
const TokenType = Tokens.TokenType;
const Token = Tokens.Token;
const Expression = Expressions.Expression;

pub const StatementType = enum {
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
};

/// TODO:
/// Should we instead have this:
/// pub const Statement = struct {
///   const Self = @This();
///   kind: StatementType,
///   token: Token,
///   value: Token,
/// };
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
        // TODO: Check undefined
        try stream.print("{s} ", .{let});
        try stream.print("{s} = ", .{name});
        if (self.value) |value| {
            try value.print(stream);
        }
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
};
