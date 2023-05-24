const std = @import("std");
const Tokens = @import("../tokens.zig");
const Expressions = @import("expressions.zig");

const WriteError = std.os.WriteError;
const TokenType = Tokens.TokenType;
const Token = Tokens.Token;
const Expression = Expressions.Expression;

pub const StatementType = enum {
    let_statement,
    return_statement,
    expression_statement,
};

pub const Statement = union(StatementType) {
    const Self = @This();
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    pub fn print(self: Self, stream: anytype) WriteError!void {
        switch (self) {
            inline else => |s| try s.print(stream),
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
    //ident: Identifier,
    //value: Expression,

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
