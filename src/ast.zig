const std = @import("std");
const Tokens = @import("token.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

/// Node:
///   decls:
///     fn tokenLiteral() []const u8
///
/// Statement:
///   decls:
///     fn statementNode() Node
///
/// Expression:
///   decls:
///     fn expressionNode() Node
pub const Node = union(enum) {
    const Self = @This();
    statement: Statement,
    expression: Expression,

    pub fn expressionNode(self: Self) Node {
        return switch (self) {
            .expression => |e| e.expressionNode(),
            .statement => Node{},
        };
    }

    pub fn statementNode(self: Self) Node {
        return switch (self) {
            .statement => |e| e.statementNode(),
            .expression => Node{},
        };
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return switch (self) {
            inline else => |s| s.tokenLiteral(),
        };
    }
};

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

    pub fn statementNode(self: Self) Node {
        return switch (self) {
            inline else => |s| s.statementNode(),
        };
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return switch (self) {
            inline else => |s| s.tokenLiteral(),
        };
    }

    pub fn print(self: Self, stream: anytype) !void {
        switch (self) {
            inline else => |s| try s.print(stream),
        }
    }
};

pub const Expression = union(enum) {
    const Self = @This();
    identifier: Identifier,
    integer_literal: IntegerLiteral,

    pub fn print(self: Self, stream: anytype) !void {
        switch (self) {
            inline else => |s| try s.print(stream),
        }
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Tokens.Token,
    value: []const u8,

    pub fn init(kind: Tokens.TokenType, value: []const u8) Self {
        return .{
            .token = Tokens.Token.init(kind, value),
            .value = value,
        };
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn print(self: Self, stream: anytype) !void {
        try stream.print("{s}", .{self.value});
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Tokens.Token,
    value: i64,

    pub fn init(token: Tokens.Token) Self {
        //const value: i64 = std.fmt.parseInt(i64, token.literal, 10) orelse return null;
        const value: i64 = std.fmt.parseInt(i64, token.literal, 10) catch 0;
        return .{
            .token = token,
            .value = value,
        };
    }

    pub fn print(self: Self, stream: anytype) !void {
        try stream.print("{d}", .{self.value});
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
    token: Tokens.Token,
    value: ?Expression,

    pub fn print(self: Self, stream: anytype) !void {
        try stream.print("{s} ", .{self.token.literal});
        if (self.value) |value| {
            try value.print(stream);
        }
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();
    token: Tokens.Token,
    value: ?Expression,

    pub fn print(self: Self, stream: anytype) !void {
        if (self.value) |value| {
            try stream.print("{s}", .{value.identifier.value});
        } else {
            try stream.print("null", .{});
        }
    }
};

pub const LetStatement = struct {
    const Self = @This();
    token: Tokens.Token,
    ident: Tokens.Token, // Identifier
    value: ?Expression,
    //ident: Identifier,
    //value: Expression,

    pub fn print(self: Self, stream: anytype) !void {
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

/// The AST representation of a program
pub const Program = struct {
    const Self = @This();
    alloc: Allocator,
    statements: ArrayList(Statement),

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .statements = ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.statements.deinit();
    }

    pub fn append(self: *Self, statement: Statement) !void {
        try self.statements.append(statement);
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements[0].token.literal;
        } else {
            return "";
        }
    }

    pub fn print(self: *Self, stream: anytype) !void {
        for (self.statements.items) |s| {
            try s.print(stream);
            try stream.print(";\n", .{});
        }
    }
};
