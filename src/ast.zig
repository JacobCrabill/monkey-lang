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

pub const Statement = union(enum) {
    const Self = @This();
    let_statement: LetStatement,

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
};

pub const Expression = union {
    const Self = @This();
    identifier: Identifier,

    pub fn expressionNode(_: Self) Node {
        return Node{}; // TODO
    }
};

pub const LetStatement = struct {
    const Self = @This();
    token: Tokens.Token,
    ident: Tokens.Token,
    value: Tokens.Token,
    //ident: Identifier,
    //value: Expression,

    pub fn statementNode(_: Self) Node {
        return Node{}; // TODO
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
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
};

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
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};
