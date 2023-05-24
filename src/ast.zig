const std = @import("std");
const Tokens = @import("tokens.zig");
const Statements = @import("ast/statements.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const WriteError = std.os.WriteError;
const Statement = Statements.Statement;

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
        for (self.statements.items) |*stmt| {
            switch (stmt.*) {
                .expression_statement => |*es| es.deinit(),
                else => {},
            }
        }
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

    pub fn print(self: *Self, stream: anytype) WriteError!void {
        for (self.statements.items) |s| {
            try s.print(stream);
            try stream.print(";\n", .{});
        }
    }

    pub fn printStatements(self: *Self, stream: anytype) WriteError!void {
        for (self.statements.items) |s| {
            try stream.print("{any}\n", .{s});
        }
    }
};
