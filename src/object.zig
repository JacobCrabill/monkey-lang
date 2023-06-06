const std = @import("std");

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/statements.zig");
    usingnamespace @import("ast/expressions.zig");
};

const Scope = @import("scope.zig").Scope;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Map = std.StringHashMap;
const BufSet = std.BufSet;

pub const ObjectType = enum(u8) {
    none,
    integer,
    boolean,
    function,
    error_msg,
};

pub const Object = union(ObjectType) {
    const Self = @This();
    none: Null,
    integer: Integer,
    boolean: Boolean,
    function: Function,
    error_msg: ErrorMessage,

    pub fn print(self: Self, stream: anytype) !void {
        switch (self) {
            .none => try stream.print("null", .{}),
            .integer => |i| try stream.print("{d}", .{i.value}),
            .boolean => |b| try stream.print("{any}", .{b.value}),
            .function => |f| try f.print(stream),
            .error_msg => |e| try stream.print("ERROR: {s}", .{e.message}),
        }
    }

    /// Free any heap allocations
    pub fn deinit(self: *Self) void {
        switch (self.*) {
            .error_msg => |*e| e.deinit(),
            .function => |*f| f.deinit(),
            inline else => {},
        }
    }

    /// Clone the Object instance, creating new copies of any heap allocations
    /// Caller owns returned heap data
    pub fn clone(self: Self) Self {
        return switch (self) {
            .function => |f| Self{ .function = f.clone() },
            else => self,
        };
    }
};

pub const Null = struct {};

pub const Integer = struct {
    value: i64,
};

pub const Boolean = struct {
    value: bool,
};

pub const Function = struct {
    const Self = @This();
    alloc: Allocator,
    parameters: ArrayList(ast.Identifier),
    body: ast.BlockStatement,
    scope: Scope,

    pub fn init(alloc: Allocator, parameters: ArrayList(ast.Identifier), body: *const ast.BlockStatement, scope: Scope) Self {
        return .{
            .alloc = alloc,
            .parameters = parameters.clone() catch unreachable,
            .body = body.clone(),
            .scope = scope,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parameters.deinit();
        self.body.deinit();
        // caller owns scope
    }

    pub fn clone(self: Self) Self {
        return Self.init(self.alloc, self.parameters, &self.body, self.scope);
    }

    pub fn print(self: Self, stream: anytype) !void {
        try stream.print("fn(", .{});
        for (self.parameters.items, 0..) |p, i| {
            try p.print(stream);
            if (i < self.parameters.items.len - 1)
                try stream.print(", ", .{});
        }
        try stream.print(") ", .{});
        try self.body.print(stream);
    }
};

pub const ErrorMessage = struct {
    const Self = @This();
    alloc: Allocator,
    message: []const u8,

    pub fn init(alloc: Allocator, comptime fmt: []const u8, args: anytype) Self {
        return .{
            .alloc = alloc,
            .message = std.fmt.allocPrint(alloc, fmt, args) catch unreachable,
        };
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.message);
    }
};

pub fn makeInteger(value: i64) Object {
    return Object{
        .integer = Integer{ .value = value },
    };
}

pub fn makeBoolean(value: bool) Object {
    return Object{
        .boolean = Boolean{ .value = value },
    };
}

pub fn makeNull() Object {
    return Object{ .none = Null{} };
}

pub const NullObject = Object{ .none = Null{} };
