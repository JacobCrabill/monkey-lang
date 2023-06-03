const std = @import("std");

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/statements.zig");
    usingnamespace @import("ast/expressions.zig");
};

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Map = std.StringHashMap;
const BufSet = std.BufSet;

pub const Scope = struct {
    const Self = @This();
    alloc: Allocator,
    map: Map(Object),
    keys: BufSet,

    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .map = Map(Object).init(alloc),
            .keys = BufSet.init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
        self.keys.deinit();
    }

    pub fn clone(self: Self) Self {
        return .{
            .alloc = self.alloc,
            .map = self.map.clone() catch unreachable,
            .keys = self.keys.clone() catch unreachable,
        };
    }

    pub fn get(self: Self, key: []const u8) ?Object {
        return self.map.get(key);
    }

    pub fn set(self: *Self, key: []const u8, value: Object) void {
        const owned_key = self.alloc.alloc(u8, key.len) catch unreachable;
        @memcpy(owned_key, key);
        self.keys.insert(owned_key) catch unreachable;
        self.map.put(owned_key, value) catch unreachable;
    }

    pub fn contains(self: Self, key: []const u8) bool {
        return self.keys.contains(key);
    }

    /// DEBUG
    pub fn print(self: Self) void {
        var iter = self.keys.iterator();
        while (iter.next()) |pkey| {
            std.debug.print("{s}\n", .{pkey.*});
        }
    }
};

pub const ScopeStack = struct {
    const Self = @This();
    alloc: Allocator,
    stack: ArrayList(Scope),
    idx: usize,

    /// Create a new instance with a single empty Scope
    pub fn init(alloc: Allocator) Self {
        var scope = Self{
            .alloc = alloc,
            .stack = ArrayList(Scope).init(alloc),
            .idx = 0,
        };
        scope.pushNew();
        return scope;
    }

    /// Clear and free all scopes
    pub fn deinit(self: *Self) void {
        for (self.stack.items) |*stack| {
            stack.deinit();
        }
        self.stack.deinit();
        self.keys.deinit();
    }

    /// Clear all existing scopes, and create a single new empty scope
    pub fn reset(self: *Self) void {
        for (self.stack.items) |*stack| {
            stack.deinit();
        }
        self.stack.clearAndFree();
        self.pushNew();
    }

    /// Instantiate and push a new (empty) Scope to our stack
    pub fn pushNew(self: *Self) void {
        self.stack.append(Scope.init(self.alloc)) catch unreachable;
        self.idx = self.stack.items.len - 1;
    }

    /// Pop the most recent Scope off of the stack
    pub fn pop(self: *Self) Scope {
        self.idx -= 1;
        return self.stack.pop();
    }
    pub fn getCopy(self: *Self) Scope {
        return self.stack.items[self.idx].clone();
    }

    /// Get a value from the latest scope
    pub fn get(self: *Self, key: []const u8) ?Object {
        return self.stack.items[self.idx].get(key);
    }

    /// Put a value into the latest scope
    pub fn set(self: *Self, key: []const u8, value: Object) void {
        self.stack.items[self.idx].set(key, value);
    }

    /// DEBUG
    pub fn print(self: Self) void {
        for (self.stack.items, 0..) |scope, i| {
            std.debug.print("Scope {d}: ", .{i});
            scope.print();
        }
    }
};

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

    pub fn init(alloc: Allocator, parameters: ArrayList(ast.Identifier), body: *ast.BlockStatement, scope: Scope) Self {
        return .{
            .alloc = alloc,
            .parameters = parameters.clone() catch unreachable,
            .body = body.clone(),
            .scope = scope,
        };
    }

    pub fn deinit(self: *Self) void {
        self.body.deinit();
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
    message: []const u8,
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
