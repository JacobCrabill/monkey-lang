const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

pub const Scope = StringHashMap(Object);

pub const ScopeStack = struct {
    const Self = @This();
    alloc: Allocator,
    stack: ArrayList(Scope),

    /// Create a new instance with a single empty Scope
    pub fn init(alloc: Allocator) Self {
        var scope = Self{
            .alloc = alloc,
            .stack = ArrayList(Scope).init(alloc),
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
    }

    /// Pop the most recent Scope off of the stack
    pub fn pop(self: *Self) Scope {
        return self.stack.pop();
    }

    /// Get a value from the latest scope
    pub fn get(self: *Self, key: []const u8) ?Object {
        return self.stack.getLast().get(key);
    }

    /// Put a value into the latest scope
    pub fn set(self: *Self, key: []const u8, value: Object) void {
        const idx: usize = self.stack.items.len - 1;
        return self.stack.items[idx].put(key, value) catch unreachable;
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
            .function => |f| try stream.print("{s}", .{f.name}),
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
    name: []const u8,
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
