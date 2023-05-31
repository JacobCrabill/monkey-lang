const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Map = std.StringHashMap;
const BufSet = std.BufSet;

pub const Scope = Map(Object);

pub const ScopeStack = struct {
    const Self = @This();
    alloc: Allocator,
    stack: ArrayList(Scope),
    keys: BufSet,
    idx: usize,

    /// Create a new instance with a single empty Scope
    pub fn init(alloc: Allocator) Self {
        var scope = Self{
            .alloc = alloc,
            .stack = ArrayList(Scope).init(alloc),
            .keys = BufSet.init(alloc),
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

    /// Get a value from the latest scope
    pub fn get(self: *Self, key: []const u8) ?Object {
        return self.stack.getLast().get(key);
    }

    /// Put a value into the latest scope
    pub fn set(self: *Self, key: []const u8, value: Object) void {
        // StringHashMap doesn't copy the key
        // Since the buffer where the key is coming from may change at any time,
        // copy the contents of 'key' locally (and free later)
        // TODO: remove unused keys from popped stacks
        const owned_key = self.alloc.alloc(u8, key.len) catch unreachable;
        @memcpy(owned_key, key);
        self.keys.insert(owned_key) catch unreachable;

        self.stack.items[self.idx].put(owned_key, value) catch unreachable;
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
