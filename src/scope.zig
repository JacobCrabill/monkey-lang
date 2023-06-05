const std = @import("std");

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/statements.zig");
    usingnamespace @import("ast/expressions.zig");
};

const Object = @import("object.zig").Object;

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

    /// Clear our map and free all allocated memory
    pub fn deinit(self: *Self) void {
        var iter = self.map.iterator();
        while (iter.next()) |kv| {
            self.alloc.free(kv.key_ptr.*);
            kv.value_ptr.deinit();
        }

        self.map.deinit();
        self.keys.deinit();
    }

    /// Clones the Scope, including all heap allocations
    /// Caller owns new heap allocations
    pub fn clone(self: Self) Self {
        var scope = Scope.init(self.alloc);

        // Use the set() method to perform our heap copies
        var iter = self.map.iterator();
        while (iter.next()) |kv| {
            scope.set(kv.key_ptr.*, kv.value_ptr.*);
        }

        return scope;
    }

    /// Get a copy of the value at 'key', or 'null'
    pub fn get(self: Self, key: []const u8) ?Object {
        return self.map.get(key);
    }

    /// Add an entry to the current Scope.
    /// Given value is copied upon insertion.
    pub fn set(self: *Self, key: []const u8, value: Object) void {
        const owned_key = self.alloc.alloc(u8, key.len) catch unreachable;
        @memcpy(owned_key, key);
        self.map.put(owned_key, value.clone()) catch unreachable;
        self.keys.insert(key) catch unreachable;
    }

    /// Check if the key exists in the map
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
        self.idx = 0;
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

    /// Push the given Scope object onto the stack
    pub fn push(self: *Self, scope: Scope) void {
        self.stack.append(scope) catch unreachable;
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

    /// Get a value from the latest scope, or a parent scope if n/a
    pub fn get(self: *Self, key: []const u8) ?Object {
        var i: usize = 0;
        while (i <= self.idx) : (i += 1) {
            if (self.stack.items[self.idx - i].get(key)) |obj| {
                return obj;
            }
        }

        return null;
    }

    /// Put a value into the latest scope
    /// Note: Value is copied upon insertion
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
