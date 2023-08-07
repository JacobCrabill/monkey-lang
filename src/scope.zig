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
    const Entry = struct {
        key: []const u8,
        value: Object,
    };
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
        self.* = undefined;
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
        if (self.contains(key)) {
            self.map.put(key, value.clone()) catch unreachable;
        } else {
            const owned_key: []u8 = self.alloc.dupe(u8, key) catch unreachable;
            self.map.put(owned_key, value.clone()) catch unreachable;
            self.keys.insert(owned_key) catch unreachable;
        }
    }

    /// Check if the key exists in the map
    pub fn contains(self: Self, key: []const u8) bool {
        return self.keys.contains(key);
    }

    /// DEBUG
    pub fn print(self: Self) void {
        var iter = self.keys.iterator();
        while (iter.next()) |pkey| {
            std.debug.print("{s}\n", .{pkey});
        }
    }
};
