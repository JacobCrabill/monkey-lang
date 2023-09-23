const std = @import("std");

const ast = struct {
    usingnamespace @import("ast.zig");
    usingnamespace @import("ast/statements.zig");
    usingnamespace @import("ast/expressions.zig");
};

const obj = @import("object.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Object = obj.Object;

pub const BuiltinType = enum(u8) {
    len,
    min,
    max,
};

pub const BuiltinNames = [_][]const u8{
    "len",
    "min",
    "max",
};

pub const BuiltinFn = union(BuiltinType) {
    const Self = @This();
    len: Len,
    min: Null,
    max: Null,

    pub fn call(self: Self, alloc: Allocator, args: anytype) Object {
        return switch (self) {
            inline else => |f| f.call(alloc, args),
        };
    }

    pub fn name(self: Self) []const u8 {
        return switch (self) {
            inline else => |f| f.Name,
        };
    }
};

pub fn isBuiltin(ident: []const u8) bool {
    for (BuiltinNames) |name| {
        if (std.mem.eql(u8, ident, name)) {
            return true;
        }
    }

    return false;
}

pub fn makeBuiltin(ident: []const u8) ?BuiltinFn {
    _ = ident;
    return null;
}

pub const Len = struct {
    const Self = @This();
    pub const Name: []const u8 = "len";

    pub fn call(self: Self, alloc: Allocator, args: anytype) Object {
        _ = self;
        if (@TypeOf(args) != Object)
            return obj.makeNull();

        return obj.makeError(alloc, "'len' not implemented yet!\n");
    }
};

pub const Null = struct {
    const Self = @This();
    pub const Name: []const u8 = "null";

    pub fn call(self: Self, alloc: Allocator, args: anytype) Object {
        _ = args;
        _ = alloc;
        _ = self;
        return obj.makeNull();
    }
};
