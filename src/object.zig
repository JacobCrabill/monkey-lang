const std = @import("std");

pub const ObjectType = enum(u8) {
    none,
    integer,
    boolean,
    function,
};

pub const Object = union(ObjectType) {
    const Self = @This();
    none: Null,
    integer: Integer,
    boolean: Boolean,
    function: Function,

    pub fn print(self: Self, stream: anytype) !void {
        switch (self) {
            .none => try stream.print("null", .{}),
            .integer => |i| try stream.print("{d}", .{i.value}),
            .boolean => |b| try stream.print("{any}", .{b.value}),
            .function => |f| try stream.print("{s}", .{f.name}),
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
