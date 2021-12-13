const std = @import("std");

/// Read a file into an array of type [_]T, at compile time.
pub fn readArray(comptime T: type, comptime path: []const u8) [@embedFile(path).len/@sizeOf(T)]T {
    const cstr = @embedFile(path).*;
    const n = cstr.len;
    const bytearr = @bitCast([n+1]u8, cstr)[0..n].*;
    return @bitCast([n/@sizeOf(T)]T, bytearr);
}

/// Convert an integer to an enum.
/// Like @intToEnum, but returns null on failure.
pub fn intToEnum(comptime T: type, opcode: std.meta.Tag(T)) ?T {
    inline for (comptime std.enums.values(T)) |op| {
        if (@enumToInt(op) == opcode)
            return op;
    }

    return null;
}

