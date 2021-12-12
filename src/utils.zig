/// Read a file into an array of type [_]T, at compile time.
pub fn readArray(comptime T: type, comptime path: []const u8) [@embedFile(path).len/@sizeOf(T)]T {
    const cstr = @embedFile(path).*;
    const n = cstr.len;
    const bytearr = @bitCast([n+1]u8, cstr)[0..n].*;
    return @bitCast([n/@sizeOf(T)]T, bytearr);
}
