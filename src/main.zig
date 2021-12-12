const std = @import("std");
const cpu = @import("cpu.zig");
const memory = @import("memory.zig");

pub fn main() !void {
    while (true) {
        std.log.info("{}", .{cpu.cpu});
        try cpu.cpu.step();
    }
}
