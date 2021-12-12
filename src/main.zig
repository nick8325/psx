const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const memory = @import("memory.zig");

pub fn main() !void {
    var cpu = try CPU.init(0xbfc00000);
    while (true) {
        std.log.info("{}", .{cpu});
        try cpu.step();
    }
}
