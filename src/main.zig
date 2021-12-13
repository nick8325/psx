const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const CPUDiff = @import("cpu.zig").CPUDiff;
const memory = @import("memory.zig");

pub fn main() !void {
    var old_cpu: ?CPU = null;
    var cpu = CPU.init(0xbfc00000);
    while (true) {
        std.log.info("{}", .{CPUDiff.init(old_cpu, cpu)});
        old_cpu = cpu;
        try cpu.step();
    }
}
