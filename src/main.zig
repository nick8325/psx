const std = @import("std");
const CPU = @import("cpu.zig");
const memory = @import("memory.zig");

pub fn main() !void {
    var old_cpu: ?CPU.CPU = null;
    var cpu = CPU.CPU.init(0xbfc00000);
    while (true) {
        const instr = CPU.decode(try memory.read(u32, cpu.pc));
        old_cpu = cpu;
        defer std.log.info("{} {}", .{instr, CPU.CPUDiff.init(old_cpu, cpu)});
        try cpu.step();
    }
}
