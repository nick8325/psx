const std = @import("std");
const CPU = @import("cpu.zig");
const memory = @import("memory.zig");

pub fn main() !void {
    //var i: usize = 0;
    try main2();
//while (i < 100) : (i += 1) { main2() catch {}; }
}

pub noinline fn main2() !void {
    try memory.init();
    var old_cpu: ?CPU.CPU = null;
    var cpu = CPU.CPU.init(0xbfc00000);
    while (true) {
        const instr = CPU.decode(try cpu.fetch());
        old_cpu = cpu;
        std.log.debug("{}", .{instr});
        try cpu.step();
        std.log.debug("{}", .{CPU.CPUDiff.init(old_cpu, cpu)});
    }
}
