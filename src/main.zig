const std = @import("std");
const cpu = @import("cpu.zig");
const memory = @import("memory.zig");

pub fn main() !void {
    std.log.info("{}", .{cpu.cpu});
    cpu.cpu.step();
    std.log.info("{}", .{cpu.cpu});
    cpu.cpu.step();
    std.log.info("{}", .{cpu.cpu});
    cpu.cpu.step();
    std.log.info("{}", .{cpu.cpu});
    cpu.cpu.step();
    std.log.info("{}", .{cpu.cpu});
    cpu.cpu.step();
}
