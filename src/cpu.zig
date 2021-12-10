const std = @import("std");
const memory = @import("memory.zig");

pub const Register = u5;

pub const CPU = struct {
    pc: u32,
    regs: [32]u32,

    pub fn make(pc: u32) CPU {
        return .{
            .pc = pc,
            .regs = [_]u32{0} ** 32
        };
    }

    pub fn format(
        self: CPU,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("PC={x} ", .{self.pc});
        for (self.regs) |x, i| {
            try writer.print("R{}={x} ", .{i, x});
        }
    }

    pub fn get(self: *CPU, reg: Register) u32 {
        return self.regs[reg];
    }

    pub fn set(self: *CPU, reg: Register, val: u32) void {
        self.regs[reg] = val;
        self.regs[0] = 0;
    }

    pub fn step(self: *CPU) void {
        const instr = memory.read(self.pc);
        self.pc +%= 4;
        std.log.info("instr={x}", .{instr});
    }
};

pub var cpu = CPU.make(0xbfc00000);
