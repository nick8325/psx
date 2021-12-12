const std = @import("std");
const memory = @import("memory.zig");

pub const Instruction = union(enum) {
    nop,
    special: struct {
        op: SpecialOp,
        rs1: Register,
        rs2: Register,
        rd: Register
    },
    shift: struct {
        op: ShiftOp,
        rs: Register,
        rd: Register,
        amount: u5
    },
    imm: struct {
        op: ImmediateOp,
        rs: Register,
        rd: Register,
        imm: u16,

        const Self = @This();
        fn immzx(self: Self) u32 {
            return @as(u32, self.imm);
        }

        fn immsx(self: Self) u32 {
            return @bitCast(u32, @as(i32, @bitCast(i16, self.imm)));
        }
    },
    jump: struct {
        op: JumpOp,
        target: u26
    },

    pub fn format(
        self: Instruction,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .nop => try writer.print("NOP", .{}),
            .special => |info| try writer.print("{s} r{}, r{}, r{}", .{@tagName(info.op), info.rd, info.rs1, info.rs2}),
            .shift => |info| try writer.print("{s} r{}, r{}, ${}", .{@tagName(info.op), info.rd, info.rs, info.amount}),
            .imm => |info| try writer.print("{s} r{}, r{}, $0x{x}", .{@tagName(info.op), info.rd, info.rs, info.imm}),
            .jump => |info| try writer.print("{s} $0x{x}", .{@tagName(info.op), info.target})
        }
    }

};

pub const SpecialOp = enum(u6) {
    OR = 0b100101
};

pub const ShiftOp = enum(u6) {
    SLL = 0b000000
};

pub const ImmediateOp = enum(u6) {
    LUI = 0b001111,
    ORI = 0b001101,
    ADDIU = 0b001001,
    SW = 0b101011
};

pub const JumpOp = enum(u6) {
    J = 0b000010
};

pub const DecodingError = error {
    UnknownInstruction
};

fn find(comptime T: type, opcode: @typeInfo(T).Enum.tag_type) ?T {
    inline for (comptime std.enums.values(T)) |op| {
        if (@enumToInt(op) == opcode)
            return op;
    }

    return null;
}

pub fn decode(instr: u32) !Instruction {
    // This is really an SLL
    if (instr == 0) return Instruction{.nop = {}};

    const opcode = @intCast(u6, instr >> 26);
    const rs = @intCast(u5, (instr >> 21) & 0b11111);
    const rt = @intCast(u5, (instr >> 16) & 0b11111);
    const rd = @intCast(u5, (instr >> 11) & 0b11111);
    const shamt = @intCast(u5, (instr >> 6) & 0b11111);
    const funct = @intCast(u6, instr & 0b111111);
    const imm = @intCast(u16, instr & 0xffff);
    const target = @intCast(u26, instr & 0x3ffffff);

    if (opcode == 0) {
        // "Special" instruction.
        if (find(ShiftOp, funct)) |op| {
            if (rs != 0) {
                std.log.err("shift with nonzero rs in {x}", .{instr});
                return error.UnknownInstruction;
            }

            return Instruction{.shift = .{
                .op = op,
                .rs = rt,
                .rd = rd,
                .amount = shamt
            }};
        } else if (find(SpecialOp, funct)) |op| {
            return Instruction{.special = .{
                .op = op,
                .rs1 = rs,
                .rs2 = rt,
                .rd = rd
            }};
        } else {
            std.log.err("unknown funct {x} in {x}", .{funct, instr});
            return error.UnknownInstruction;
        }
    } else if (find(ImmediateOp, opcode)) |op| {
        if (op == .LUI and rs != 0) {
            std.log.err("lui with nonzero rs in {x}", .{instr});
            return error.UnknownInstruction;
        }

        return Instruction{.imm = .{
            .op = op,
            .rs = rs,
            .rd = rt,
            .imm = imm
        }};
    } else if (find(JumpOp, opcode)) |op| {
        return Instruction{.jump = .{
            .op = op,
            .target = target
        }};
    } else {
        std.log.err("unknown opcode {x} in {x}", .{opcode, instr});
        return error.UnknownInstruction;
    }
}

pub const Register = u5;

pub const CPUDiff = struct {
    cpu1: ?CPU,
    cpu2: CPU,

    pub fn init(cpu1: ?CPU, cpu2: CPU) CPUDiff {
        return .{.cpu1 = cpu1, .cpu2 = cpu2};
    }

    pub fn format(
        self: *const CPUDiff,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.cpu1) |cpu1| {
            if (cpu1.pc != self.cpu2.pc)
                try writer.print("PC={x}->{x} ", .{cpu1.pc, self.cpu2.pc});
            for (cpu1.regs) |x, i| {
                if (x != self.cpu2.regs[i])
                    try writer.print("R{}={x}->{x} ", .{i, x, self.cpu2.regs[i]});
            }
        } else try self.cpu2.format(fmt, options, writer);
    }
};

pub const CPU = struct {
    pc: u32,
    regs: [32]u32,
    instruction: u32,

    pub fn init(pc: u32) !CPU {
        return CPU {
            .pc = pc,
            .regs = [_]u32{0} ** 32,
            .instruction = try memory.read(pc)
        };
    }

    pub fn format(
        self: *const CPU,
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

    pub fn get(self: *const CPU, reg: Register) u32 {
        return self.regs[reg];
    }

    pub fn set(self: *CPU, reg: Register, val: u32) void {
        self.regs[reg] = val;
        self.regs[0] = 0;
    }

    pub fn step(self: *CPU) !void {
        const decoded = try decode(self.instruction);
        self.pc +%= 4;
        self.instruction = try memory.read(self.pc);

        std.log.info("{}", .{decoded});
        try self.execute(decoded);
    }

    pub fn execute(self: *CPU, instr: Instruction) !void {
        switch(instr) {
            .nop => {},
            .special => |info| switch(info.op) {
                .OR => self.set(info.rd, self.get(info.rs1) | self.get(info.rs2))
            },
            .shift => |info| switch(info.op) {
                .SLL => self.set(info.rd, self.get(info.rs) << info.amount)
            },
            .imm => |info| switch(info.op) {
                .LUI => self.set(info.rd, info.immzx() << 16),
                .ORI => self.set(info.rd, self.get(info.rs) | info.imm),
                .ADDIU => self.set(info.rd, self.get(info.rs) +% info.immsx()),
                .SW => try memory.write(self.get(info.rs) +% info.immsx(), self.get(info.rd))
            },
            .jump => |info| switch(info.op) {
                .J => self.pc = (self.pc & 0xf0000000) | (@as(u32, info.target) << 2)
            }
        }
    }
};
