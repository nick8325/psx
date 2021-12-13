const std = @import("std");
const memory = @import("memory.zig");
const utils = @import("utils.zig");

/// A register is a 5-bit integer.
pub const Register = struct { val: u5 };

/// A decoded instruction.
pub const Instruction = union(enum) {
    /// No-operation.
    nop,
    /// Three-register instructions.
    reg: struct {
        op: RegisterOp,
        source1: Register,
        source2: Register,
        dest: Register
    },
    /// Shift instructions.
    shift: struct {
        op: ShiftOp,
        source: Register,
        dest: Register,
        amount: u5
    },
    /// Instructions with an immediate operand.
    imm: struct {
        op: ImmediateOp,
        source: Register,
        dest: Register,
        imm: u16,

        const Self = @This();
        /// Return the operand zero-extended to 32 bits.
        fn zImm(self: Self) u32 {
            return @as(u32, self.imm);
        }

        /// Return the operand sign-extended to 32 bits.
        fn sImm(self: Self) u32 {
            return @bitCast(u32, @as(i32, @bitCast(i16, self.imm)));
        }
    },
    /// Instructions with a jump target.
    jump: struct {
        op: JumpOp,
        target: u26,

        const Self = @This();

        /// Resolve the jump target as an absolute jump.
        fn absTarget(self: Self, pc: u32) u32 {
            return (pc & 0xf0000000) | (@as(u32, self.target) << 2);
        }
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
            .reg => |info| try writer.print("{s} r{}, r{}, r{}", .{@tagName(info.op), info.dest.val, info.source1.val, info.source2.val}),
            .shift => |info| try writer.print("{s} r{}, r{}, ${}", .{@tagName(info.op), info.dest.val, info.source.val, info.amount}),
            .imm => |info| try writer.print("{s} r{}, r{}, $0x{x}", .{@tagName(info.op), info.dest.val, info.source.val, info.imm}),
            .jump => |info| try writer.print("{s} $0x{x}", .{@tagName(info.op), info.target})
        }
    }

};

/// List of register-type instructions.
pub const RegisterOp = enum(u6) {
    OR = 0b100101
};

/// List of shift-type instructions.
pub const ShiftOp = enum(u6) {
    SLL = 0b000000
};

/// List of immediate-type instructions.
pub const ImmediateOp = enum(u6) {
    LUI = 0b001111,
    ORI = 0b001101,
    ADDIU = 0b001001,
    SW = 0b101011
};

/// List of jump-type instructions.
pub const JumpOp = enum(u6) {
    J = 0b000010
};

/// Errors returned by the decode function.
pub const DecodingError = error {
    UnknownInstruction
};

/// Decode an instruction.
pub fn decode(instr: u32) !Instruction {
    // Extract the different parts of the instruction.
    // Each instruction uses only some of these fields.
    const opcode = @intCast(u6, instr >> 26);
    const rs = Register{.val = @intCast(u5, (instr >> 21) & 0b11111)};
    const rt = Register{.val = @intCast(u5, (instr >> 16) & 0b11111)};
    const rd = Register{.val = @intCast(u5, (instr >> 11) & 0b11111)};
    const shift = @intCast(u5, (instr >> 6) & 0b11111);
    const funct = @intCast(u6, instr & 0b111111);
    const imm = @intCast(u16, instr & 0xffff);
    const target = @intCast(u26, instr & 0x3ffffff);

    // NOP is coded as SLL r0, r0, $0, but let's treat it specially
    if (instr == 0) return Instruction{.nop = {}}
    else if (opcode == 0) { // Actual opcode is found in funct
        if (utils.intToEnum(RegisterOp, funct)) |op| {
            if (shift != 0) {
                std.log.err("register op with nonzero shift in {x}", .{instr});
                return error.UnknownInstruction;
            }

            return Instruction{.reg = .{
                .op = op,
                .source1 = rs,
                .source2 = rt,
                .dest = rd
            }};
        } else if (utils.intToEnum(ShiftOp, funct)) |op| {
            if (rs.val != 0) {
                std.log.err("shift with nonzero rs in {x}", .{instr});
                return error.UnknownInstruction;
            }

            return Instruction{.shift = .{
                .op = op,
                .source = rt,
                .dest = rd,
                .amount = shift
            }};
        } else {
            std.log.err("unknown funct {x} in {x}", .{funct, instr});
            return error.UnknownInstruction;
        }
    } else if (utils.intToEnum(ImmediateOp, opcode)) |op| {
        if (op == .LUI and rs.val != 0) {
            std.log.err("lui with nonzero rs in {x}", .{instr});
            return error.UnknownInstruction;
        }

        return Instruction{.imm = .{
            .op = op,
            .source = rs,
            .dest = rt,
            .imm = imm
        }};
    } else if (utils.intToEnum(JumpOp, opcode)) |op| {
        return Instruction{.jump = .{
            .op = op,
            .target = target
        }};
    } else {
        std.log.err("unknown opcode {x} in {x}", .{opcode, instr});
        return error.UnknownInstruction;
    }
}

/// The state of the CPU.
pub const CPU = struct {
    /// Current PC.
    pc: u32,
    /// Registers.
    regs: [32]u32,
    /// The next instruction to be executed.
    /// Used to implement the branch delay slot.
    /// The instruction at PC will be executed after this one.
    instruction: u32,

    pub fn init(pc: u32) CPU {
        return CPU {
            .pc = pc,
            .regs = [_]u32{0} ** 32,
            .instruction = 0
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

    /// Get the value of a register.
    pub fn get(self: *const CPU, reg: Register) u32 {
        return self.regs[reg.val];
    }

    /// Set the value of a register.
    /// Writes to register 0 are ignored.
    pub fn set(self: *CPU, reg: Register, val: u32) void {
        self.regs[reg.val] = val;
        self.regs[0] = 0;
    }

    /// Fetch and execute the next instruction.
    pub fn step(self: *CPU) !void {
        const decoded = try decode(self.instruction);

        // Fetch the next instruction and advance PC.
        // Do this before executing the instruction so that
        // jump instructions can set PC correctly.
        self.instruction = try memory.read(self.pc);
        self.pc +%= 4;

        std.log.info("{}", .{decoded});
        try self.execute(decoded);
    }

    /// Execute a single, decoded instruction.
    pub fn execute(self: *CPU, instr: Instruction) !void {
        switch(instr) {
            .nop => {},
            .reg => |info| switch(info.op) {
                .OR => self.set(info.dest, self.get(info.source1) | self.get(info.source2))
            },
            .shift => |info| switch(info.op) {
                .SLL => self.set(info.dest, self.get(info.source) << info.amount)
            },
            .imm => |info| switch(info.op) {
                .LUI => self.set(info.dest, info.zImm() << 16),
                .ORI => self.set(info.dest, self.get(info.source) | info.imm),
                .ADDIU => self.set(info.dest, self.get(info.source) +% info.sImm()),
                .SW => try memory.write(self.get(info.source) +% info.sImm(), self.get(info.dest))
            },
            .jump => |info| switch(info.op) {
                .J => self.pc = (self.pc & 0xf0000000) | (@as(u32, info.target) << 2)
            }
        }
    }
};

/// Printing the value CPUDiff.init(cpu1, cpu2)
/// shows the difference in CPU state between cpu1 and cpu2.
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
