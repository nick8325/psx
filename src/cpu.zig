const std = @import("std");
const memory = @import("memory.zig");
const utils = @import("utils.zig");

/// A register is a 5-bit integer.
pub const Register = struct { val: u5 };
/// A coprocessor register.
pub const CoRegister = struct { val: u5 };

/// A decoded instruction.
pub const Instruction = union(enum) {
    /// No-operation.
    nop,
    /// Three-register instructions.
    reg: struct {
        op: RegisterOp,
        src1: Register,
        src2: Register,
        dest: Register
    },
    /// Shift instructions.
    shift: struct {
        op: ShiftOp,
        src: Register,
        dest: Register,
        amount: u5
    },
    /// Instructions with an immediate operand.
    imm: struct {
        op: ImmediateOp,
        src: Register,
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
    /// Move to/from coprocessor.
    mtc: struct {
        cop: u2,
        src: Register,
        dest: CoRegister
    },
    mfc: struct {
        cop: u2,
        src: CoRegister,
        dest: Register
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
            .reg => |info| try writer.print("{s} r{}, r{}, r{}", .{@tagName(info.op), info.dest.val, info.src1.val, info.src2.val}),
            .shift => |info| try writer.print("{s} r{}, r{}, ${}", .{@tagName(info.op), info.dest.val, info.src.val, info.amount}),
            .imm => |info| try writer.print("{s} r{}, r{}, $0x{x}", .{@tagName(info.op), info.dest.val, info.src.val, info.imm}),
            .jump => |info| try writer.print("{s} $0x{x}", .{@tagName(info.op), info.target}),
            .mtc => |info| try writer.print("MTC{} c{}, r{}", .{info.cop, info.dest.val, info.src.val}),
            .mfc => |info| try writer.print("MFC{} r{}, c{}", .{info.cop, info.dest.val, info.src.val})
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
    SW = 0b101011,
    BNE = 0b000101,
    ADDI = 0b001000
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
                .src1 = rs,
                .src2 = rt,
                .dest = rd
            }};
        } else if (utils.intToEnum(ShiftOp, funct)) |op| {
            if (rs.val != 0) {
                std.log.err("shift with nonzero rs in {x}", .{instr});
                return error.UnknownInstruction;
            }

            return Instruction{.shift = .{
                .op = op,
                .src = rt,
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
            .src = rs,
            .dest = rt,
            .imm = imm
        }};
    } else if (utils.intToEnum(JumpOp, opcode)) |op| {
        return Instruction{.jump = .{
            .op = op,
            .target = target
        }};
    } else if ((opcode & 0b111100) == 0b010000) {
        // COP instruction.
        const cop = @intCast(u2, opcode & 0b11);
        if (rs.val == 0b00100 and shift == 0 and funct == 0) {
            return Instruction{.mtc = .{
                .cop = cop,
                .src = rt,
                .dest = .{.val = rd.val}
            }};
        } else if (rs.val == 0 and shift == 0 and funct == 0) {
            return Instruction{.mfc = .{
                .cop = cop,
                .src = .{.val = rd.val},
                .dest = rt
            }};
        } else {
            std.log.err("unknown COP instruction in {x}", .{instr});
            return error.UnknownInstruction;
        }
    } else {
        std.log.err("unknown opcode {x} in {x}", .{opcode, instr});
        return error.UnknownInstruction;
    }
}

/// Errors returned during execution
pub const ExecutionError = error {
    InvalidCOP,
    Overflow
};

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
    /// Coprocessor 0 registers.
    cop0: [32]u32,

    pub fn init(pc: u32) CPU {
        return CPU {
            .pc = pc,
            .regs = [_]u32{0} ** 32,
            .instruction = 0,
            .cop0 = [_]u32{0} ** 32,
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
        for (self.cop0) |x, i| {
            try writer.print("COP0.R{}={x} ", .{i, x});
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

    /// Get the value of a COP0 register.
    pub fn getCOP0(self: *const CPU, reg: CoRegister) u32 {
        return self.cop0[reg.val];
    }

    /// Set the value of a COP0 register.
    pub fn setCOP0(self: *CPU, reg: CoRegister, val: u32) void {
        self.cop0[reg.val] = val;
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
                .OR => self.set(info.dest, self.get(info.src1) | self.get(info.src2))
            },
            .shift => |info| switch(info.op) {
                .SLL => self.set(info.dest, self.get(info.src) << info.amount)
            },
            .imm => |info| switch(info.op) {
                .LUI => self.set(info.dest, info.zImm() << 16),
                .ORI => self.set(info.dest, self.get(info.src) | info.imm),
                .ADDIU => self.set(info.dest, self.get(info.src) +% info.sImm()),
                .ADDI => {
                    const src1 = @bitCast(i32, self.get(info.src));
                    const src2 = @bitCast(i32, info.sImm());
                    var dest: i32 = undefined;
                    if (@addWithOverflow(i32, src1, src2, &dest))
                        return error.Overflow
                    else
                        self.set(info.dest, @bitCast(u32, dest));
                },
                .SW => try memory.write(self.get(info.src) +% info.sImm(), self.get(info.dest)),
                .BNE => {
                    // TO DO: check if this branches to the right place
                    if (self.get(info.src) != self.get(info.dest))
                        self.pc +%= info.sImm() << 2;
                }
            },
            .jump => |info| switch(info.op) {
                .J => self.pc = (self.pc & 0xf0000000) | (@as(u32, info.target) << 2)
            },
            .mtc => |info| switch(info.cop) {
                0 => self.setCOP0(info.dest, self.get(info.src)),
                else => {
                    std.log.err("invalid cop {} in {x}", .{info.cop, instr});
                }
            },
            .mfc => |info| switch(info.cop) {
                0 => self.set(info.dest, self.getCOP0(info.src)),
                else => {
                    std.log.err("invalid cop {} in {x}", .{info.cop, instr});
                }
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
            for (cpu1.cop0) |x, i| {
                if (x != self.cpu2.cop0[i])
                    try writer.print("COP0.R{}={x}->{x} ", .{i, x, self.cpu2.cop0[i]});
            }
        } else try self.cpu2.format(fmt, options, writer);
    }
};
