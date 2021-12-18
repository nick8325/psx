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
    OR = 0b100101,
    SLTU = 0b101011,
    ADDU = 0b100001,
    JR = 0b001000
};

/// List of shift-type instructions.
pub const ShiftOp = enum(u6) {
    SLL = 0b000000,
    SRL = 0b000010
};

/// List of immediate-type instructions.
pub const ImmediateOp = enum(u6) {
    LUI = 0b001111,
    ORI = 0b001101,
    ADDIU = 0b001001,
    SW = 0b101011,
    BNE = 0b000101,
    ADDI = 0b001000,
    LW = 0b100011,
    SH = 0b101001,
    ANDI = 0b001100,
    SB = 0b101000,
    LB = 0b100000,
    BEQ = 0b00100,
};

/// List of jump-type instructions.
pub const JumpOp = enum(u6) {
    J = 0b000010,
    JAL = 0b000011
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

            if (op == .JR and (rt.val != 0 or rd.val != 0)) {
                std.log.err("JR with nonzero rt or rd in {x}", .{instr});
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
    InvalidCOP, // TODO: we return this also on e.g. reads/writes of unknown COP0 registers - is this right?
    Overflow
};

/// The state of the system coprocessor.
pub const COP0 = struct {
    const SR = struct {
        value: u32,

        const Self = @This();

        // 1 bit: this bit is not allowed to be modified
        const mask:  u32 = 0b00001111101101000000000011000000;
        // Initial value of SR.
        const init_value: u32 = 0b00000000010000000000000000101010;

        pub fn init() Self {
            // BEV=1, KU[*]=1, everything else 0
            return .{.value = init_value};
        }

        pub fn get(self: Self) u32 {
            return self.value;
        }

        pub fn set(self: *Self, val: u32) !void {
            if ((val & mask) != (init_value & mask)) {
                std.log.err("write to read-only bit in COP0.SR", .{});

                var i: usize = 0;
                var bit: u32 = 1;
                while (i < 32) {
                    if ((mask & bit) != 0 and (val & bit) != (init_value & bit))
                        std.log.err("bit {} is {x}, should be {x}", .{i, val & bit, init_value & bit});
                    i += 1;
                    bit *%= 2;
                }
                return error.InvalidCOP;
            }

            self.value = val;
        }

        pub fn bev(self: Self) bool {
            return utils.testBit(self.value, 22);
        }

        pub fn cm(self: Self) bool {
            return utils.testBit(self.value, 19);
        }

        pub fn swc(self: Self) bool {
            return utils.testBit(self.value, 17);
        }

        pub fn isc(self: Self) bool {
            return utils.testBit(self.value, 16);
        }

        pub fn im(self: Self) [8] bool {
            return .{
                utils.testBit(self.value, 8),
                utils.testBit(self.value, 9),
                utils.testBit(self.value, 10),
                utils.testBit(self.value, 11),
                utils.testBit(self.value, 12),
                utils.testBit(self.value, 13),
                utils.testBit(self.value, 14),
                utils.testBit(self.value, 15)
            };
        }

        pub fn ie(self: Self) [3] bool {
            return .{
                utils.testBit(self.value, 0),
                utils.testBit(self.value, 2),
                utils.testBit(self.value, 4)
            };
        }
    };

    sr: SR,
    cause: u32,
    epc: u32,
    badvaddr: u32,
    // We don't really use these ones, but they can be get and set.
    // Details from Nocash PSX.
    bpc: u32,
    bda: u32,
    dcic: u32,
    bdam: u32,
    bpcm: u32,

    fn init() COP0 {
        return .{
            .sr = SR.init(),
            .cause = 0,
            .epc = 0,
            .badvaddr = 0,
            .bpc = 0,
            .bda = 0,
            .dcic = 0,
            .bdam = 0,
            .bpcm = 0
        };
    }

    pub fn get(self: *const COP0, reg: CoRegister) !u32 {
        switch(reg.val) {
            3 => return self.bpc,
            5 => return self.bda,
            7 => return self.dcic,
            9 => return self.bdam,
            11 => return self.bpcm,
            8 => return self.badvaddr,
            12 => return self.sr.get(),
            13 => return self.cause,
            14 => return self.epc,
            15 => return 2, // PRId - value from Nocash PSX
            else => {
                std.log.err("unknown COP0 register read {}", .{reg.val});
                return error.InvalidCOP;
            }
        }
    }

    pub fn set(self: *COP0, reg: CoRegister, val: u32) !void {
        switch (reg.val) {
            3 => self.bpc = val,
            5 => self.bda = val,
            6 => {}, // JUMPDEST
            7 => self.dcic = val,
            9 => self.bdam = val,
            11 => self.bpcm = val,
            12 => try self.sr.set(val),
            13 => {
                // According to "Playstation Emulation Guide",
                // bits 8 and 9 are writable.
                self.cause = (self.cause & ~@as(u32, 0x30)) | (val & 0x30);
            },
            else => {
                std.log.err("unknown COP0 register write {}", .{reg.val});
                return error.InvalidCOP;
            }
        }
    }
  };

/// The state of the CPU.
pub const CPU = struct {
    /// Current PC.
    pc: u32,
    /// Next value of PC. Used to implement branch delay slot.
    next_pc: u32,
    /// Registers.
    regs: [32]u32,
    /// COP0 registers.
    cop0: COP0,

    pub fn init(pc: u32) CPU {
        return CPU {
            .pc = pc,
            .next_pc = pc +% 4,
            .regs = .{0} ** 32,
            .cop0 = COP0.init()
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
        try writer.print("COP0.SR={x} ", .{self.cop0.sr.get()});
        try writer.print("COP0.CAUSE={x} ", .{self.cop0.cause});
        try writer.print("COP0.EPC={x} ", .{self.cop0.epc});
        try writer.print("COP0.BADVADDR={x} ", .{self.cop0.badvaddr});
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
        const instr = try decode(try memory.read(u32, self.pc));
        // The execute function is in charge of updating pc and next_pc.
        try self.execute(instr);
    }

    /// Execute a single, decoded instruction.
    pub fn execute(self: *CPU, instr: Instruction) !void {
        var new_pc: u32 = self.next_pc +% 4;

        switch(instr) {
            .nop => {},
            .reg => |info| switch(info.op) {
                .OR => self.set(info.dest, self.get(info.src1) | self.get(info.src2)),
                .SLTU => self.set(info.dest, if (self.get(info.src1) < self.get(info.src2)) 1 else 0),
                .ADDU => self.set(info.dest, self.get(info.src1) + self.get(info.src2)),
                .JR => {
                    new_pc = self.get(info.src1);
                }
            },
            .shift => |info| switch(info.op) {
                .SLL => self.set(info.dest, self.get(info.src) << info.amount),
                .SRL => self.set(info.dest, self.get(info.src) >> info.amount)
            },
            .imm => |info| switch(info.op) {
                .LUI => self.set(info.dest, info.zImm() << 16),
                .ORI => self.set(info.dest, self.get(info.src) | info.zImm()),
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
                .SW => try memory.write(u32, self.get(info.src) +% info.sImm(), self.get(info.dest)),
                .LW => self.set(info.dest, try memory.read(u32, self.get(info.src) +% info.sImm())),
                .BNE => {
                    if (self.get(info.src) != self.get(info.dest))
                        new_pc = self.next_pc +% (info.sImm() << 2);
                },
                .BEQ => {
                    if (self.get(info.src) == self.get(info.dest))
                        new_pc = self.next_pc +% (info.sImm() << 2);
                },
                .SH => try memory.write(u16, self.get(info.src) +% info.sImm(), @intCast(u16, self.get(info.dest) & 0xffff)),
                .SB => try memory.write(u8, self.get(info.src) +% info.sImm(), @intCast(u8, self.get(info.dest) & 0xff)),
                .ANDI => self.set(info.dest, self.get(info.src) & info.zImm()),
                .LB => self.set(info.dest, @bitCast(u32, @as(i32, try memory.read(i8, self.get(info.src) +% info.sImm())))),
            },
            .jump => |info| switch(info.op) {
                .J => {
                    new_pc = (self.next_pc & 0xf0000000) | (@as(u32, info.target) << 2);
                },
                .JAL => {
                    new_pc = (self.next_pc & 0xf0000000) | (@as(u32, info.target) << 2);
                    self.set(.{.val = 31}, self.next_pc +% 4);
                }
            },
            .mtc => |info| switch(info.cop) {
                0 => try self.cop0.set(info.dest, self.get(info.src)),
                else => {
                    std.log.err("invalid cop {} in {x}", .{info.cop, instr});
                }
            },
            .mfc => |info| switch(info.cop) {
                0 => self.set(info.dest, try self.cop0.get(info.src)),
                else => {
                    std.log.err("invalid cop {} in {x}", .{info.cop, instr});
                }
            }
        }

        self.pc = self.next_pc;
        self.next_pc = new_pc;
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
            if (cpu1.cop0.sr.get() != self.cpu2.cop0.sr.get())
                try writer.print("COP0.SR={x}->{x} ", .{cpu1.cop0.sr.get(), self.cpu2.cop0.sr.get()});
            if (cpu1.cop0.cause != self.cpu2.cop0.cause)
                try writer.print("COP0.CAUSE={x}->{x} ", .{cpu1.cop0.cause, self.cpu2.cop0.cause});
            if (cpu1.cop0.epc != self.cpu2.cop0.epc)
                try writer.print("COP0.EPC={x}->{x} ", .{cpu1.cop0.epc, self.cpu2.cop0.epc});
            if (cpu1.cop0.badvaddr != self.cpu2.cop0.badvaddr)
                try writer.print("COP0.BADVADDR={x}->{x} ", .{cpu1.cop0.badvaddr, self.cpu2.cop0.badvaddr});
        } else try self.cpu2.format(fmt, options, writer);
    }
};
