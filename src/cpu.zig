const std = @import("std");
const memory = @import("memory.zig");
const utils = @import("utils.zig");

//////////////////////////////////////////////////////////////////////
// Instruction decoding.
//////////////////////////////////////////////////////////////////////

/// Errors returned by the decode function.
pub const DecodingError = error {
    UnknownInstruction
};

/// Decode an instruction.
pub fn decode(instr: u32) !Instruction {
    const parts = InstructionParts.make(instr);

    inline for (@typeInfo(Instruction).Union.fields) |field| {
        if (field.field_type.decode(parts)) |val|
            return @unionInit(Instruction, field.name, val);
    }

    std.log.err("unknown instruction {x}: {}", .{instr, parts});
    return error.UnknownInstruction;
}

/// A decoded instruction.
pub const Instruction = union(enum) {
    NOP: struct {
        const Self = @This();

        fn decode(parts: InstructionParts) ?Self {
            if (parts.val == 0)
                return Self{}
            else
                return null;
        }
    },
    SLL: ShiftType(0),
    SRL: ShiftType(2),
    SRA: ShiftType(3),
    SLLV: RegisterType(4),
    SRLV: RegisterType(6),
    SRAV: RegisterType(7),
    JR: RSType(8),
    JALR: RSDType(9),
    SYSCALL: DontCareType(12),
    BREAK: DontCareType(13),
    MFHI: RDType(16),
    MTHI: RSType(17),
    MFLO: RDType(18),
    MTLO: RSType(19),
    MULT: RSTType(24),
    MULTU: RSTType(25),
    DIV: RSTType(26),
    DIVU: RSTType(27),
    ADD: RegisterType(32),
    ADDU: RegisterType(33),
    SUB: RegisterType(34),
    SUBU: RegisterType(35),
    AND: RegisterType(36),
    OR: RegisterType(37),
    XOR: RegisterType(38),
    NOR: RegisterType(39),
    SLT: RegisterType(42),
    SLTU: RegisterType(43),
    BLTZ: RegImmBranchType(0),
    BGEZ: RegImmBranchType(1),
    BLTZAL: RegImmBranchType(16),
    BGEZAL: RegImmBranchType(17),
    J: JumpType(2),
    JAL: JumpType(3),
    BEQ: BranchType(4),
    BNE: BranchType(5),
    BLEZ: BranchZeroType(6),
    BGTZ: BranchZeroType(7),
    ADDI: SignedImmediateType(8),
    ADDIU: SignedImmediateType(9),
    SUBI: SignedImmediateType(10),
    SUBIU: SignedImmediateType(11),
    ANDI: UnsignedImmediateType(12),
    ORI: UnsignedImmediateType(13),
    XORI: UnsignedImmediateType(14),
    LUI: struct {
        /// ImmediateType(15, false) but rs is unused
        const Self = @This();

        dest: Register,
        imm: UnsignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 15)
                return Self{ .dest = parts.rt, .imm = parts.uimm }
            else
                return null;
        }
    },
    LB: SignedImmediateType(32),
    LH: SignedImmediateType(33),
    LWL: SignedImmediateType(34),
    LW: SignedImmediateType(35),
    LBU: SignedImmediateType(36),
    LHU: SignedImmediateType(37),
    LWR: SignedImmediateType(38),
    SB: SignedImmediateType(40),
    SH: SignedImmediateType(41),
    SWL: SignedImmediateType(42),
    SW: SignedImmediateType(43),
    SWR: SignedImmediateType(46),

    MFC0: struct {
        const Self = @This();
        dest: Register,
        src: CoRegister,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 16 and parts.rs.val == 0 and parts.shift == 0 and parts.func == 0)
                return Self{.dest = parts.rt, .src = .{.val = parts.rd.val}}
            else
                return null;
        }
    },
    MTC0: struct {
        const Self = @This();
        dest: CoRegister,
        src: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 16 and parts.rs.val == 4 and parts.shift == 0 and parts.func == 0)
                return Self{.src = parts.rt, .dest = .{.val = parts.rd.val}}
            else
                return null;
        }
    },

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.writeAll(@tagName(self));
        inline for (@typeInfo(Instruction).Union.fields) |alt| {
            const tag = comptime std.meta.stringToEnum(@typeInfo(Instruction).Union.tag_type.?, alt.name).?;

            switch (self) {
                tag => |args| {
                    var first = true;

                    inline for (std.meta.fields(alt.field_type)) |field| {
                        const arg = @field(args, field.name);
                        if (first)
                            try writer.print(" {}", .{arg})
                        else
                            try writer.print(", {}", .{arg});
                        first = false;
                    }
                },
                else => {}
            }
        }

    }
};

/// The low-level decoding of an instruction.
const InstructionParts = struct {
    // Every instruction has a val and opcode and:
    // - rs+rt+rd+shift+func, or
    // - rs+rt+simm, or
    // - rs+rt+uimm, or
    // - target.
    val: u32,
    opcode: u6,
    rs: Register,
    rt: Register,
    rd: Register,
    shift: u5,
    func: u6,
    simm: SignedImmediate,
    uimm: UnsignedImmediate,
    target: JumpTarget,

    fn make(instr: u32) InstructionParts {
        return .{
            .val = instr,
            .opcode = @intCast(u6, instr >> 26),
            .rs = Register{.val = @intCast(u5, (instr >> 21) & 0b11111)},
            .rt = Register{.val = @intCast(u5, (instr >> 16) & 0b11111)},
            .rd = Register{.val = @intCast(u5, (instr >> 11) & 0b11111)},
            .shift = @intCast(u5, (instr >> 6) & 0b11111),
            .func = @intCast(u6, instr & 0b111111),
            .simm = SignedImmediate.make(@intCast(u16, instr & 0xffff)),
            .uimm = UnsignedImmediate.make(@intCast(u16, instr & 0xffff)),
            .target = JumpTarget{.val = @intCast(u26, instr & 0x3ffffff)},
        };
    }
};

/// The operand of a jump-type instruction.
const JumpTarget = struct {
    val: u26,

    /// Resolve the jump target as an absolute jump.
    fn absTarget(self: JumpTarget, pc: u32) u32 {
        return (pc & 0xf0000000) | (@as(u32, self.val) << 2);
    }

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("$0x{x}", .{self.val});
    }
};

/// A signed immediate operand.
const SignedImmediate = struct {
    val: u32,

    fn make(val: u16) SignedImmediate {
        return .{.val = @bitCast(u32, @as(i32, @bitCast(i16, val)))};
    }

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("$0x{x}", .{self.val});
    }
};

/// An unsigned immediate operand.
const UnsignedImmediate = struct {
    val: u32,

    fn make(val: u16) UnsignedImmediate {
        return .{.val = @as(u32, val)};
    }

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("$0x{x}", .{self.val});
    }
};

/// Instruction decoder for instructions with "don't-care" rs/rt/rd/shift.
fn DontCareType(comptime func: u6) type {
    return struct {
        const Self = @This();

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func)
                return Self{}
            else
                return null;
        }
    };
}

/// Instruction decoder for register-type instructions.
fn RegisterType(comptime func: u6) type {
    return struct {
        const Self = @This();

        dest: Register,
        src1: Register,
        src2: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.shift == 0)
                return Self{.dest = parts.rd, .src1 = parts.rs, .src2 = parts.rt}
            else
                return null;
        }
    };
}

/// Instruction decoder for register-type instructions with rs only.
fn RSType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.rt.val == 0 and parts.rd.val == 0 and parts.shift == 0)
                return Self{.src = parts.rs}
            else
                return null;
        }
    };
}

/// Instruction decoder for register-type instructions with rd only.
fn RDType(comptime func: u6) type {
    return struct {
        const Self = @This();

        dest: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.rs.val == 0 and parts.rt.val == 0 and parts.shift == 0)
                return Self{.dest = parts.rd}
            else
                return null;
        }
    };
}

/// Instruction decoder for register-type instructions with rs and rd only.
fn RSDType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.rt.val == 0 and parts.shift == 0)
                return Self{.src = parts.rs, .dest = parts.rd}
            else
                return null;
        }
    };
}

/// Instruction decoder for register-type instructions with rs and rt only.
fn RSTType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src1: Register,
        src2: Register,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.rd.val == 0 and parts.shift == 0)
                return Self{.src1 = parts.rs, .src2 = parts.rt}
            else
                return null;
        }
    };
}

/// Instruction decoder for shift-type instructions.
fn ShiftType(comptime func: u6) type {
    return struct {
        const Self = @This();

        dest: Register,
        src: Register,
        amount: u5,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 0 and parts.func == func and parts.rs.val == 0)
                return Self{.dest = parts.rd, .src = parts.rs, .amount = parts.shift}
            else
                return null;
        }
    };
}

/// Instruction decoder for signed immediate-type instructions.
fn SignedImmediateType(comptime opcode: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == opcode)
                return Self{ .src = parts.rs, .dest = parts.rt, .imm = parts.simm }
            else
                return null;
        }
    };
}

/// Instruction decoder for unsigned immediate-type instructions.
fn UnsignedImmediateType(comptime opcode: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,
        imm: UnsignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == opcode)
                return Self{ .src = parts.rs, .dest = parts.rt, .imm = parts.uimm }
            else
                return null;
        }
    };
}

/// Instruction decoder for immediate-type branch instructions.
fn BranchType(comptime opcode: u6) type {
    return struct {
        const Self = @This();

        src1: Register,
        src2: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == opcode)
                return Self{ .src1 = parts.rs, .src2 = parts.rt, .imm = parts.simm }
            else
                return null;
        }
    };
}

/// Instruction decoder for immediate-type branch-against-zero instructions.
fn BranchZeroType(comptime opcode: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == opcode and parts.rt.val == 0)
                return Self{ .src = parts.rs, .imm = parts.simm }
            else
                return null;
        }
    };
}

/// Instruction decoder for "REGIMM"-type branch instructions.
fn RegImmBranchType(comptime kind: u5) type {
    return struct {
        const Self = @This();

        src: Register,
        offset: SignedImmediate,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == 1 and parts.rt.val == kind)
                return Self{.src = parts.rs, .offset = parts.simm}
            else
                return null;
        }
    };
}

/// Instruction decoder for jump-type instructions.
fn JumpType(comptime opcode: u6) type {
    return struct {
        const Self = @This();

        target: JumpTarget,

        fn decode(parts: InstructionParts) ?Self {
            if (parts.opcode == opcode)
                return Self{.target = parts.target}
            else
                return null;
        }
    };
}

//////////////////////////////////////////////////////////////////////
// Processor state and execution.
//////////////////////////////////////////////////////////////////////

/// A register is a 5-bit integer.
pub const Register = struct {
    val: u5,

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("r{}", .{self.val});
    }
};

/// A coprocessor register.
pub const CoRegister = struct {
    val: u5,

    pub fn format(self: @This(), comptime _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("c{}", .{self.val});
    }
};

/// Errors returned during execution
pub const ExecutionError = error {
    InvalidCOP, // TODO: we return this also on e.g. reads/writes of unknown COP0 registers - is this right?
    Overflow,
    MemoryProtectionError
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

    /// Resolve a virtual address to a physical address,
    /// also checking access permissions.
    pub fn resolve_address(self: *const CPU, addr: u32) !u32 {
        if (addr >= 0x80000000 and self.cop0.sr.ku()[0])
            return error.MemoryProtectionError;

        if (addr < 0x80000000) // KUSEG
            // Officially this gets translated to addr + 1GB,
            // but let's just identity map it
            return addr
        else if (addr < 0xa0000000) // KSEG0
            return addr - 0x80000000
        else if (addr < 0xc0000000) // KSEG1
            return addr - 0xa0000000
        else // KSEG2
            return addr;
    }

    /// Fetch the next instruction.
    pub fn fetch(self: *const CPU) !u32 {
        return memory.fetch(try self.resolve_address(self.pc));
    }

    /// Read from a given memory address.
    pub fn read(self: *const CPU, comptime T: type, addr: u32) !T {
        return memory.read(T, try self.resolve_address(addr));
    }

    /// Write to a given memory address.
    pub fn write(self: *const CPU, comptime T: type, addr: u32, value: T) !void {
        return memory.write(T, try self.resolve_address(addr), value);
    }

    /// Fetch and execute the next instruction.
    pub fn step(self: *CPU) !void {
        const instr = try decode(try self.fetch());
        // The execute function is in charge of updating pc and next_pc.
        try self.execute(instr);
    }

    /// Execute a single, decoded instruction.
    pub fn execute(self: *CPU, instr: Instruction) !void {
        var new_pc: u32 = self.next_pc +% 4;

        switch(instr) {
            .NOP => {},
            .OR => |info| self.set(info.dest, self.get(info.src1) | self.get(info.src2)),
            .AND => |info| self.set(info.dest, self.get(info.src1) & self.get(info.src2)),
            .SLTU => |info| self.set(info.dest, if (self.get(info.src1) < self.get(info.src2)) 1 else 0),
            .ADDU => |info| self.set(info.dest, self.get(info.src1) +% self.get(info.src2)),
            .ADD => |info| {
                const src1 = @bitCast(i32, self.get(info.src1));
                const src2 = @bitCast(i32, self.get(info.src2));
                var dest: i32 = undefined;
                if (@addWithOverflow(i32, src1, src2, &dest))
                    return error.Overflow
                else
                    self.set(info.dest, @bitCast(u32, dest));
            },
            .SUBU => |info| self.set(info.dest, self.get(info.src1) -% self.get(info.src2)),
            .JR => |info| new_pc = self.get(info.src),
            .SLL => |info| self.set(info.dest, self.get(info.src) << info.amount),
            .SRL => |info| self.set(info.dest, self.get(info.src) >> info.amount),
            .LUI => |info| self.set(info.dest, info.imm.val << 16),
            .ORI => |info| self.set(info.dest, self.get(info.src) | info.imm.val),
            .ADDIU => |info| self.set(info.dest, self.get(info.src) +% info.imm.val),
            .ADDI => |info| {
                const src1 = @bitCast(i32, self.get(info.src));
                const src2 = @bitCast(i32, info.imm.val);
                var dest: i32 = undefined;
                if (@addWithOverflow(i32, src1, src2, &dest))
                    return error.Overflow
                else
                    self.set(info.dest, @bitCast(u32, dest));
            },
            .SW => |info| try self.write(u32, self.get(info.src) +% info.imm.val, self.get(info.dest)),
            .LW => |info| self.set(info.dest, try self.read(u32, self.get(info.src) +% info.imm.val)),
            .BNE => |info| {
                if (self.get(info.src1) != self.get(info.src2))
                    new_pc = self.next_pc +% (info.imm.val << 2);
            },
            .BEQ => |info| {
                if (self.get(info.src1) == self.get(info.src2))
                    new_pc = self.next_pc +% (info.imm.val << 2);
            },
            .SH => |info| try self.write(u16, self.get(info.src) +% info.imm.val, @intCast(u16, self.get(info.dest) & 0xffff)),
            .SB => |info| try self.write(u8, self.get(info.src) +% info.imm.val, @intCast(u8, self.get(info.dest) & 0xff)),
            .ANDI => |info| self.set(info.dest, self.get(info.src) & info.imm.val),
            .LB => |info| self.set(info.dest, @bitCast(u32, @as(i32, try self.read(i8, self.get(info.src) +% info.imm.val)))),
            .J => |info| new_pc = info.target.absTarget(self.next_pc),
            .JAL => |info| {
                new_pc = info.target.absTarget(self.next_pc);
                self.set(.{.val = 31}, self.next_pc +% 4);
            },
            .MTC0 => |info| try self.cop0.set(info.dest, self.get(info.src)),
            .MFC0 => |info| self.set(info.dest, try self.cop0.get(info.src)),
            else => {
                std.log.err("unimplemented instruction {}", .{instr});
                return error.UnknownInstruction;
            }
        }

        self.pc = self.next_pc;
        self.next_pc = new_pc;
    }
};

/// The system coprocessor.
pub const COP0 = struct {
    const SR = struct {
        value: u32,

        const Self = @This();

        // 1 bit: this bit is not allowed to be modified
        const mask:  u32 = 0b00001111101101000000000011000000;
        // Initial value of SR.
        const init_value: u32 = 0b00000000010000000000000000000000;

        pub fn init() Self {
            // BEV=1, everything else 0
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

        pub fn cu(self: Self) [4]bool {
            return .{
                utils.testBit(u32, self.value, 28),
                utils.testBit(u32, self.value, 29),
                utils.testBit(u32, self.value, 30),
                utils.testBit(u32, self.value, 31)
            };
        }

        pub fn bev(self: Self) bool {
            return utils.testBit(u32, self.value, 22);
        }

        pub fn cm(self: Self) bool {
            return utils.testBit(u32, self.value, 19);
        }

        pub fn swc(self: Self) bool {
            return utils.testBit(u32, self.value, 17);
        }

        pub fn isc(self: Self) bool {
            return utils.testBit(u32, self.value, 16);
        }

        pub fn im(self: Self) [8] bool {
            return .{
                utils.testBit(u32, self.value, 8),
                utils.testBit(u32, self.value, 9),
                utils.testBit(u32, self.value, 10),
                utils.testBit(u32, self.value, 11),
                utils.testBit(u32, self.value, 12),
                utils.testBit(u32, self.value, 13),
                utils.testBit(u32, self.value, 14),
                utils.testBit(u32, self.value, 15)
            };
        }

        pub fn ku(self: Self) [3] bool {
            return .{
                utils.testBit(u32, self.value, 5),
                utils.testBit(u32, self.value, 3),
                utils.testBit(u32, self.value, 1)
            };
        }

        pub fn ie(self: Self) [3] bool {
            return .{
                utils.testBit(u32, self.value, 4),
                utils.testBit(u32, self.value, 2),
                utils.testBit(u32, self.value, 0)
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
            13 => {},
            14 => {},
            else => {
                std.log.err("unknown COP0 register write {}", .{reg.val});
                return error.InvalidCOP;
            }
        }
    }
};

//////////////////////////////////////////////////////////////////////
// Miscellaneous.
//////////////////////////////////////////////////////////////////////

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
