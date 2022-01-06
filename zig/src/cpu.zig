const std = @import("std");
const memory = @import("memory.zig");
const utils = @import("utils.zig");

//////////////////////////////////////////////////////////////////////
// Instruction decoding.
//////////////////////////////////////////////////////////////////////

const MaskedValue = struct {
    mask: u32,
    value: u32,

    fn make(mask: u32, value: u32) MaskedValue {
        return .{.mask = mask, .value = value & mask};
    }

    fn plus(self: MaskedValue, other: MaskedValue) MaskedValue {
        const both_mask = self.mask & other.mask;
        std.debug.assert((self.value & both_mask) == (other.value & both_mask));

        return .{.mask = self.mask | other.mask, .value = self.value | other.value};
    }
};

pub const TrieError = error {
    AmbiguousEncoding
};

const Masks = std.enums.EnumMap(std.meta.Tag(Instruction), MaskedValue);
var trie_children: [4][256][256]u16 = undefined;
var trie_values: [4][256]u16 = undefined;
var trie_end: [4]u16 = .{0, 0, 0, 0};
var trie_root: u16 = undefined;

pub fn init_trie() !void {
    trie_end = .{0,0,0,0};

    const tag_type = std.meta.Tag(Instruction);
    var tags = Masks.init(.{});
    inline for (@typeInfo(tag_type).Enum.fields) |field| {
        const tag = @intToEnum(tag_type, field.value);
        tags.put(tag, Instruction.mask(tag));
    }
    var masks: [4][65536]@TypeOf(tags.bits.mask) = undefined;
    trie_root = try make_trie(3, tags, &masks);
}

fn the_tag(tags: Masks) ?u16 {
    var tags_copy = tags;
    var iter = tags_copy.iterator();
    return switch (tags.count()) {
        0 => @as(u16, 0xffff),
        1 => @enumToInt(iter.next().?.key),
        else => null
    };
}

fn add_node(phase: isize, tags: Masks, masks: *[4][65536]@TypeOf(Masks.init(.{}).bits.mask), children: [256]u16, value: u16) u16 {
    const pos = trie_end[@intCast(usize, phase)];
    trie_end[@intCast(usize, phase)] += 1;

    trie_children[@intCast(usize, phase)][pos] = children;
    trie_values[@intCast(usize, phase)][pos] = value;
    masks[@intCast(usize, phase)][pos] = tags.bits.mask;

    return pos;
}

fn make_trie(phase: isize, tags: Masks, masks: *[4][65536]@TypeOf(Masks.init(.{}).bits.mask)) TrieError!u16 {
    if (phase >= 0)
        if (std.mem.indexOfScalar(@TypeOf(tags.bits.mask), masks[@intCast(usize, phase)][0..trie_end[@intCast(usize, phase)]], tags.bits.mask)) |index|
            return @intCast(u16, index);

    if (phase == -1) {
        if (the_tag(tags)) |tag|
            return tag
        else
            return error.AmbiguousEncoding;
    } else if (the_tag(tags)) |tag| {
        const subnode = try make_trie(phase-1, Masks.init(.{}), masks);
        return add_node(phase, tags, masks, .{subnode} ** 256, tag);
    } else {
        var tags_copy = tags;
        var children: [256]u16 = .{undefined} ** 256;
        var i: usize = 0;
        while (i < 256) : (i += 1) {
            var subtags = tags;
            var iter = tags_copy.iterator();
            while (iter.next()) |tag| {
                const phase_mask = @as(u32, 0xff) << (@intCast(u5, phase)*8);
                const phase_value = @intCast(u32, i) << (@intCast(u5, phase)*8);
                const masked = tag.value;
                const mask = phase_mask & masked.mask;

                if ((phase_value & mask) != (masked.value & mask))
                    subtags.remove(tag.key);
            }

            children[i] = try make_trie(phase-1, subtags, masks);
        }

        return add_node(phase, tags, masks, children, 0xff);
    }
}

/// Errors returned by the decode function.
pub const DecodingError = error {
    UnknownInstruction,
    InvalidTrie
};

pub inline fn decode(instr: u32) DecodingError!Instruction {
    const parts = InstructionParts.make(instr);
    const byte1 = @truncate(u8, instr >> 24);
    const byte2 = @truncate(u8, instr >> 16);
    const byte3 = @truncate(u8, instr >> 8);
    const byte4 = @truncate(u8, instr);

    std.log.warn("decoding {x}/{x}/{x}/{x}, expect {}", .{byte1, byte2, byte3, byte4, old_decode(instr)});

    var op: u16 = 0xff;
    op &= trie_values[0][trie_root];
    const level1 = trie_children[0][trie_root][byte1];
    op &= trie_values[1][level1];
    const level2 = trie_children[1][level1][byte2];
    op &= trie_values[2][level2];
    const level3 = trie_children[2][level2][byte3];
    op &= trie_values[3][level3];
    const level4 = trie_children[3][level3][byte4];
    op &= level4;

    std.log.warn("went {}->{}->{}->{}", .{level1, level2, level3, level4});
    std.log.warn("added {x}->{x}->{x}->{x}", .{trie_values[0][trie_root], trie_values[1][level1], trie_values[2][level2], trie_values[3][level3]});
    std.log.warn("result is {}", .{op});

    return switch(op) {
        @enumToInt(Instruction.SLL) => Instruction.decode(.SLL, parts),
        @enumToInt(Instruction.SRL) => Instruction.decode(.SRL, parts),
        @enumToInt(Instruction.SRA) => Instruction.decode(.SRA, parts),
        @enumToInt(Instruction.SLLV) => Instruction.decode(.SLLV, parts),
        @enumToInt(Instruction.SRLV) => Instruction.decode(.SRLV, parts),
        @enumToInt(Instruction.SRAV) => Instruction.decode(.SRAV, parts),
        @enumToInt(Instruction.JR) => Instruction.decode(.JR, parts),
        @enumToInt(Instruction.JALR) => Instruction.decode(.JALR, parts),
        @enumToInt(Instruction.SYSCALL) => Instruction.decode(.SYSCALL, parts),
        @enumToInt(Instruction.BREAK) => Instruction.decode(.BREAK, parts),
        @enumToInt(Instruction.MFHI) => Instruction.decode(.MFHI, parts),
        @enumToInt(Instruction.MTHI) => Instruction.decode(.MTHI, parts),
        @enumToInt(Instruction.MFLO) => Instruction.decode(.MFLO, parts),
        @enumToInt(Instruction.MTLO) => Instruction.decode(.MTLO, parts),
        @enumToInt(Instruction.MULT) => Instruction.decode(.MULT, parts),
        @enumToInt(Instruction.MULTU) => Instruction.decode(.MULTU, parts),
        @enumToInt(Instruction.DIV) => Instruction.decode(.DIV, parts),
        @enumToInt(Instruction.DIVU) => Instruction.decode(.DIVU, parts),
        @enumToInt(Instruction.ADD) => Instruction.decode(.ADD, parts),
        @enumToInt(Instruction.ADDU) => Instruction.decode(.ADDU, parts),
        @enumToInt(Instruction.SUB) => Instruction.decode(.SUB, parts),
        @enumToInt(Instruction.SUBU) => Instruction.decode(.SUBU, parts),
        @enumToInt(Instruction.AND) => Instruction.decode(.AND, parts),
        @enumToInt(Instruction.OR) => Instruction.decode(.OR, parts),
        @enumToInt(Instruction.XOR) => Instruction.decode(.XOR, parts),
        @enumToInt(Instruction.NOR) => Instruction.decode(.NOR, parts),
        @enumToInt(Instruction.SLT) => Instruction.decode(.SLT, parts),
        @enumToInt(Instruction.SLTU) => Instruction.decode(.SLTU, parts),
        @enumToInt(Instruction.BLTZ) => Instruction.decode(.BLTZ, parts),
        @enumToInt(Instruction.BGEZ) => Instruction.decode(.BGEZ, parts),
        @enumToInt(Instruction.BLTZAL) => Instruction.decode(.BLTZAL, parts),
        @enumToInt(Instruction.BGEZAL) => Instruction.decode(.BGEZAL, parts),
        @enumToInt(Instruction.J) => Instruction.decode(.J, parts),
        @enumToInt(Instruction.JAL) => Instruction.decode(.JAL, parts),
        @enumToInt(Instruction.BEQ) => Instruction.decode(.BEQ, parts),
        @enumToInt(Instruction.BNE) => Instruction.decode(.BNE, parts),
        @enumToInt(Instruction.BLEZ) => Instruction.decode(.BLEZ, parts),
        @enumToInt(Instruction.BGTZ) => Instruction.decode(.BGTZ, parts),
        @enumToInt(Instruction.ADDI) => Instruction.decode(.ADDI, parts),
        @enumToInt(Instruction.ADDIU) => Instruction.decode(.ADDIU, parts),
        @enumToInt(Instruction.SUBI) => Instruction.decode(.SUBI, parts),
        @enumToInt(Instruction.SUBIU) => Instruction.decode(.SUBIU, parts),
        @enumToInt(Instruction.ANDI) => Instruction.decode(.ANDI, parts),
        @enumToInt(Instruction.ORI) => Instruction.decode(.ORI, parts),
        @enumToInt(Instruction.XORI) => Instruction.decode(.XORI, parts),
        @enumToInt(Instruction.LUI) => Instruction.decode(.LUI, parts),
        @enumToInt(Instruction.LB) => Instruction.decode(.LB, parts),
        @enumToInt(Instruction.LH) => Instruction.decode(.LH, parts),
        @enumToInt(Instruction.LWL) => Instruction.decode(.LWL, parts),
        @enumToInt(Instruction.LW) => Instruction.decode(.LW, parts),
        @enumToInt(Instruction.LBU) => Instruction.decode(.LBU, parts),
        @enumToInt(Instruction.LHU) => Instruction.decode(.LHU, parts),
        @enumToInt(Instruction.LWR) => Instruction.decode(.LWR, parts),
        @enumToInt(Instruction.SB) => Instruction.decode(.SB, parts),
        @enumToInt(Instruction.SH) => Instruction.decode(.SH, parts),
        @enumToInt(Instruction.SWL) => Instruction.decode(.SWL, parts),
        @enumToInt(Instruction.SW) => Instruction.decode(.SW, parts),
        @enumToInt(Instruction.SWR) => Instruction.decode(.SWR, parts),
        @enumToInt(Instruction.MFC0) => Instruction.decode(.MFC0, parts),
        @enumToInt(Instruction.MTC0) => Instruction.decode(.MTC0, parts),
        else => error.InvalidTrie
    };
}


/// Decode an instruction.
pub inline fn old_decode(instr: u32) DecodingError!Instruction {
    const parts = InstructionParts.make(instr);

    return switch (parts.opcode) {
        0 => switch (parts.func) {
            0 => Instruction.decode(.SLL, parts),
            2 => Instruction.decode(.SRL, parts),
            3 => Instruction.decode(.SRA, parts),
            4 => Instruction.decode(.SLLV, parts),
            6 => Instruction.decode(.SRLV, parts),
            7 => Instruction.decode(.SRAV, parts),
            8 => Instruction.decode(.JR, parts),
            9 => Instruction.decode(.JALR, parts),
            12 => Instruction.decode(.SYSCALL, parts),
            13 => Instruction.decode(.BREAK, parts),
            16 => Instruction.decode(.MFHI, parts),
            17 => Instruction.decode(.MTHI, parts),
            18 => Instruction.decode(.MFLO, parts),
            19 => Instruction.decode(.MTLO, parts),
            24 => Instruction.decode(.MULT, parts),
            25 => Instruction.decode(.MULTU, parts),
            26 => Instruction.decode(.DIV, parts),
            27 => Instruction.decode(.DIVU, parts),
            32 => Instruction.decode(.ADD, parts),
            33 => Instruction.decode(.ADDU, parts),
            34 => Instruction.decode(.SUB, parts),
            35 => Instruction.decode(.SUBU, parts),
            36 => Instruction.decode(.AND, parts),
            37 => Instruction.decode(.OR, parts),
            38 => Instruction.decode(.XOR, parts),
            39 => Instruction.decode(.NOR, parts),
            42 => Instruction.decode(.SLT, parts),
            43 => Instruction.decode(.SLTU, parts),
            else => error.UnknownInstruction
        },
        1 => switch (parts.rt.val) {
            0 => Instruction.decode(.BLTZ, parts),
            1 => Instruction.decode(.BGEZ, parts),
            16 => Instruction.decode(.BLTZAL, parts),
            17 => Instruction.decode(.BGEZAL, parts),
            else => error.UnknownInstruction
        },
        2 => Instruction.decode(.J, parts),
        3 => Instruction.decode(.JAL, parts),
        4 => Instruction.decode(.BEQ, parts),
        5 => Instruction.decode(.BNE, parts),
        6 => Instruction.decode(.BLEZ, parts),
        7 => Instruction.decode(.BGTZ, parts),
        8 => Instruction.decode(.ADDI, parts),
        9 => Instruction.decode(.ADDIU, parts),
        10 => Instruction.decode(.SUBI, parts),
        11 => Instruction.decode(.SUBIU, parts),
        12 => Instruction.decode(.ANDI, parts),
        13 => Instruction.decode(.ORI, parts),
        14 => Instruction.decode(.XORI, parts),
        15 => Instruction.decode(.LUI, parts),
        32 => Instruction.decode(.LB, parts),
        33 => Instruction.decode(.LH, parts),
        34 => Instruction.decode(.LWL, parts),
        35 => Instruction.decode(.LW, parts),
        36 => Instruction.decode(.LBU, parts),
        37 => Instruction.decode(.LHU, parts),
        38 => Instruction.decode(.LWR, parts),
        40 => Instruction.decode(.SB, parts),
        41 => Instruction.decode(.SH, parts),
        42 => Instruction.decode(.SWL, parts),
        43 => Instruction.decode(.SW, parts),
        46 => Instruction.decode(.SWR, parts),
        16 => switch (parts.rs.val) {
            0 => Instruction.decode(.MFC0, parts),
            4 => Instruction.decode(.MTC0, parts),
            else => error.UnknownInstruction
        },
        else => error.UnknownInstruction
    };
}

/// A decoded instruction.
pub const Instruction = union(enum) {
    /// Decode a particular instruction given an InstructionParts.
    fn decode(comptime tag: @typeInfo(Instruction).Union.tag_type.?, parts: InstructionParts) !Instruction {
        return @unionInit(Instruction, @tagName(tag), try std.meta.TagPayload(Instruction, tag).decode(parts));
    }

    fn mask(comptime tag: @typeInfo(Instruction).Union.tag_type.?) MaskedValue {
        return std.meta.TagPayload(Instruction, tag).mask();
    }

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
        /// UnsignedImmediateType but rs is unused
        const Self = @This();

        dest: Register,
        imm: UnsignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            return Self{ .dest = parts.rt, .imm = parts.uimm };
        }

        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(15);
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

        fn decode(parts: InstructionParts) !Self {
            if (parts.shift == 0 and parts.func == 0)
                return Self{.dest = parts.rt, .src = .{.val = parts.rd.val}}
            else
                return error.UnknownInstruction;
        }

        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(16).plus(InstructionParts.rsMask(0)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.funcMask(0));
        }
    },
    MTC0: struct {
        const Self = @This();
        dest: CoRegister,
        src: Register,

        fn decode(parts: InstructionParts) !Self {
            if (parts.shift == 0 and parts.func == 0)
                return Self{.src = parts.rt, .dest = .{.val = parts.rd.val}}
            else
                return error.UnknownInstruction;
        }

        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(16).plus(InstructionParts.rsMask(4)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.funcMask(0));
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
            .opcode = @truncate(u6, instr >> 26),
            .rs = Register{.val = @truncate(u5, (instr >> 21))},
            .rt = Register{.val = @truncate(u5, (instr >> 16))},
            .rd = Register{.val = @truncate(u5, (instr >> 11))},
            .shift = @truncate(u5, instr >> 6),
            .func = @truncate(u6, instr),
            .simm = SignedImmediate.make(@truncate(u16, instr)),
            .uimm = UnsignedImmediate.make(@truncate(u16, instr)),
            .target = JumpTarget{.val = @truncate(u26, instr)},
        };
    }

    fn mask(it: anytype, pos: u5) MaskedValue {
        const max = std.math.maxInt(@TypeOf(it));
        const the_mask = @as(u32, max) << pos;
        const val = @as(u32, it) << pos;
        return MaskedValue.make(the_mask, val);
    }

    fn opcodeMask(opcode: u6) MaskedValue { return mask(opcode, 26); }
    fn rsMask(rs: u5) MaskedValue { return mask(rs, 21); }
    fn rtMask(rt: u5) MaskedValue { return mask(rt, 16); }
    fn rdMask(rd: u5) MaskedValue { return mask(rd, 11); }
    fn shiftMask(shift: u5) MaskedValue { return mask(shift, 6); }
    fn funcMask(func: u6) MaskedValue { return mask(func, 0); }
    fn immMask(imm: u16) MaskedValue { return mask(imm, 0); }
    fn targetMask(target: u26) MaskedValue { return mask(target, 0); }
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

    fn vali(self: @This()) i32 {
        return @bitCast(i32, self.val);
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
fn DontCareType(comptime op: u6) type {
    return struct {
        const Self = @This();

        fn decode(_: InstructionParts) !Self {
            return Self{};
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(op));
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

        fn decode(parts: InstructionParts) !Self {
            if (parts.shift == 0)
                return Self{.dest = parts.rd, .src1 = parts.rs, .src2 = parts.rt}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.shiftMask(0));
        }
    };
}

/// Instruction decoder for register-type instructions with rs only.
fn RSType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src: Register,

        fn decode(parts: InstructionParts) !Self {
            if (parts.rt.val == 0 and parts.rd.val == 0 and parts.shift == 0)
                return Self{.src = parts.rs}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.rtMask(0)).plus(InstructionParts.rdMask(0));
        }
    };
}

/// Instruction decoder for register-type instructions with rd only.
fn RDType(comptime func: u6) type {
    return struct {
        const Self = @This();

        dest: Register,

        fn decode(parts: InstructionParts) !Self {
            if (parts.rs.val == 0 and parts.rt.val == 0 and parts.shift == 0)
                return Self{.dest = parts.rd}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.rsMask(0)).plus(InstructionParts.rtMask(0));
        }
    };
}

/// Instruction decoder for register-type instructions with rs and rd only.
fn RSDType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,

        fn decode(parts: InstructionParts) !Self {
            if (parts.rt.val == 0 and parts.shift == 0)
                return Self{.src = parts.rs, .dest = parts.rd}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.rtMask(0));
        }
    };
}

/// Instruction decoder for register-type instructions with rs and rt only.
fn RSTType(comptime func: u6) type {
    return struct {
        const Self = @This();

        src1: Register,
        src2: Register,

        fn decode(parts: InstructionParts) !Self {
            if (parts.rd.val == 0 and parts.shift == 0)
                return Self{.src1 = parts.rs, .src2 = parts.rt}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.shiftMask(0)).plus(InstructionParts.rdMask(0));
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

        fn decode(parts: InstructionParts) !Self {
            if (parts.rs.val == 0)
                return Self{.dest = parts.rd, .src = parts.rs, .amount = parts.shift}
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(0).plus(InstructionParts.funcMask(func)).plus(InstructionParts.rsMask(0));
        }
    };
}

/// Instruction decoder for signed immediate-type instructions.
fn SignedImmediateType(comptime op: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            return Self{ .src = parts.rs, .dest = parts.rt, .imm = parts.simm };
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(op);
        }
    };
}

/// Instruction decoder for unsigned immediate-type instructions.
fn UnsignedImmediateType(comptime op: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        dest: Register,
        imm: UnsignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            return Self{ .src = parts.rs, .dest = parts.rt, .imm = parts.uimm };
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(op);
        }
    };
}

/// Instruction decoder for immediate-type branch instructions.
fn BranchType(comptime op: u6) type {
    return struct {
        const Self = @This();

        src1: Register,
        src2: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            return Self{ .src1 = parts.rs, .src2 = parts.rt, .imm = parts.simm };
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(op);
        }
    };
}

/// Instruction decoder for immediate-type branch-against-zero instructions.
fn BranchZeroType(comptime op: u6) type {
    return struct {
        const Self = @This();

        src: Register,
        imm: SignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            if (parts.rt.val == 0)
                return Self{ .src = parts.rs, .imm = parts.simm }
            else
                return error.UnknownInstruction;
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(op).plus(InstructionParts.rtMask(0));
        }
    };
}

/// Instruction decoder for "REGIMM"-type branch instructions.
fn RegImmBranchType(comptime op: u5) type {
    return struct {
        const Self = @This();

        src: Register,
        offset: SignedImmediate,

        fn decode(parts: InstructionParts) !Self {
            return Self{.src = parts.rs, .offset = parts.simm};
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(1).plus(InstructionParts.rtMask(op));
        }
    };
}

/// Instruction decoder for jump-type instructions.
fn JumpType(comptime op: u6) type {
    return struct {
        const Self = @This();

        target: JumpTarget,

        fn decode(parts: InstructionParts) !Self {
            return Self{.target = parts.target};
        }
        fn mask() MaskedValue {
            return InstructionParts.opcodeMask(op);
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

    /// Get the value of a register - signed version.
    pub fn geti(self: *const CPU, reg: Register) i32 {
        return @bitCast(i32, self.get(reg));
    }

    /// Set the value of a register.
    /// Writes to register 0 are ignored.
    pub fn set(self: *CPU, reg: Register, val: u32) void {
        self.regs[reg.val] = val;
        self.regs[0] = 0;
    }

    /// Set the value of a register - signed version.
    pub fn seti(self: *CPU, reg: Register, val: i32) void {
        self.set(reg, @bitCast(u32, val));
    }

    /// Resolve a virtual address to a physical address,
    /// also checking access permissions.
    pub fn resolve_address(self: *const CPU, addr: u32, code: bool) !u32 {
        if (addr >= 0x80000000 and self.cop0.sr.ku()[0])
            return error.MemoryProtectionError;

        // If "isolate cache" bit is set, high bits are discarded and
        // low bits are used to index into scratchpad (only for data,
        // not code).
        if (!code and self.cop0.sr.isc())
            // TODO: allow scratchpad to be disabled too
            return (addr & 0x3ff) + 0x1f800000;

        return addr; // memory.zig does memory mirroring
    }

    /// Fetch the next instruction.
    pub fn fetch(self: *const CPU) !u32 {
        return memory.fetch(try self.resolve_address(self.pc, true));
    }

    /// Read from a given memory address.
    pub fn read(self: *const CPU, comptime T: type, addr: u32) !T {
        return memory.read(T, try self.resolve_address(addr, false));
    }

    /// Write to a given memory address.
    pub fn write(self: *const CPU, comptime T: type, addr: u32, value: T) !void {
        return memory.write(T, try self.resolve_address(addr, false), value);
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
            .OR => |info| self.set(info.dest, self.get(info.src1) | self.get(info.src2)),
            .AND => |info| self.set(info.dest, self.get(info.src1) & self.get(info.src2)),
            .SLTU => |info| self.set(info.dest, if (self.get(info.src1) < self.get(info.src2)) 1 else 0),
            .SLT => |info| self.seti(info.dest, if (self.geti(info.src1) < self.geti(info.src2)) 1 else 0),
            .ADDU => |info| self.set(info.dest, self.get(info.src1) +% self.get(info.src2)),
            .ADD => |info| {
                var dest: i32 = undefined;
                if (@addWithOverflow(i32, self.geti(info.src1), self.geti(info.src2), &dest))
                    return error.Overflow
                else
                    self.seti(info.dest, dest);
            },
            .SUBU => |info| self.set(info.dest, self.get(info.src1) -% self.get(info.src2)),
            .SUBIU => |info| self.set(info.dest, self.get(info.src) -% info.imm.val),
            .JR => |info| new_pc = self.get(info.src),
            .SLL => |info| self.set(info.dest, self.get(info.src) << info.amount),
            .SRL => |info| self.set(info.dest, self.get(info.src) >> info.amount),
            .SRA => |info| self.seti(info.dest, self.geti(info.src) >> info.amount),
            .LUI => |info| self.set(info.dest, info.imm.val << 16),
            .ORI => |info| self.set(info.dest, self.get(info.src) | info.imm.val),
            .ADDIU => |info| self.set(info.dest, self.get(info.src) +% info.imm.val),
            .ADDI => |info| {
                var dest: i32 = undefined;
                if (@addWithOverflow(i32, self.geti(info.src), info.imm.vali(), &dest))
                    return error.Overflow
                else
                    self.seti(info.dest, dest);
            },
            .SUBI => |info| {
                var dest: i32 = undefined;
                if (@subWithOverflow(i32, self.geti(info.src), info.imm.vali(), &dest))
                    return error.Overflow
                else
                    self.seti(info.dest, dest);
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
            .BLEZ => |info| {
                if (@bitCast(i32, self.get(info.src)) <= 0)
                    new_pc = self.next_pc +% (info.imm.val << 2);
            },
            .BGTZ => |info| {
                if (@bitCast(i32, self.get(info.src)) > 0)
                    new_pc = self.next_pc +% (info.imm.val << 2);
            },
            .SH => |info| try self.write(u16, self.get(info.src) +% info.imm.val, @truncate(u16, self.get(info.dest))),
            .SB => |info| try self.write(u8, self.get(info.src) +% info.imm.val, @truncate(u8, self.get(info.dest))),
            .ANDI => |info| self.set(info.dest, self.get(info.src) & info.imm.val),
            .LB => |info| self.seti(info.dest, @as(i32, try self.read(i8, self.get(info.src) +% info.imm.val))),
            .LBU => |info| self.set(info.dest, @as(u32, try self.read(u8, self.get(info.src) +% info.imm.val))),
            .LH => |info| self.seti(info.dest, @as(i32, try self.read(i16, self.get(info.src) +% info.imm.val))),
            .LHU => |info| self.set(info.dest, @as(u32, try self.read(u16, self.get(info.src) +% info.imm.val))),
            .J => |info| new_pc = info.target.absTarget(self.next_pc),
            .JALR => |info| {
                new_pc = self.get(info.src);
                self.set(.{.val = 31}, self.next_pc +% 4);
            },
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

test {
    std.testing.refAllDecls(@This());
}

test "init trie" {
    init_trie() catch unreachable;
    std.log.warn("{} {} {} {} {}", .{trie_end[3], trie_end[2], trie_end[1], trie_end[0], trie_root});
}

test "every instruction has the correct mask" {
    @setEvalBranchQuota(10000);
    init_trie() catch unreachable;
    const tag_type = std.meta.Tag(Instruction);
    inline for (@typeInfo(tag_type).Enum.fields) |field| {
        const tag = @intToEnum(tag_type, field.value);
        const mask = Instruction.mask(tag);
        const inverted_value = mask.value | ~mask.mask;
        try std.testing.expectEqual(tag, try decode(mask.value));
        try std.testing.expectEqual(tag, try decode(inverted_value));
    }
}
