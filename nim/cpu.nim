import memory
import trie32
import fusion/matching
import std/[setutils, sequtils, tables, options]
{.experimental: "caseStmtMacros".}

type
  DecodingError* = object of CatchableError
  UnknownInstruction* = object of DecodingError
  Opcode* {.pure.} = enum
    SLL, SRL, SRA, SLLV, SRLV, SRAV, JR, JALR, SYSCALL, BREAK, MFHI, MTHI, MFLO,
    MTLO, MULT, MULTU, DIV, DIVU, ADD, ADDU, SUB, SUBU, AND, OR, XOR, NOR, SLT,
    SLTU, BLTZ, BGEZ, BLTZAL, BGEZAL, J, JAL, BEQ, BNE, BLEZ, BGTZ, ADDI, ADDIU,
    SUBI, SUBIU, ANDI, ORI, XORI, LUI, LB, LH, LWL, LW, LBU, LHU, LWR, SB, SH,
    SWL, SW, SWR, MFC0, MTC0

  Section = tuple[pos: int, width: int]

func pattern(section: Section, value: uint32): Pattern =
  let mask = cast[uint32]((1 shl section.width) - 1) shl section.pos
  initPattern(mask, value shl section.pos)

func get(section: Section, value: uint32): uint32 =
  (value shr section.pos) and ((1u32 shl section.width) - 1)

const
  opcode: Section = (pos: 26, width: 6)
  rs: Section = (pos: 21, width: 5)
  rt: Section = (pos: 16, width: 5)
  rd: Section = (pos: 11, width: 5)
  shamt: Section = (pos: 6, width: 5)
  funct: Section = (pos: 0, width: 6)
  imm: Section = (pos: 0, width: 16)
  target: Section = (pos: 0, width: 26)

func registerPattern(op: uint32): Pattern =
  opcode.pattern(0) and funct.pattern(op) and shamt.pattern(0)

func shiftPattern(op: uint32): Pattern =
  opcode.pattern(0) and funct.pattern(op) and rs.pattern(0)

func regImmBranchPattern(op: uint32): Pattern =
  opcode.pattern(1) and rt.pattern(op)

const
  patterns: Table[Opcode, Pattern] = {
    SLL: shiftPattern(0),
    SRL: shiftPattern(2),
    SRA: shiftPattern(3),
    SLLV: registerPattern(4),
    SRLV: registerPattern(6),
    SRAV: registerPattern(7),
    JR: registerPattern(8),
    JALR: registerPattern(9),
    SYSCALL: opcode.pattern(0) and funct.pattern(12),
    BREAK: opcode.pattern(0) and funct.pattern(13),
    MFHI: registerPattern(16),
    MTHI: registerPattern(17),
    MFLO: registerPattern(18),
    MTLO: registerPattern(19),
    MULT: registerPattern(24),
    MULTU: registerPattern(25),
    DIV: registerPattern(26),
    DIVU: registerPattern(27),
    ADD: registerPattern(32),
    ADDU: registerPattern(33),
    SUB: registerPattern(34),
    SUBU: registerPattern(35),
    AND: registerPattern(36),
    OR: registerPattern(37),
    XOR: registerPattern(38),
    NOR: registerPattern(39),
    SLT: registerPattern(42),
    SLTU: registerPattern(43),
    BLTZ: regImmBranchPattern(0),
    BGEZ: regImmBranchPattern(1),
    BLTZAL: regImmBranchPattern(16),
    BGEZAL: regImmBranchPattern(17),
    J: opcode.pattern(2),
    JAL: opcode.pattern(3),
    BEQ: opcode.pattern(4),
    BNE: opcode.pattern(5),
    BLEZ: opcode.pattern(6) and rt.pattern(0),
    BGTZ: opcode.pattern(7) and rt.pattern(0),
    ADDI: opcode.pattern(8),
    ADDIU: opcode.pattern(9),
    SUBI: opcode.pattern(10),
    SUBIU: opcode.pattern(11),
    ANDI: opcode.pattern(12),
    ORI: opcode.pattern(13),
    XORI: opcode.pattern(14),
    LUI: opcode.pattern(15),
    LB: opcode.pattern(32),
    LH: opcode.pattern(33),
    LWL: opcode.pattern(34),
    LW: opcode.pattern(35),
    LBU: opcode.pattern(36),
    LHU: opcode.pattern(37),
    LWR: opcode.pattern(38),
    SB: opcode.pattern(40),
    SH: opcode.pattern(41),
    SWL: opcode.pattern(42),
    SW: opcode.pattern(43),
    SWR: opcode.pattern(46),
    MFC0: opcode.pattern(16) and rs.pattern(0),
    MTC0: opcode.pattern(16) and rs.pattern(4)
    }.toTable

  trie = makeTrie(patterns)

for op, pat in patterns.pairs:
  let
    instr1 = pat.value
    instr2 = pat.value or (not pat.mask)
  assert trie.find[:Opcode](instr1) == some(op)
  assert trie.find[:Opcode](instr2) == some(op)

type
  Instruction = tuple
    op: Opcode
    val: uint32

let
  decodingError = newException(DecodingError, "decoding error")

proc decode(instr: uint32): Instruction =
  case trie.find[:Opcode](instr)
  of Some(@op): return (op: op, val: instr)
  of None(): raise decodingError
