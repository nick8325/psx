## An interpreter for the R3000A CPU.

import memory, trie32, utils, common
import fusion/matching
import std/[setutils, tables, options, bitops, strformat]
{.experimental: "caseStmtMacros".}

# Processor state.

type
  Register* = distinct range[0..31] ## A register.
  CoRegister = distinct range[0..31] ## A coprocessor register.

func `$`*(x: Register): string =
  "r" & $int(x)

func `<=`(r1, r2: Register): bool {. borrow .} # needed for iterator to work

type
  COP0 = object
    ## COP0 status. All fields are COP0 registers.
    sr: uint32
    cause: uint32
    epc: uint32
    badvaddr: uint32
    bpc: uint32
    bda: uint32
    dcic: uint32
    bdam: uint32
    bpcm: uint32

const initCOP0: COP0 =
  # Initial value of COP0.
  COP0(sr: 1 shl 22) # BEV=1

proc `[]`(cop0: COP0, reg: CoRegister): uint32 =
  ## Access registers by number.
  case reg
  of CoRegister(3): cop0.bpc
  of CoRegister(5): cop0.bda
  of CoRegister(7): cop0.dcic
  of CoRegister(8): cop0.badvaddr
  of CoRegister(9): cop0.bdam
  of CoRegister(11): cop0.bpcm
  of CoRegister(12): cop0.sr
  of CoRegister(13): cop0.cause
  of CoRegister(14): cop0.epc
  of CoRegister(15): 2 # PRId - value from Nocash PSX
  else: raise invalidCOPError

proc `[]=`*(cop0: var COP0, reg: CoRegister, val: uint32) =
  ## Access registers by number.
  case reg
  of CoRegister(3): cop0.bpc = val
  of CoRegister(5): cop0.bda = val
  of CoRegister(6): discard # JUMPDEST
  of CoRegister(7): cop0.dcic = val
  of CoRegister(9): cop0.bdam = val
  of CoRegister(11): cop0.bpcm = val
  of CoRegister(12):
    # Which bits are allowed to be modified
    const mask = 0b00001111101101000000000011000000u32
    if (val and mask) != (cop0.sr and mask):
      raise invalidCOPError
    cop0.sr = val
  of CoRegister(13): discard
  of CoRegister(14): discard
  else: raise invalidCopError

func cu(cop0: COP0): array[4, bool] =
  ## Get value of COP0.CU.
  for i in 0..<4:
    result[i] = testBit(cop0.sr, 28+i)

func bev(cop0: COP0): bool =
  ## Get value of COP0.BEV.
  testBit(cop0.sr, 22)

func cm(cop0: COP0): bool =
  ## Get value of COP0.CM.
  testBit(cop0.sr, 19)

func swc(cop0: COP0): bool =
  ## Get value of COP0.SR.
  testBit(cop0.sr, 17)

func isc(cop0: COP0): bool =
  ## Get value of COP0.ISC.
  testBit(cop0.sr, 16)

func im(cop0: COP0): array[8, bool] =
  ## Get value of COP0.IM.
  for i in 0..<8:
    result[i] = testBit(cop0.sr, 8+i)

func ku(cop0: COP0): array[3, bool] =
  ## Get value of COP0.KU.
  for i in 0..<3:
    result[i] = testBit(cop0.sr, 5-2*i)

func ie(cop0: COP0): array[3, bool] =
  ## Get value of COP0.IE.
  for i in 0..<3:
    result[i] = testBit(cop0.sr, 4-2*i)

type
  CPU = object
    ## CPU state.
    pc: uint32 ## Current PC.
    nextPC: uint32 ## Next PC. Used to implement branch delay slot.
    registers: array[Register, uint32] ## Registers.
    cop0: COP0 ## COP0 registers.

let initCPU: CPU = block:
  # The initial state of the CPU after reset.
  const pc = 0xbfc00000u32
  CPU(pc: pc, nextPC: pc+4, cop0: initCOP0)

func `[]`(cpu: CPU, reg: Register): uint32 =
  ## Access registers by number.
  cpu.registers[reg]

func `[]=`(cpu: var CPU, reg: Register, val: uint32) =
  ## Access registers by number. Takes care of ignoring writes to R0.
  cpu.registers[reg] = val
  cpu.registers[Register(0)] = 0

proc resolveAddress(cpu: CPU, address: uint32, code: bool): uint32 =
  ## Resolve a virtual address to a physical address,
  ## also checking access permissions.
  if address >= 0x80000000u32 and cpu.cop0.ku[0]:
    raise memoryProtectionError

  # If "isolate cache" bit is set, high bits are discarded and
  # low bits are used to index into scratchpad (only for data,
  # not code).
  if not code and cpu.cop0.isc:
    # TODO: allow scratchpad to be disabled too
    return (address and 0x3ff) + 0x1f800000

  return address # memory.nim does memory mirroring

proc fetch*(cpu: CPU): uint32 =
  ## Fetch the next instruction.
  fetch(cpu.resolveAddress(cpu.pc, true))

proc read*[T](cpu: CPU, address: uint32): T =
  ## Read from a given virtual address.
  read[T](cpu.resolveAddress(address, false))

proc write*[T](cpu: CPU, address: uint32, val: T) =
  ## Write to a given virtual address.
  write[T](cpu.resolveAddress(address, false), val)

func `$`*(cpu: CPU): string =
  result = fmt "PC={cpu.pc:x} "
  for i, x in cpu.registers:
    result &= fmt "R{i}={x:x} "
  result &= fmt "COP0.SR={cpu.cop0.sr:x}"

func cpuDiff(cpu1: CPU, cpu2: CPU): string =
  if cpu1.pc != cpu2.pc:
    result &= fmt "PC={cpu1.pc:x}->{cpu2.pc:x} "
  for i, x in cpu1.registers:
    if x != cpu2.registers[i]:
      result &= fmt "R{i}={x:x}->{cpu2.registers[i]:x} "
  if cpu1.cop0.sr != cpu2.cop0.sr:
    result &= fmt "COP0.SR={cpu1.cop0.sr:x}->{cpu2.cop0.sr:x}"

# Instruction decoding and execution.

type
  Opcode* {.pure.} = enum
    ## An instruction opcode.
    SLL, SRL, SRA, SLLV, SRLV, SRAV, JR, JALR, SYSCALL, BREAK, MFHI, MTHI, MFLO,
    MTLO, MULT, MULTU, DIV, DIVU, ADD, ADDU, SUB, SUBU, AND, OR, XOR, NOR, SLT,
    SLTU, BLTZ, BGEZ, BLTZAL, BGEZAL, J, JAL, BEQ, BNE, BLEZ, BGTZ, ADDI, ADDIU,
    SUBI, SUBIU, ANDI, ORI, XORI, LUI, LB, LH, LWL, LW, LBU, LHU, LWR, SB, SH,
    SWL, SW, SWR, MFC0, MTC0

  SignedImmediate = distinct int16    ## A sign-extended immediate operand.
  UnsignedImmediate = distinct uint16 ## A zero-extended immediate operand.
  Target = distinct range[0 .. (1 shl 26)-1] ## A 26-bit jump target.

func absTarget(target: Target, pc: uint32): uint32 =
  (pc and 0xf000_0000u32) or (uint32(target) shl 2)

converter toInt32(x: SignedImmediate): int32 =
  int32(int16(x))

converter toUint32(x: SignedImmediate): uint32 =
  x.toInt32.unsigned

converter toUint32(x: UnsignedImmediate): uint32 =
  uint32(x)

const
  opcode: BitSlice[int, word] = (pos: 26, width: 6)
  rs: BitSlice[Register, word] = (pos: 21, width: 5)
  rt: BitSlice[Register, word] = (pos: 16, width: 5)
  rd: BitSlice[Register, word] = (pos: 11, width: 5)
  shamt: BitSlice[int, word] = (pos: 6, width: 5)
  funct: BitSlice[int, word] = (pos: 0, width: 6)
  unsignedImm: BitSlice[SignedImmediate, word] = (pos: 0, width: 16)
  signedImm: BitSlice[UnsignedImmediate, word] = (pos: 0, width: 16)
  target: BitSlice[Target, word] = (pos: 0, width: 26)

func registerPattern(op: word): Pattern[word] =
  opcode.equals(0) and funct.equals(op) and shamt.equals(0)

func shiftPattern(op: word): Pattern[word] =
  opcode.equals(0) and funct.equals(op) and rs.equals(0)

func regImmBranchPattern(op: word): Pattern[word] =
  opcode.equals(1) and rt.equals(op)

let
  patterns: Table[Opcode, Pattern[word]] = {
    SLL: shiftPattern(0),
    SRL: shiftPattern(2),
    SRA: shiftPattern(3),
    SLLV: registerPattern(4),
    SRLV: registerPattern(6),
    SRAV: registerPattern(7),
    JR: registerPattern(8),
    JALR: registerPattern(9),
    SYSCALL: opcode.equals(0) and funct.equals(12),
    BREAK: opcode.equals(0) and funct.equals(13),
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
    J: opcode.equals(2),
    JAL: opcode.equals(3),
    BEQ: opcode.equals(4),
    BNE: opcode.equals(5),
    BLEZ: opcode.equals(6) and rt.equals(0),
    BGTZ: opcode.equals(7) and rt.equals(0),
    ADDI: opcode.equals(8),
    ADDIU: opcode.equals(9),
    SUBI: opcode.equals(10),
    SUBIU: opcode.equals(11),
    ANDI: opcode.equals(12),
    ORI: opcode.equals(13),
    XORI: opcode.equals(14),
    LUI: opcode.equals(15),
    LB: opcode.equals(32),
    LH: opcode.equals(33),
    LWL: opcode.equals(34),
    LW: opcode.equals(35),
    LBU: opcode.equals(36),
    LHU: opcode.equals(37),
    LWR: opcode.equals(38),
    SB: opcode.equals(40),
    SH: opcode.equals(41),
    SWL: opcode.equals(42),
    SW: opcode.equals(43),
    SWR: opcode.equals(46),
    MFC0: opcode.equals(16) and rs.equals(0),
    MTC0: opcode.equals(16) and rs.equals(4)
    }.toTable

  trie = makeTrie(patterns)

for op, pat in patterns.pairs:
  let
    instr1 = pat.value
    instr2 = pat.value or (not pat.mask)
  assert trie.find[:Opcode](instr1) == some(op)
  assert trie.find[:Opcode](instr2) == some(op)

proc decode*(instr: uint32): Opcode =
  case trie.find[:Opcode](instr)
  of Some(@op): return op
  of None(): raise unknownInstructionError

proc execute(cpu: var CPU, op: Opcode, instr: uint32) =
  var newPC = cpu.nextPC + 4

  case op
  of OR: cpu[instr[rd]] = cpu[instr[rs]] or cpu[instr[rt]]
  of AND: cpu[instr[rd]] = cpu[instr[rs]] and cpu[instr[rt]]
  of SLTU: cpu[instr[rd]] = if cpu[instr[rs]] < cpu[instr[rt]]: 1 else: 0
  of SLT: cpu[instr[rd]] = word(cpu[instr[rs]].signed < cpu[instr[rt]].signed)
  of ADDU: cpu[instr[rd]] = cpu[instr[rs]] + cpu[instr[rt]]
  of ADD:
    let
      src1 = cpu[instr[rs]].signed
      src2 = cpu[instr[rt]].signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise overflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise overflowError
    cpu[instr[rd]] = src1.unsigned + src2.unsigned
  of SUBU: cpu[instr[rd]] = cpu[instr[rs]] - cpu[instr[rt]]
  of SUBIU: cpu[instr[rt]] = cpu[instr[rs]] - instr[signedImm]
  of JR: newPC = cpu[instr[rs]]
  of SLL: cpu[instr[rd]] = cpu[instr[rs]] shl instr[shamt]
  of SRL: cpu[instr[rd]] = cpu[instr[rs]] shr instr[shamt]
  of SRA: cpu[instr[rd]] = (cpu[instr[rs]].signed shr instr[shamt]).unsigned
  of LUI: cpu[instr[rt]] = instr[unsignedImm].unsigned shl 16
  of ORI: cpu[instr[rt]] = cpu[instr[rs]] or instr[unsignedImm]
  of ADDIU: cpu[instr[rt]] = cpu[instr[rs]] + instr[unsignedImm]
  of ADDI:
    let
      src1 = cpu[instr[rs]].signed
      src2 = instr[signedImm].signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise overflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise overflowError
    cpu[instr[rt]] = src1.unsigned + src2.unsigned
  of SUBI:
    let
      src1 = cpu[instr[rs]].signed
      src2 = instr[signedImm].signed
    if src1 > 0 and src2 < low(int32) + src1:
      raise overflowError
    if src1 < 0 and src2 > high(int32) + src1:
      raise overflowError
    cpu[instr[rt]] = src1.unsigned - src2.unsigned
  of SW: write[uint32](cpu[instr[rs]] + instr[signedImm].toUint32, cpu[instr[rt]])
  of LW: cpu[instr[rt]] = read[uint32](cpu[instr[rs]] + instr[signedImm].toUint32)
  of BNE:
    if cpu[instr[rs]] != cpu[instr[rt]]:
      newPC = cpu.nextPC + instr[signedImm].toUint32 shl 2
  of BEQ:
    if cpu[instr[rs]] == cpu[instr[rt]]:
      newPC = cpu.nextPC + instr[signedImm].toUint32 shl 2
  of BLEZ:
    if cpu[instr[rs]].signed <= 0:
      newPC = cpu.nextPC + instr[signedImm].toUint32 shl 2
  of BGTZ:
    if cpu[instr[rs]].signed > 0:
      newPC = cpu.nextPC + instr[signedImm].toUint32 shl 2
  of SH:
    write[uint16](cpu[instr[rs]] + instr[signedImm].toUint32, uint16(cpu[instr[rt]] and 0xffff))
  of SB:
    write[uint8](cpu[instr[rs]] + instr[signedImm].toUint32, uint8(cpu[instr[rt]] and 0xff))
  of ANDI:
    cpu[instr[rt]] = cpu[instr[rs]] and instr[unsignedImm]
  of LB:
    cpu[instr[rt]] = int32(read[int8](cpu[instr[rs]] + instr[signedImm].toUint32)).unsigned
  of LBU:
    cpu[instr[rt]] = uint32(read[uint8](cpu[instr[rs]] + instr[signedImm].toUint32))
  of LH:
    cpu[instr[rt]] = int32(read[int16](cpu[instr[rs]] + instr[signedImm].toUint32)).unsigned
  of LHU:
    cpu[instr[rt]] = uint32(read[uint16](cpu[instr[rs]] + instr[signedImm].toUint32))
  of J:
    newPC = instr[target].absTarget(cpu.nextPC)
  of JALR:
    newPC = cpu[instr[rs]]
    cpu[Register(31)] = cpu.nextPC + 4
  of JAL:
    newPC = instr[target].absTarget(cpu.nextPC)
    cpu[Register(31)] = cpu.nextPC + 4
  of MTC0:
    cpu.cop0[CoRegister(instr[rd])] = cpu[instr[rt]]
  of MFC0:
    cpu[instr[rt]] = cpu.cop0[CoRegister(instr[rd])]
  else:
    raise unknownInstructionError

  cpu.pc = cpu.nextPC
  cpu.nextPC = newPC

proc step(cpu: var CPU) =
  let instr = cpu.fetch()
  # The execute function is in charge of updating pc and nextPC.
  cpu.execute(decode(instr), instr)

proc test() =
  var oldCPU: CPU
  var cpu = initCPU
  while true:
    echo cpuDiff(oldCPU, cpu)
    echo cpu.fetch.decode
    oldCPU = cpu
    step(cpu)

test()
