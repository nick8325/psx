## An interpreter for the R3000A CPU.

import utils, common
import memory
import fusion/matching
import std/[tables, bitops, strformat]

# Processor state.

type
  Register* = distinct range[0..31] ## A register.
  CoRegister = distinct range[0..31] ## A coprocessor register.

const
  r0 = Register(0)

func `==`*(x, y: Register): bool {.borrow.}

func `$`*(x: Register): string =
  "r" & $int(x)

func `$`*(x: CoRegister): string =
  "c" & $int(x)

func `<=`(r1, r2: Register): bool {. borrow .} # needed for iterator to work

type
  COP0 = object
    ## COP0 status. All fields are COP0 registers.
    sr: uint32
    cause: uint32
    epc: uint32
    badvaddr: uint32
    # We don't really use these ones, but they can be get and set.
    # Details from Nocash PSX.
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
  else: raise new InvalidCOPError # TODO Is this the right exception?

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
      raise new InvalidCOPError # TODO Is this the right exception?
    cop0.sr = val
  of CoRegister(13): discard
  of CoRegister(14): discard
  else: raise new InvalidCOPError # TODO Is this the right exception?

func cu(cop0: COP0): array[4, bool] {.used.} =
  ## Get value of COP0.CU.
  for i in 0..<4:
    result[i] = testBit(cop0.sr, 28+i)

func bev(cop0: COP0): bool {.used.} =
  ## Get value of COP0.BEV.
  testBit(cop0.sr, 22)

func cm(cop0: COP0): bool {.used.} =
  ## Get value of COP0.CM.
  testBit(cop0.sr, 19)

func swc(cop0: COP0): bool {.used.} =
  ## Get value of COP0.SR.
  testBit(cop0.sr, 17)

func isc(cop0: COP0): bool =
  ## Get value of COP0.ISC.
  testBit(cop0.sr, 16)

func im(cop0: COP0): array[8, bool] {.used.} =
  ## Get value of COP0.IM.
  for i in 0..<8:
    result[i] = testBit(cop0.sr, 8+i)

func ku(cop0: COP0): array[3, bool] =
  ## Get value of COP0.KU.
  for i in 0..<3:
    result[i] = testBit(cop0.sr, 5-2*i)

func ie(cop0: COP0): array[3, bool] {.used.} =
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
  cpu.registers[r0] = 0

proc resolveAddress(cpu: CPU, address: uint32, code: bool): uint32 =
  ## Resolve a virtual address to a physical address,
  ## also checking access permissions.
  if address >= 0x80000000u32 and cpu.cop0.ku[0]:
    raise new MemoryProtectionError

  # If "isolate cache" bit is set, high bits are discarded and
  # low bits are used to index into scratchpad (only for data,
  # not code).
  if not code and cpu.cop0.isc:
    # TODO: allow scratchpad to be disabled too
    return (address and 0x3ff) + 0x1f800000

  return address # memory.nim does memory mirroring

proc fetch*(cpu: CPU): uint32 =
  ## Fetch the next instruction.
  addressSpace.fetch(cpu.resolveAddress(cpu.pc, true))

proc read*[T](cpu: CPU, address: uint32): T =
  ## Read from a given virtual address.
  addressSpace.read[:T](cpu.resolveAddress(address, false))

proc write*[T](cpu: CPU, address: uint32, val: T) =
  ## Write to a given virtual address.
  addressSpace.write[:T](cpu.resolveAddress(address, false), val)

func `$`*(cpu: CPU): string =
  result = fmt "PC={cpu.pc:x} "
  for i, x in cpu.registers:
    result &= fmt "{i}={x:x} "
  result &= fmt "COP0.SR={cpu.cop0.sr:x}"

func cpuDiff(cpu1: CPU, cpu2: CPU): string {.used.} =
  ## Show the difference between two CPU states.
  if cpu1.pc != cpu2.pc:
    result &= fmt "PC={cpu1.pc:x}->{cpu2.pc:x} "
  for i, x in cpu1.registers:
    if x != cpu2.registers[i]:
      result &= fmt "{i}={x:x}->{cpu2.registers[i]:x} "
  if cpu1.cop0.sr != cpu2.cop0.sr:
    result &= fmt "COP0.SR={cpu1.cop0.sr:x}->{cpu2.cop0.sr:x} "

# Instruction decoding and execution.

type
  Opcode* {.pure.} = enum
    ## An instruction opcode.
    # Arithmetic
    ADD, ADDI, ADDU, ADDIU, SUB, SUBU, SUBI, SUBIU,
    DIV, DIVU, MULT, MULTU, MFLO, MTLO, MFHI, MTHI,
    SLT, SLTU, LUI,
    # Logic
    AND, ANDI, OR, ORI, XOR, XORI, NOR,
    # Shifts
    SLL, SLLV, SRA, SRAV, SRL, SRLV,
    # Memory
    LW, LB, LBU, LH, LHU, LWL, LWR,
    SW, SB, SH, SWL, SWR,
    # Branches
    BEQ, BNE, BGEZ, BGEZAL, BGTZ, BLEZ, BLTZ, BLTZAL,
    J, JR, JAL, JALR,
    # System
    SYSCALL, BREAK, MFC0, MTC0, RFE
  Immediate = distinct uint16   ## An immediate operand.
  Target = distinct range[0 .. (1 shl 26)-1] ## A 26-bit jump target.

func `$`*(x: Immediate): string =
  fmt"$0x{uint16(x):x}"

func `$`*(x: Target): string =
  fmt"$0x{word(x):x}"

func absTarget(target: Target, pc: uint32): uint32 =
  ## Resolve a target address.
  (pc and 0xf000_0000u32) or (uint32(target) shl 2)

func zeroExt(x: Immediate): uint32 =
  ## Zero extend an immediate to a uint32.
  uint32(uint16(x))

func signExt(x: Immediate): uint32 =
  ## Sign extend an immediate to a uint32.
  cast[uint32](int32(cast[int16](uint16(x))))

type
  OpcodeType {.pure.} = enum
    ## Possible instruction codings used by MIPS
    Reg,   ## R-type - RD, RS, RT used
    RegST, ## R-type - RS and RT used as source registers (MULT and DIV family)
    RegD,  ## R-type - RD used (MFHI/MFLO)
    RegS,  ## R-type - RS used (MTHI/MTLO)
    RegDS, ## R-type - RD and RS used (JALR)
    Shift, ## R-type with shift - RT and RD used
    Imm,   ## Immediate - RS is source, RT is target
    ImmT,  ## Immediate - RT used (LUI)
    ImmS,  ## Immediate - RS used (BGTZ and family)
    Mem,   ## Load/store - RS is base address, RT is value
    Jump,  ## Absolute jump
    MFC,   ## MFCn/CFCn - RD is (COP) source, RT is target
    MTC,   ## MTCn/CFCn - RT is source, RD is (COP) target
    Whole, ## Only opcode used, rest is user-settable (SYSCALL and BREAK)
    None   ## No arguments (RFE)

const opcodeType: array[Opcode, OpcodeType] =
  # Type of each opcode
  [ADD: Reg, ADDI: Imm, ADDU: Reg, ADDIU: Imm, SUB: Reg, SUBU: Imm, SUBI: Reg,
   SUBIU: Imm, DIV: RegST, DIVU: RegST, MULT: RegST, MULTU: RegST, MFLO: RegD,
   MTLO: RegS, MFHI: RegD, MTHI: RegS, SLT: Reg, SLTU: Reg, LUI: ImmT, AND: Reg,
   ANDI: Imm, OR: Reg, ORI: Imm, XOR: Reg, XORI: Imm, NOR: Reg, SLL: Shift,
   SLLV: Reg, SRA: Shift, SRAV: Reg, SRL: Shift, SRLV: Reg, LW: Mem, LB: Mem,
   LBU: Mem, LH: Mem, LHU: Mem, LWL: Mem, LWR: Mem, SW: Mem, SB: Mem, SH: Mem,
   SWL: Mem, SWR: Mem, BEQ: Imm, BNE: Imm, BGEZ: ImmS, BGEZAL: ImmS, BGTZ: ImmS,
   BLEZ: ImmS, BLTZ: ImmS, BLTZAL: ImmS, J: Jump, JR: RegS, JAL: Jump,
   JALR: RegDS, SYSCALL: Whole, BREAK: Whole, MFC0: MFC, MTC0: MTC, RFE: None]

const
  # The different parts of a machine instruction.
  opcode: BitSlice[int, word] = (pos: 26, width: 6)
  rs: BitSlice[Register, word] = (pos: 21, width: 5)
  rt: BitSlice[Register, word] = (pos: 16, width: 5)
  rd: BitSlice[Register, word] = (pos: 11, width: 5)
  shamt: BitSlice[int, word] = (pos: 6, width: 5)
  funct: BitSlice[int, word] = (pos: 0, width: 6)
  imm: BitSlice[Immediate, word] = (pos: 0, width: 16)
  target: BitSlice[Target, word] = (pos: 0, width: 26)
  copimm: BitSlice[word, word] = (pos: 0, width: 25)
  whole: BitSlice[word, word] = (pos: 0, width: 26)

proc decode*(instr: uint32): Opcode {.inline.} =
  ## Decode an instruction to find its opcode.

  proc guard(x: bool) {.inline.} =
    if not x: raise new UnknownInstructionError

  proc ret(instr: word, op: static Opcode): Opcode {.inline.} =
    ## Check for fields which must be zero, depending on instruction type
    case opcodeType[op]
    of Reg: guard instr[shamt] == 0
    of RegST: guard instr[shamt] == 0 and instr[rd] == r0
    of RegD: guard instr[shamt] == 0 and instr[rs] == r0 and instr[rt] == r0
    of RegS: guard instr[shamt] == 0 and instr[rd] == r0 and instr[rt] == r0
    of RegDS: guard instr[shamt] == 0 and instr[rt] == r0
    of Shift: guard instr[rs] == r0
    of MFC: guard instr[shamt] == 0 and instr[funct] == 0
    of MTC: guard instr[shamt] == 0 and instr[funct] == 0
    else: discard

    return op

  case instr[opcode]
  of 0:
    case instr[funct]
    of 0: instr.ret SLL
    of 2: instr.ret SRL
    of 3: instr.ret SRA
    of 4: instr.ret SLLV
    of 6: instr.ret SRLV
    of 7: instr.ret SRAV
    of 8: instr.ret JR
    of 9: instr.ret JALR
    of 12: instr.ret SYSCALL
    of 13: instr.ret BREAK
    of 16: instr.ret MFHI
    of 17: instr.ret MTHI
    of 18: instr.ret MFLO
    of 19: instr.ret MTLO
    of 24: instr.ret MULT
    of 25: instr.ret MULTU
    of 26: instr.ret DIV
    of 27: instr.ret DIVU
    of 32: instr.ret ADD
    of 33: instr.ret ADDU
    of 34: instr.ret SUB
    of 35: instr.ret SUBU
    of 36: instr.ret AND
    of 37: instr.ret OR
    of 38: instr.ret XOR
    of 39: instr.ret NOR
    of 42: instr.ret SLT
    of 43: instr.ret SLTU
    else: raise new UnknownInstructionError
  of 1:
    case int(instr[rt])
    of 0: instr.ret BLTZ
    of 1: instr.ret BGEZ
    of 16: instr.ret BLTZAL
    of 17: instr.ret BGEZAL
    else: raise new UnknownInstructionError
  of 2: instr.ret J
  of 3: instr.ret JAL
  of 4: instr.ret BEQ
  of 5: instr.ret BNE
  of 6:
    guard instr[rt] == r0
    instr.ret BLEZ
  of 7:
    guard instr[rt] == r0
    instr.ret BGTZ
  of 8: instr.ret ADDI
  of 9: instr.ret ADDIU
  of 10: instr.ret SUBI
  of 11: instr.ret SUBIU
  of 12: instr.ret ANDI
  of 13: instr.ret ORI
  of 14: instr.ret XORI
  of 15: instr.ret LUI
  of 32: instr.ret LB
  of 33: instr.ret LH
  of 34: instr.ret LWL
  of 35: instr.ret LW
  of 36: instr.ret LBU
  of 37: instr.ret LHU
  of 38: instr.ret LWR
  of 40: instr.ret SB
  of 41: instr.ret SH
  of 42: instr.ret SWL
  of 43: instr.ret SW
  of 46: instr.ret SWR
  of 16:
    case int(instr[rs])
    of 0: instr.ret MFC0
    of 4: instr.ret MTC0
    of 16:
      guard instr[copimm] == 16
      instr.ret RFE
    else: raise new UnknownInstructionError
  else: raise new UnknownInstructionError

proc format*(instr: uint32): string =
  ## Disassemble an instruction.

  let
    op = decode(instr)
    rd = instr[rd]
    rs = instr[rs]
    rt = instr[rt]
    imm = instr[imm]
    target = instr[target]
    whole = instr[whole]
    shamt = instr[shamt]

  proc args(args: varargs[string, `$`]): string =
    ## Convert an argument list to a string.
    result = $op
    for i, arg in args:
      if i != 0: result &= ","
      result &= " " & arg

  return case opcodeType[op]
  of Reg: args(rd, rs, rt)
  of RegST: args(rs, rt)
  of RegD: args(rd)
  of RegS: args(rs)
  of RegDS: args(rd, rs)
  of Shift: args(rd, rt, shamt)
  of Imm: args(rt, rs, imm)
  of ImmT: args(rt, imm)
  of ImmS: args(rs, imm)
  of Mem: args(rt, fmt"{imm}({rs})")
  of Jump: args(target)
  of MFC: args(rt, CoRegister(rd))
  of MTC: args(CoRegister(rd), rt)
  of Whole: args(fmt"$0x{whole}")
  of None: $op

proc execute(cpu: var CPU, instr: uint32) {.inline.} =
  var newPC = cpu.nextPC + 4
  let
    op = decode(instr)
    rd = instr[rd]
    rs = instr[rs]
    rt = instr[rt]
    shamt = instr[shamt]
    imm = instr[imm]
    target = instr[target]

  case op
  of ADD:
    let
      src1 = cpu[rs].signed
      src2 = cpu[rt].signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise new ArithmeticOverflowError
    cpu[rd] = src1.unsigned + src2.unsigned
  of ADDI:
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise new ArithmeticOverflowError
    cpu[rt] = src1.unsigned + src2.unsigned
  of ADDU: cpu[rd] = cpu[rs] + cpu[rt]
  of ADDIU: cpu[rt] = cpu[rs] + imm.signExt
  of SUB: raise new UnknownInstructionError
  of SUBI:
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 < low(int32) + src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 > high(int32) + src1:
      raise new ArithmeticOverflowError
    cpu[rt] = src1.unsigned - src2.unsigned
  of SUBU: cpu[rd] = cpu[rs] - cpu[rt]
  of SUBIU: cpu[rt] = cpu[rs] - imm.signExt
  of DIV: raise new UnknownInstructionError
  of DIVU: raise new UnknownInstructionError
  of MULT: raise new UnknownInstructionError
  of MULTU: raise new UnknownInstructionError
  of MFLO: raise new UnknownInstructionError
  of MTLO: raise new UnknownInstructionError
  of MFHI: raise new UnknownInstructionError
  of MTHI: raise new UnknownInstructionError
  of SLT: cpu[rd] = word(cpu[rs].signed < cpu[rt].signed)
  of SLTU: cpu[rd] = if cpu[rs] < cpu[rt]: 1 else: 0
  of LUI: cpu[rt] = imm.zeroExt shl 16
  of AND: cpu[rd] = cpu[rs] and cpu[rt]
  of ANDI: cpu[rt] = cpu[rs] and imm.zeroExt
  of OR: cpu[rd] = cpu[rs] or cpu[rt]
  of ORI: cpu[rt] = cpu[rs] or imm.zeroExt
  of XOR: cpu[rd] = cpu[rs] xor cpu[rt]
  of XORI: cpu[rt] = cpu[rs] xor imm.zeroExt
  of NOR: cpu[rd] = not (cpu[rs] or cpu[rt])
  of SLL: cpu[rd] = cpu[rt] shl shamt
  of SLLV: raise new UnknownInstructionError
  of SRA: cpu[rd] = (cpu[rt].signed shr shamt).unsigned
  of SRAV: raise new UnknownInstructionError
  of SRL: cpu[rd] = cpu[rt] shr shamt
  of SRLV: raise new UnknownInstructionError
  of LW: cpu[rt] = cpu.read[:uint32](cpu[rs] + imm.signExt)
  of LB: cpu[rt] = int32(cpu.read[:int8](cpu[rs] + imm.signExt)).unsigned
  of LBU: cpu[rt] = uint32(cpu.read[:uint8](cpu[rs] + imm.signExt))
  of LH: cpu[rt] = int32(cpu.read[:int16](cpu[rs] + imm.signExt)).unsigned
  of LHU: cpu[rt] = uint32(cpu.read[:uint16](cpu[rs] + imm.signExt))
  of LWL: raise new UnknownInstructionError
  of LWR: raise new UnknownInstructionError
  of SW: cpu.write[:uint32](cpu[rs] + imm.signExt, cpu[rt])
  of SB: cpu.write[:uint8](cpu[rs] + imm.signExt, cast[uint8](cpu[rt]))
  of SH: cpu.write[:uint16](cpu[rs] + imm.signExt, cast[uint16](cpu[rt]))
  of SWL: raise new UnknownInstructionError
  of SWR: raise new UnknownInstructionError
  of BEQ:
    if cpu[rs] == cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BNE:
    if cpu[rs] != cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BGEZ: raise new UnknownInstructionError
  of BGEZAL: raise new UnknownInstructionError
  of BGTZ:
    if cpu[rs].signed > 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BLEZ:
    if cpu[rs].signed <= 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BLTZ: raise new UnknownInstructionError
  of BLTZAL: raise new UnknownInstructionError
  of J: newPC = target.absTarget(cpu.nextPC)
  of JR: newPC = cpu[rs]
  of JAL:
    newPC = target.absTarget(cpu.nextPC)
    cpu[Register(31)] = cpu.nextPC + 4
  of JALR:
    newPC = cpu[rs]
    cpu[rd] = cpu.nextPC + 4
  of SYSCALL: raise new UnknownInstructionError
  of BREAK: raise new UnknownInstructionError
  of MFC0: cpu[rt] = cpu.cop0[CoRegister(rd)]
  of MTC0: cpu.cop0[CoRegister(rd)] = cpu[rt]
  of RFE: raise new UnknownInstructionError

  cpu.pc = cpu.nextPC
  cpu.nextPC = newPC

proc step(cpu: var CPU) {.inline.} =
  # The execute function is in charge of updating pc and nextPC.
  cpu.execute(cpu.fetch)

var clocks = 0

proc test() =
  var oldCPU: CPU
  var cpu = initCPU
  while true:
    oldCPU = cpu
    echo format(cpu.fetch)
    step(cpu)
    clocks += 1
    echo cpuDiff(oldCPU, cpu)

test()

# for i in 0..<100:
#   try:
#     test()
#   except CatchableError:
#     discard

echo(clocks)
