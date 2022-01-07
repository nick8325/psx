## An interpreter for the R3000A CPU.

import utils, common
from memory import nil
import fusion/matching
import std/[tables, bitops, strformat]
import sugar

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
  memory.fetch(cpu.resolveAddress(cpu.pc, true))

proc read*[T](cpu: CPU, address: uint32): T =
  ## Read from a given virtual address.
  memory.read[T](cpu.resolveAddress(address, false))

proc write*[T](cpu: CPU, address: uint32, val: T) =
  ## Write to a given virtual address.
  memory.write[T](cpu.resolveAddress(address, false), val)

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
    SYSCALL, BREAK, MFC0, MTC0
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

proc decode*(instr: uint32): Opcode {.inline.} =
  ## Decode an instruction to find its opcode.

  proc guard(op: Opcode, x: bool): Opcode {.inline.} =
    ## Return an opcode, but only after checking that a given condition holds.
    if x:
      return op
    else:
      raise new UnknownInstructionError

  return case instr[opcode]
  of 0:
    case instr[funct]
    of 0: SLL.guard(instr[rs] == r0)
    of 2: SRL.guard(instr[rs] == r0)
    of 3: SRA.guard(instr[rs] == r0)
    of 4: SLLV.guard(instr[shamt] == 0)
    of 6: SRLV.guard(instr[shamt] == 0)
    of 7: SRAV.guard(instr[shamt] == 0)
    of 8: JR.guard(instr[shamt] == 0)
    of 9: JALR.guard(instr[shamt] == 0)
    of 12: SYSCALL
    of 13: BREAK
    of 16: MFHI.guard(instr[shamt] == 0)
    of 17: MTHI.guard(instr[shamt] == 0)
    of 18: MFLO.guard(instr[shamt] == 0)
    of 19: MTLO.guard(instr[shamt] == 0)
    of 24: MULT.guard(instr[shamt] == 0)
    of 25: MULTU.guard(instr[shamt] == 0)
    of 26: DIV.guard(instr[shamt] == 0)
    of 27: DIVU.guard(instr[shamt] == 0)
    of 32: ADD.guard(instr[shamt] == 0)
    of 33: ADDU.guard(instr[shamt] == 0)
    of 34: SUB.guard(instr[shamt] == 0)
    of 35: SUBU.guard(instr[shamt] == 0)
    of 36: AND.guard(instr[shamt] == 0)
    of 37: OR.guard(instr[shamt] == 0)
    of 38: XOR.guard(instr[shamt] == 0)
    of 39: NOR.guard(instr[shamt] == 0)
    of 42: SLT.guard(instr[shamt] == 0)
    of 43: SLTU.guard(instr[shamt] == 0)
    else: raise new UnknownInstructionError
  of 1:
    case int(instr[rt])
    of 0: BLTZ
    of 1: BGEZ
    of 16: BLTZAL
    of 17: BGEZAL
    else: raise new UnknownInstructionError
  of 2: J
  of 3: JAL
  of 4: BEQ
  of 5: BNE
  of 6: BLEZ.guard(instr[rt] == r0)
  of 7: BGTZ.guard(instr[rt] == r0)
  of 8: ADDI
  of 9: ADDIU
  of 10: SUBI
  of 11: SUBIU
  of 12: ANDI
  of 13: ORI
  of 14: XORI
  of 15: LUI
  of 32: LB
  of 33: LH
  of 34: LWL
  of 35: LW
  of 36: LBU
  of 37: LHU
  of 38: LWR
  of 40: SB
  of 41: SH
  of 42: SWL
  of 43: SW
  of 46: SWR
  of 16:
    case int(instr[rs])
    of 0: MFC0
    of 4: MTC0
    else: raise new UnknownInstructionError
  else: raise new UnknownInstructionError

proc format*(instr: uint32): string =
  let
    op = decode(instr)
    rd = instr[rd]
    rs = instr[rs]
    rt = instr[rt]
    target = instr[target]
    shamt = instr[shamt]
    imm = instr[imm]

  proc args(args: varargs[string, `$`]): string =
    result = $op
    for i, arg in args:
      if i != 0: result &= ","
      result &= " " & arg

  let
    r = () => args(rd, rs, rt)
    rst = () => args(rs, rt)
    rdd = () => args(rd)
    rss = () => args(rs)
    rds = () => args(rd, rs)
    shift = () => args(rd, rt, shamt)
    i = () => args(rt, rs, imm)
    it = () => args(rt, imm)
    iss = () => args(rs, imm)
    j = () => args(target)
    mem = () => args(rt, fmt"{imm}({rs})")
    whole = () => args(fmt"$0x{instr and 0x3ffffff:x}")
    mfc0 = () => args(rt, CoRegister(rd))
    mtc0 = () => args(CoRegister(rd), rt)

  let
    kinds: array[Opcode, () -> string] =
      [ADD: r, ADDI: i, ADDU: r, ADDIU: i, SUB: r, SUBU: i, SUBI: r, SUBIU: i,
       DIV: rst, DIVU: rst, MULT: rst, MULTU: rst, MFLO: rdd, MTLO: rss,
       MFHI: rdd, MTHI: rss, SLT: r, SLTU: r, LUI: it,
       AND: r, ANDI: i, OR: r, ORI: i, XOR: r, XORI: i, NOR: r,
       SLL: shift, SLLV: r, SRA: shift, SRAV: r, SRL: shift, SRLV: r,
       LW: mem, LB: mem, LBU: mem, LH: mem, LHU: mem, LWL: mem, LWR: mem,
       SW: mem, SB: mem, SH: mem, SWL: mem, SWR: mem,
       BEQ: i, BNE: i, BGEZ: iss, BGEZAL: iss, BGTZ: iss, BLEZ: iss,
       BLTZ: iss, BLTZAL: iss,
       J: j, JR: rss, JAL: j, JALR: rds,
       SYSCALL: whole, BREAK: whole, MFC0: mfc0, MTC0: mtc0]

  return kinds[op]()

proc execute(cpu: var CPU, instr: uint32) {.inline.} =
  var newPC = cpu.nextPC + 4
  let op = decode(instr)
  let rd = instr[rd]
  let rs = instr[rs]
  let rt = instr[rt]
  let target = instr[target]
  let shamt = instr[shamt]
  let imm = instr[imm]

  case op
  of OR: cpu[rd] = cpu[rs] or cpu[rt]
  of AND: cpu[rd] = cpu[rs] and cpu[rt]
  of SLTU: cpu[rd] = if cpu[rs] < cpu[rt]: 1 else: 0
  of SLT: cpu[rd] = word(cpu[rs].signed < cpu[rt].signed)
  of ADDU: cpu[rd] = cpu[rs] + cpu[rt]
  of ADD:
    let
      src1 = cpu[rs].signed
      src2 = cpu[rt].signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise new ArithmeticOverflowError
    cpu[rd] = src1.unsigned + src2.unsigned
  of SUBU: cpu[rd] = cpu[rs] - cpu[rt]
  of SUBIU: cpu[rt] = cpu[rs] - imm.signExt
  of JR: newPC = cpu[rs]
  of SLL: cpu[rd] = cpu[rs] shl shamt
  of SRL: cpu[rd] = cpu[rs] shr shamt
  of SRA: cpu[rd] = (cpu[rs].signed shr shamt).unsigned
  of LUI: cpu[rt] = imm.zeroExt shl 16
  of ORI: cpu[rt] = cpu[rs] or imm.zeroExt
  of ADDIU: cpu[rt] = cpu[rs] + imm.signExt
  of ADDI:
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise new ArithmeticOverflowError
    cpu[rt] = src1.unsigned + src2.unsigned
  of SUBI:
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 < low(int32) + src1:
      raise new ArithmeticOverflowError
    if src1 < 0 and src2 > high(int32) + src1:
      raise new ArithmeticOverflowError
    cpu[rt] = src1.unsigned - src2.unsigned
  of SW: cpu.write[:uint32](cpu[rs] + imm.signExt, cpu[rt])
  of LW: cpu[rt] = cpu.read[:uint32](cpu[rs] + imm.signExt)
  of BNE:
    if cpu[rs] != cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BEQ:
    if cpu[rs] == cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BLEZ:
    if cpu[rs].signed <= 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BGTZ:
    if cpu[rs].signed > 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of SH: cpu.write[:uint16](cpu[rs] + imm.signExt, cast[uint16](cpu[rt]))
  of SB: cpu.write[:uint8](cpu[rs] + imm.signExt, cast[uint8](cpu[rt]))
  of ANDI: cpu[rt] = cpu[rs] and imm.zeroExt
  of LB: cpu[rt] = int32(cpu.read[:int8](cpu[rs] + imm.signExt)).unsigned
  of LBU: cpu[rt] = uint32(cpu.read[:uint8](cpu[rs] + imm.signExt))
  of LH: cpu[rt] = int32(cpu.read[:int16](cpu[rs] + imm.signExt)).unsigned
  of LHU: cpu[rt] = uint32(cpu.read[:uint16](cpu[rs] + imm.signExt))
  of J: newPC = target.absTarget(cpu.nextPC)
  of JALR:
    newPC = cpu[rs]
    cpu[Register(31)] = cpu.nextPC + 4
  of JAL:
    newPC = target.absTarget(cpu.nextPC)
    cpu[Register(31)] = cpu.nextPC + 4
  of MTC0: cpu.cop0[CoRegister(rd)] = cpu[rt]
  of MFC0: cpu[rt] = cpu.cop0[CoRegister(rd)]
  else:
    raise new UnknownInstructionError

  cpu.pc = cpu.nextPC
  cpu.nextPC = newPC

proc step(cpu: var CPU) {.inline.} =
  # The execute function is in charge of updating pc and nextPC.
  cpu.execute(cpu.fetch)

var clocks = 0

proc test() =
#  var oldCPU: CPU
  var cpu = initCPU
  while true:
#    oldCPU = cpu
    step(cpu)
    clocks += 1
#    echo cpuDiff(oldCPU, cpu)

for i in 0..<100:
  try:
    test()
  except CatchableError:
    discard

echo(clocks)
