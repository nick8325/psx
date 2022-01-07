## An interpreter for the R3000A CPU.

import utils, common
from memory import nil
import fusion/matching
import std/[setutils, tables, options, bitops, strformat]
{.experimental: "caseStmtMacros".}

# Processor state.

type
  Register* = distinct range[0..31] ## A register.
  CoRegister = distinct range[0..31] ## A coprocessor register.

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
  else: raise invalidCOPError # TODO Is this the right exception?

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
      raise invalidCOPError # TODO Is this the right exception?
    cop0.sr = val
  of CoRegister(13): discard
  of CoRegister(14): discard
  else: raise invalidCopError # TODO Is this the right exception?

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

func cpuDiff(cpu1: CPU, cpu2: CPU): string =
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
    SLL, SRL, SRA, SLLV, SRLV, SRAV, JR, JALR, SYSCALL, BREAK, MFHI, MTHI, MFLO,
    MTLO, MULT, MULTU, DIV, DIVU, ADD, ADDU, SUB, SUBU, AND, OR, XOR, NOR, SLT,
    SLTU, BLTZ, BGEZ, BLTZAL, BGEZAL, J, JAL, BEQ, BNE, BLEZ, BGTZ, ADDI, ADDIU,
    SUBI, SUBIU, ANDI, ORI, XORI, LUI, LB, LH, LWL, LW, LBU, LHU, LWR, SB, SH,
    SWL, SW, SWR, MFC0, MTC0

  Immediate = distinct uint16   ## An immediate operand.
  Target = distinct range[0 .. (1 shl 26)-1] ## A 26-bit jump target.

func absTarget(target: Target, pc: uint32): uint32 =
  ## Resolve a target address.
  (pc and 0xf000_0000u32) or (uint32(target) shl 2)

func zeroExt(x: Immediate): uint32 =
  ## Zero extend an immediate to a uint32.
  uint32(uint16(x))

func signExt(x: Immediate): uint32 =
  ## Sign extend an immediate to a uint32.
  cast[uint32](int32(cast[int16](uint16(x))))

func `$`*(x: Immediate): string =
  fmt"$0x{uint16(x):x}"

func `$`*(x: Target): string =
  fmt"$0x{word(x):x}"

const
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
  case instr[opcode]
  of 0:
    case instr[funct]
    of 0:
      if instr[rs] == Register(0): return SLL
    of 2:
      if instr[rs] == Register(0): return SRL
    of 3:
      if instr[rs] == Register(0): return SRA
    of 4:
      if instr[shamt] == 0: return SLLV
    of 6:
      if instr[shamt] == 0: return SRLV
    of 7:
      if instr[shamt] == 0: return SRAV
    of 8:
      if instr[shamt] == 0: return JR
    of 9:
      if instr[shamt] == 0: return JALR
    of 12: return SYSCALL
    of 13: return BREAK
    of 16:
      if instr[shamt] == 0: return MFHI
    of 17:
      if instr[shamt] == 0: return MTHI
    of 18:
      if instr[shamt] == 0: return MFLO
    of 19:
      if instr[shamt] == 0: return MTLO
    of 24:
      if instr[shamt] == 0: return MULT
    of 25:
      if instr[shamt] == 0: return MULTU
    of 26:
      if instr[shamt] == 0: return DIV
    of 27:
      if instr[shamt] == 0: return DIVU
    of 32:
      if instr[shamt] == 0: return ADD
    of 33:
      if instr[shamt] == 0: return ADDU
    of 34:
      if instr[shamt] == 0: return SUB
    of 35:
      if instr[shamt] == 0: return SUBU
    of 36:
      if instr[shamt] == 0: return AND
    of 37:
      if instr[shamt] == 0: return OR
    of 38:
      if instr[shamt] == 0: return XOR
    of 39:
      if instr[shamt] == 0: return NOR
    of 42:
      if instr[shamt] == 0: return SLT
    of 43:
      if instr[shamt] == 0: return SLTU
    else: raise unknownInstructionError
  of 1:
    case int(instr[rt])
    of 0: return BLTZ
    of 1: return BGEZ
    of 16: return BLTZAL
    of 17: return BGEZAL
    else: raise unknownInstructionError
  of 2: return J
  of 3: return JAL
  of 4: return BEQ
  of 5: return BNE
  of 6:
    if instr[rt] == Register(0): return BLEZ
  of 7:
    if instr[rt] == Register(0): return BGTZ
  of 8: return ADDI
  of 9: return ADDIU
  of 10: return SUBI
  of 11: return SUBIU
  of 12: return ANDI
  of 13: return ORI
  of 14: return XORI
  of 15: return LUI
  of 32: return LB
  of 33: return LH
  of 34: return LWL
  of 35: return LW
  of 36: return LBU
  of 37: return LHU
  of 38: return LWR
  of 40: return SB
  of 41: return SH
  of 42: return SWL
  of 43: return SW
  of 46: return SWR
  of 16:
    case int(instr[rs])
    of 0: return MFC0
    of 4: return MTC0
    else: raise unknownInstructionError
  else: raise unknownInstructionError

  raise unknownInstructionError

const
  debug = false
template log(str: untyped) =
  if debug:
    echo str

proc execute(cpu: var CPU, op: Opcode, instr: uint32) =
  var newPC = cpu.nextPC + 4
  let rd = instr[rd]
  let rs = instr[rs]
  let rt = instr[rt]
  let target = instr[target]
  let shamt = instr[shamt]
  let imm = instr[imm]

  case op
  of OR:
    log fmt"OR {rd}, {rs}, {rt}"
    cpu[rd] = cpu[rs] or cpu[rt]
  of AND:
    log fmt"AND {rd}, {rs}, {rt}"
    cpu[rd] = cpu[rs] and cpu[rt]
  of SLTU:
    log fmt"SLTU {rd}, {rs}, {rt}"
    cpu[rd] = if cpu[rs] < cpu[rt]: 1 else: 0
  of SLT:
    log fmt"SLT {rd}, {rs}, {rt}"
    cpu[rd] = word(cpu[rs].signed < cpu[rt].signed)
  of ADDU:
    log fmt"ADDU {rd}, {rs}, {rt}"
    cpu[rd] = cpu[rs] + cpu[rt]
  of ADD:
    log fmt"ADD {rd}, {rs}, {rt}"
    let
      src1 = cpu[rs].signed
      src2 = cpu[rt].signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise overflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise overflowError
    cpu[rd] = src1.unsigned + src2.unsigned
  of SUBU:
    log fmt"SUBU {rd}, {rs}, {rt}"
    cpu[rd] = cpu[rs] - cpu[rt]
  of SUBIU:
    log fmt"SUBIU {rt}, {rs}, {imm}"
    cpu[rt] = cpu[rs] - imm.signExt
  of JR:
    log fmt"JR {rs}"
    newPC = cpu[rs]
  of SLL:
    log fmt"SLL {rd}, {rs}, {shamt}"
    cpu[rd] = cpu[rs] shl shamt
  of SRL:
    log fmt"SRL {rd}, {rs}, {shamt}"
    cpu[rd] = cpu[rs] shr shamt
  of SRA:
    log fmt"SRA {rd}, {rs}, {shamt}"
    cpu[rd] = (cpu[rs].signed shr shamt).unsigned
  of LUI:
    log fmt"LUI {rt}, {imm}"
    cpu[rt] = imm.zeroExt shl 16
  of ORI:
    log fmt"ORI {rt}, {rs}, {imm}"
    cpu[rt] = cpu[rs] or imm.zeroExt
  of ADDIU:
    log fmt"ADDIU {rt}, {rs}, {imm}"
    cpu[rt] = cpu[rs] + imm.signExt
  of ADDI:
    log fmt"ADDI {rt}, {rs}, {imm}"
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 > high(int32) - src1:
      raise overflowError
    if src1 < 0 and src2 < low(int32) - src1:
      raise overflowError
    cpu[rt] = src1.unsigned + src2.unsigned
  of SUBI:
    log fmt"SUBI {rt}, {rs}, {imm}"
    let
      src1 = cpu[rs].signed
      src2 = imm.signExt.signed
    if src1 > 0 and src2 < low(int32) + src1:
      raise overflowError
    if src1 < 0 and src2 > high(int32) + src1:
      raise overflowError
    cpu[rt] = src1.unsigned - src2.unsigned
  of SW:
    log fmt"SW {rt}, {rs}, {imm}"
    cpu.write[:uint32](cpu[rs] + imm.signExt, cpu[rt])
  of LW:
    log fmt"LW {rt}, {rs}, {imm}"
    cpu[rt] = cpu.read[:uint32](cpu[rs] + imm.signExt)
  of BNE:
    log fmt"BNE {rs}, {rt}, {imm}"
    if cpu[rs] != cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BEQ:
    log fmt"BEQ {rs}, {rt}, {imm}"
    if cpu[rs] == cpu[rt]:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BLEZ:
    log fmt"BLEZ {rs}, {imm}"
    if cpu[rs].signed <= 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of BGTZ:
    log fmt"BGTZ {rs}, {imm}"
    if cpu[rs].signed > 0:
      newPC = cpu.nextPC + imm.signExt shl 2
  of SH:
    log fmt"SH {rt}, {rs}, {imm}"
    cpu.write[:uint16](cpu[rs] + imm.signExt, cast[uint16](cpu[rt]))
  of SB:
    log fmt"SB {rt}, {rs}, {imm}"
    cpu.write[:uint8](cpu[rs] + imm.signExt, cast[uint8](cpu[rt]))
  of ANDI:
    log fmt"ANDI {rt}, {rs}, {imm}"
    cpu[rt] = cpu[rs] and imm.zeroExt
  of LB:
    log fmt"LB {rt}, {rs}, {imm}"
    cpu[rt] = int32(cpu.read[:int8](cpu[rs] + imm.signExt)).unsigned
  of LBU:
    log fmt"LBU {rt}, {rs}, {imm}"
    cpu[rt] = uint32(cpu.read[:uint8](cpu[rs] + imm.signExt))
  of LH:
    log fmt"LH {rt}, {rs}, {imm}"
    cpu[rt] = int32(cpu.read[:int16](cpu[rs] + imm.signExt)).unsigned
  of LHU:
    log fmt"LHU {rt}, {rs}, {imm}"
    cpu[rt] = uint32(cpu.read[:uint16](cpu[rs] + imm.signExt))
  of J:
    log fmt"J {target}"
    newPC = target.absTarget(cpu.nextPC)
  of JALR:
    log fmt"JALR {rs}"
    newPC = cpu[rs]
    cpu[Register(31)] = cpu.nextPC + 4
  of JAL:
    log fmt"JAL {target}"
    newPC = target.absTarget(cpu.nextPC)
    cpu[Register(31)] = cpu.nextPC + 4
  of MTC0:
    log fmt "MTC0 {CoRegister(rd)}, {rt}"
    cpu.cop0[CoRegister(rd)] = cpu[rt]
  of MFC0:
    log fmt "MFC0 {rt}, {CoRegister(rd)}"
    cpu[rt] = cpu.cop0[CoRegister(rd)]
  else:
    log fmt"Unknown opcode {op}"
    raise unknownInstructionError

  cpu.pc = cpu.nextPC
  cpu.nextPC = newPC

proc step(cpu: var CPU) =
  let instr = cpu.fetch()
  # The execute function is in charge of updating pc and nextPC.
  cpu.execute(decode(instr), instr)

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
