## An interpreter for the R3000A CPU.

import utils, basics, memory, gte
import std/[tables, bitops, strformat]

const loggerComponent = logCPU

# Processor state.

type
  Register* = distinct range[0..31]  ## A register.
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
  COP0 {.requiresInit.} = object
    ## COP0 status. All fields are COP0 registers.
    sr: word
    cause: word
    epc: word
    badvaddr: word
    # We don't really use these ones, but they can be get and set.
    # Details from Nocash PSX.
    bpc: word
    bda: word
    dcic: word
    bdam: word
    bpcm: word

const initCOP0: COP0 =
  # Initial value of COP0. BEV=1, everything else 0.
  COP0(sr: 1 shl 22, cause: 0, epc: 0, badvaddr: 0, bpc: 0, bda: 0,
       dcic: 0, bdam: 0, bpcm: 0)

const
  # User-settable bits in COP0.SR.
  cu = BitSlice[word, word](pos: 28, width: 4)
  bev = bit[word] 22
  cm {.used.} = bit[word] 19
  swc = bit[word] 17
  isc = bit[word] 16
  im = BitSlice[word, word](pos: 8, width: 8)
  ku = [bit[word] 1, bit[word] 3, bit[word] 5]
  ie = [bit[word] 0, bit[word] 2, bit[word] 4]
  ie0 = ie[0]

  # IRQ bits in COP0.CAUSE.
  ip = BitSlice[word, word](pos: 8, width: 8)

  # Read-write SR bits
  writableSRBits: word =
    block:
      var result: word = 0
      # Note: CM is not user-settable (and not set at all currently).
      result = result or cu.toMask
      for x in @[bev, swc, isc] & @ie & @ku:
        result = result or x.toMask
      result = result or im.toMask
      result

  # Read-only SR bits (writes to them are ignored)
  fixedSRBits = not writableSRBits

  # Read-only SR bits which should print a warning on write
  forbiddenSRBits: word =
    # Writing to CM or IM is OK, just ignored
    fixedSRBits and not cm.toMask and not im.toMask

proc `[]`(cop0: COP0, reg: CoRegister): word =
  ## Access registers by number.
  case reg
  of CoRegister(3): cop0.bpc
  of CoRegister(5): cop0.bda
  of CoRegister(6): 0 # JUMPDEST
  of CoRegister(7): cop0.dcic
  of CoRegister(8): cop0.badvaddr
  of CoRegister(9): cop0.bdam
  of CoRegister(11): cop0.bpcm
  of CoRegister(12): cop0.sr
  of CoRegister(13): cop0.cause
  of CoRegister(14): cop0.epc
  of CoRegister(15): 2 # PRId - value from Nocash PSX
  else:
    warn fmt"Ignoring read of unknown COP register {reg}"
    return 0

proc `[]=`(cop0: var COP0, reg: CoRegister, val: word) =
  ## Access registers by number.
  case reg
  of CoRegister(3): cop0.bpc = val
  of CoRegister(5): cop0.bda = val
  of CoRegister(6): discard # JUMPDEST
  of CoRegister(7): cop0.dcic = val
  of CoRegister(9): cop0.bdam = val
  of CoRegister(11): cop0.bpcm = val
  of CoRegister(12):
    let conflicting = (val and forbiddenSRBits) xor (cop0.sr and forbiddenSRBits)
    if conflicting != 0:
      warn fmt"Ignoring writes to forbidden SR bits: {conflicting:x}"
    cop0.sr = (cop0.sr and fixedSRBits) or (val and writableSRBits)
  of CoRegister(13):
    cop0.cause[ip.bit 0] = val[ip.bit 0]
    cop0.cause[ip.bit 1] = val[ip.bit 1]
  of CoRegister(14): discard
  else:
    warn fmt"Ignoring write to unknown COP register {reg}"

proc leaveKernel(cop0: var COP0) =
  # Nocash PSX: RFE leaves IEo/KUo unchanged
  for i in 0..1:
    cop0.sr[ku[i]] = cop0.sr[ku[i+1]]
    cop0.sr[ie[i]] = cop0.sr[ie[i+1]]

proc enterKernel(cop0: var COP0) =
  for i in countdown(1, 0):
    cop0.sr[ku[i+1]] = cop0.sr[ku[i]]
    cop0.sr[ie[i+1]] = cop0.sr[ie[i]]
  cop0.sr[ku[0]] = false
  cop0.sr[ie[0]] = false

type
  CPU* {.requiresInit.} = object
    ## CPU state.
    pc*: word ## Current PC.
    nextPC: word ## Next PC. Used to implement branch delay slot.
    registers: array[Register, word] ## Registers.
    lo, hi: word ## LO/HI registers.
    delayedUpdate: tuple[reg: Register, val: word] ## Load delay slot
    cop0: COP0 ## COP0 registers.
    gte: GTE ## GTE registers.

let initCPU*: CPU = block:
  # The initial state of the CPU after reset.
  const pc = 0xbfc00000u32
  var registers: array[Register, word]
  CPU(pc: pc,
      nextPC: pc+4,
      registers: registers,
      lo: 0,
      hi: 0,
      delayedUpdate: (reg: r0, val: 0u32),
      cop0: initCOP0,
      gte: initGTE)

func `[]`*(cpu: CPU, reg: Register): word =
  ## Access registers by number.
  assert cpu.registers[r0] == 0
  cpu.registers[reg]

func `[]=`*(cpu: var CPU, reg: Register, val: word) =
  ## Access registers by number. Takes care of ignoring writes to R0.
  cpu.registers[reg] = val
  cpu.registers[r0] = 0

func delayedGet(cpu: CPU, reg: Register): word =
  ## Access registers by number, but also look in the load delay slot.
  assert cpu.registers[r0] == 0
  if cpu.delayedUpdate.reg == reg:
    return cpu.delayedUpdate.val
  else:
    return cpu.registers[reg]

func setIRQ*(cpu: var CPU, irq: bool) =
  ## Update the IRQ flag.

  cpu.cop0.cause[ip.bit 2] = irq

proc jump*(cpu: var CPU, pc: word) =
  ## Jump to a particular address.

  cpu.pc = pc
  cpu.nextPC = pc+4

proc resolveAddress(cpu: CPU, address: word, kind: AccessKind): word =
  ## Resolve a virtual address to a physical address,
  ## also checking access permissions.
  if address >= 0x80000000u32 and cpu.cop0.sr[ku[0]]:
    raise MachineError(error: AddressError, address: address, kind: kind)

  # If "isolate cache" bit is set, high bits are discarded and
  # low bits are used to index into scratchpad (only for data,
  # not code).
  if kind != Fetch and cpu.cop0.sr[isc]:
    # TODO: allow scratchpad to be disabled too
    return (address and 0x3ff) + 0x1f800000

  return address # memory.nim does memory mirroring

proc fetch*(cpu: CPU): word =
  ## Fetch the next instruction.
  addressSpace.fetch(cpu.resolveAddress(cpu.pc, Fetch))

proc read*[T](cpu: CPU, address: word, time: var int64): T =
  ## Read from a given virtual address.
  addressSpace.read[:T](cpu.resolveAddress(address, Load), time)

proc write*[T](cpu: CPU, address: word, val: T) =
  ## Write to a given virtual address.
  addressSpace.write[:T](cpu.resolveAddress(address, Store), val)

# Instruction decoding and execution.

type
  Opcode* {.pure.} = enum
    ## An instruction opcode.
    # Arithmetic
    ADD, ADDI, ADDU, ADDIU, SUB, SUBU,
    DIV, DIVU, MULT, MULTU, MFLO, MTLO, MFHI, MTHI,
    SLT, SLTI, SLTU, SLTIU, LUI,
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
    SYSCALL, BREAK, MFC0, MTC0, RFE,
    # GTE
    COP2, MFC2, MTC2, CFC2, CTC2
  Immediate = distinct uint16   ## An immediate operand.
  Target = distinct range[0 .. (1 shl 26)-1] ## A 26-bit jump target.

func `$`*(x: Immediate): string =
  fmt"$0x{uint16(x):x}"

func `$`*(x: Target): string =
  fmt"$0x{word(x):x}"

func absTarget(target: Target, pc: word): word =
  ## Resolve a target address.
  (pc and 0xf000_0000u32) or (word(target) shl 2)

func zeroExt(x: Immediate): word =
  ## Zero extend an immediate to a word.
  uint32(uint16(x))

func signExt(x: Immediate): word =
  ## Sign extend an immediate to a word.
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
    Exc,   ## Only opcode and funct used, rest is user-settable (SYSCALL and BREAK)
    COP,   ## Coprocessor instruction, lowest 25 bits are opcode
    None   ## No arguments (RFE)

const opcodeType: array[Opcode, OpcodeType] =
  # Type of each opcode
  [ADD: Reg, ADDI: Imm, ADDU: Reg, ADDIU: Imm, SUB: Reg, SUBU: Reg, DIV: RegST,
   DIVU: RegST, MULT: RegST, MULTU: RegST, MFLO: RegD, MTLO: RegS, MFHI: RegD,
   MTHI: RegS, SLT: Reg, SLTI: Imm, SLTU: Reg, SLTIU: Imm, LUI: ImmT, AND: Reg,
   ANDI: Imm, OR: Reg, ORI: Imm, XOR: Reg, XORI: Imm, NOR: Reg, SLL: Shift,
   SLLV: Reg, SRA: Shift, SRAV: Reg, SRL: Shift, SRLV: Reg, LW: Mem, LB: Mem,
   LBU: Mem, LH: Mem, LHU: Mem, LWL: Mem, LWR: Mem, SW: Mem, SB: Mem, SH: Mem,
   SWL: Mem, SWR: Mem, BEQ: Imm, BNE: Imm, BGEZ: ImmS, BGEZAL: ImmS, BGTZ: ImmS,
   BLEZ: ImmS, BLTZ: ImmS, BLTZAL: ImmS, J: Jump, JR: RegS, JAL: Jump,
   JALR: RegDS, SYSCALL: Exc, BREAK: Exc, MFC0: MFC, MTC0: MTC, RFE: None,
   COP2: COP, MFC2: MFC, MTC2: MTC, CFC2: MFC, CTC2: MTC]

const
  # The different parts of a machine instruction.
  opcode = BitSlice[int, word](pos: 26, width: 6)
  rs = BitSlice[Register, word](pos: 21, width: 5)
  rt = BitSlice[Register, word](pos: 16, width: 5)
  rd = BitSlice[Register, word](pos: 11, width: 5)
  shamt = BitSlice[int, word](pos: 6, width: 5)
  funct = BitSlice[int, word](pos: 0, width: 6)
  imm = BitSlice[Immediate, word](pos: 0, width: 16)
  target = BitSlice[Target, word](pos: 0, width: 26)
  copimm = BitSlice[word, word](pos: 0, width: 25)
  exc = BitSlice[word, word](pos: 6, width: 20)

proc decode(instr: word): Opcode {.inline.} =
  ## Decode an instruction to find its opcode.

  template guard(x: untyped) =
    if not x: raise MachineError(error: ReservedInstruction, instruction: instr)

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
    else: raise MachineError(error: ReservedInstruction)
  of 1:
    # Bit 0 false: BLTZ true: BGEZ
    # Bits 1-5=10000: link, otherwise: don't link
    let op = instr[rt].int
    if op.testBit 0:
      if op == 17: instr.ret BGEZAL
      else: instr.ret BGEZ
    else:
      if op == 16: instr.ret BLTZAL
      else: instr.ret BLTZ
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
  of 10: instr.ret SLTI
  of 11: instr.ret SLTIU
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
    else: raise MachineError(error: ReservedInstruction)
  of 18:
    if int(instr[rs]).testBit(5):
      instr.ret COP2
    else:
      case int(instr[rs])
      of 0: instr.ret MFC2
      of 2: instr.ret CFC2
      of 4: instr.ret MTC2
      of 6: instr.ret CFC2
      else: raise MachineError(error: ReservedInstruction)
  else: raise MachineError(error: ReservedInstruction)

proc format*(instr: word): string =
  ## Disassemble an instruction.

  let
    op =
      try: decode(instr)
      except MachineError: return fmt"(unknown instruction {instr:08x})"
    rd = instr[rd]
    rs = instr[rs]
    rt = instr[rt]
    imm = instr[imm]
    target = instr[target]
    exc = instr[exc]
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
  of Exc: args(fmt"$0x{exc:x}")
  of COP: args(fmt"$0x{copimm}")
  of None: $op

proc `$`*(cpu: CPU): string =
  let instruction =
    try: fmt"{cpu.fetch:x}"
    except MachineError: "(invalid PC)"
  let instructionStr =
    try: cpu.fetch.format
    except MachineError: "(unknown opcode)"
  result = fmt "{instruction} {instructionStr}: PC={cpu.pc:x} "
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

proc signedAdd(x, y: word): word =
  if x.signed > 0 and y.signed > high(iword) - x.signed:
    raise MachineError(error: ArithmeticOverflow)
  if x.signed < 0 and y.signed < low(iword) - x.signed:
    raise MachineError(error: ArithmeticOverflow)
  return x + y

proc signedSub(x, y: word): word =
  # x >= 0 ==> (x-y > iword.high <==> y < x-iword.high)
  if x.signed >= 0 and y.signed < x.signed - iword.high:
    raise MachineError(error: ArithmeticOverflow)
  # x < 0 ==> (x-y < iword.low <==> y > x-iword.low)
  if x.signed < 0 and y.signed > x.signed - iword.low:
    raise MachineError(error: ArithmeticOverflow)
  return x - y

func replaceLeft(value, replacement: word, alignment: 0..3): word =
  ## Helper for LWL/SWL.

  case alignment
  of 3: replacement
  of 2: (value and 0xff) or (replacement shl 8)
  of 1: (value and 0xffff) or (replacement shl 16)
  of 0: (value and 0xffffff) or (replacement shl 24)

func replaceRight(value, replacement: word, alignment: 0..3): word =
  ## Helper for LWR/SWR.

  case alignment
  of 0: replacement
  of 1: (value and 0xff000000u32) or (replacement shr 8)
  of 2: (value and 0xffff0000u32) or (replacement shr 16)
  of 3: (value and 0xffffff00u32) or (replacement shr 24)

# Test that replaceLeft/replaceRight are working properly.
static:
  for i in 0..3:
    let start = 0x12345678u32
    let bytes = [0x0d, 0xd0, 0xfe, 0xca, 0xfe, 0xbe, 0xda, 0xde]
    proc read(i: int): word =
      word(bytes[i]) +
      (word(bytes[i+1]) shl 8) +
      (word(bytes[i+2]) shl 16) +
      (word(bytes[i+3]) shl 24)
    proc left(value: word, i: 0..3): word =
      replaceLeft(value, read((i+3) and not 3), (i+3) mod 4)
    proc right(value: word, i: 0..3): word =
      value.replaceRight(read(i and not 3), i)
    assert start.left(i).right(i) == read(i)
    assert start.right(i).left(i) == read(i)

proc execute(cpu: var CPU, instr: word, time: var int64) {.inline.} =
  ## Decode and execute an instruction.
  time += cpuClock
  var newPC = cpu.nextPC + 4

  # Load delay slot: a register write to be done *next* instruction.
  var newDelayedUpdate = (reg: r0, val: 0u32)
  # A register write to be done this instruction.
  var update = (reg: r0, val: 0u32)

  let
    op = decode(instr)
    rd = instr[rd]
    rs = instr[rs]
    rt = instr[rt]
    shamt = instr[shamt]
    imm = instr[imm]
    target = instr[target]
    copimm = instr[copimm]

  template branchIf(x: bool) =
    if x:
      newPC = cpu.nextPC + imm.signExt shl 2

  template link(r: Register) =
    r.set(cpu.nextPC + 4)

  template link() =
    link(Register(31))

  template absJump() =
    newPC = target.absTarget(cpu.nextPC)

  template logCall() =
    debug fmt"call from {cpu.pc:x} to {newPC:x}"

  template logReturn() =
    debug fmt"return from {cpu.pc:x} to {newPC:x}"

  template delayedSet(r: Register, v: word) =
    newDelayedUpdate = (reg: r, val: v)
    # According to the AmiDog tests, if you load to the same register
    # twice in consecutive instructions, the first load never takes effect
    if cpu.delayedUpdate.reg == r:
      cpu.delayedUpdate = (reg: r0, val: 0u32)

  template set(r: Register, v: word) =
    update = (reg: r, val: v)

  case op
  of ADD: rd.set(signedAdd(cpu[rs], cpu[rt]))
  of ADDI: rt.set(signedAdd(cpu[rs], imm.signExt))
  of ADDU: rd.set(cpu[rs] + cpu[rt])
  of ADDIU: rt.set(cpu[rs] + imm.signExt)
  of SUB: rd.set(signedSub(cpu[rs], cpu[rt]))
  of SUBU: rd.set(cpu[rs] - cpu[rt])
  of DIV:
    let
      x = cpu[rs].signed
      y = cpu[rt].signed

    # Info from Nocash PSX
    if y == 0:
      cpu.lo = (if x >= 0: -1 else: 1).unsigned
      cpu.hi = x.unsigned
    elif x == int32.low and y == -1:
      cpu.lo = int32.low.unsigned
      cpu.hi = 0
    else:
      cpu.lo = (x div y).unsigned
      cpu.hi = (x mod y).unsigned

  of DIVU:
    let
      x = cpu[rs]
      y = cpu[rt]

    if y == 0:
      cpu.lo = word.high
      cpu.hi = x
    else:
      cpu.lo = x div y
      cpu.hi = x mod y

  of MULT:
    let
      x = cpu[rs].signed
      y = cpu[rt].signed
      z = int64(x)*int64(y)
    cpu.lo = cast[word](z)
    cpu.hi = cast[word](z shr 32)
  of MULTU:
    let
      x = cpu[rs]
      y = cpu[rt]
      z = uint64(x)*uint64(y)
    cpu.lo = cast[word](z)
    cpu.hi = cast[word](z shr 32)
  of MFLO: rd.set(cpu.lo)
  of MTLO: cpu.lo = cpu[rs]
  of MFHI: rd.set(cpu.hi)
  of MTHI: cpu.hi = cpu[rs]
  of SLT: rd.set(word(cpu[rs].signed < cpu[rt].signed))
  of SLTI: rt.set(word(cpu[rs].signed < imm.signExt.signed))
  of SLTU: rd.set(word(cpu[rs] < cpu[rt]))
  of SLTIU: rt.set(word(cpu[rs] < imm.signExt))
  of LUI: rt.set(imm.zeroExt shl 16)
  of AND: rd.set(cpu[rs] and cpu[rt])
  of ANDI: rt.set(cpu[rs] and imm.zeroExt)
  of OR: rd.set(cpu[rs] or cpu[rt])
  of ORI: rt.set(cpu[rs] or imm.zeroExt)
  of XOR: rd.set(cpu[rs] xor cpu[rt])
  of XORI: rt.set(cpu[rs] xor imm.zeroExt)
  of NOR: rd.set(not (cpu[rs] or cpu[rt]))
  of SLL: rd.set(cpu[rt] shl shamt)
  of SLLV: rd.set(cpu[rt] shl (cpu[rs] and 0x1f))
  of SRA: rd.set((cpu[rt].signed shr shamt).unsigned)
  of SRAV: rd.set((cpu[rt].signed shr (cpu[rs] and 0x1f)).unsigned)
  of SRL: rd.set(cpu[rt] shr shamt)
  of SRLV: rd.set(cpu[rt] shr (cpu[rs] and 0x1f))
  of LW: rt.delayedSet(cpu.read[:word](cpu[rs] + imm.signExt, time))
  of LB: rt.delayedSet(iword(cpu.read[:int8](cpu[rs] + imm.signExt, time)).unsigned)
  of LBU: rt.delayedSet(word(cpu.read[:uint8](cpu[rs] + imm.signExt, time)))
  of LH: rt.delayedSet(iword(cpu.read[:int16](cpu[rs] + imm.signExt, time)).unsigned)
  of LHU: rt.delayedSet(word(cpu.read[:uint16](cpu[rs] + imm.signExt, time)))
  of LWL:
    # Skip the load delay slot for LWL/LWR
    let
      address = cpu[rs] + imm.signExt
      value = cpu.read[:word](address and not 3u32, time)
    rt.delayedSet(cpu.delayedGet(rt).replaceLeft(value, address and 3))
  of LWR:
    let
      address = cpu[rs] + imm.signExt
      value = cpu.read[:word](address and not 3u32, time)
    rt.delayedSet(cpu.delayedGet(rt).replaceRight(value, address and 3))
  of SW: cpu.write[:word](cpu[rs] + imm.signExt, cpu[rt])
  of SB: cpu.write[:uint8](cpu[rs] + imm.signExt, cast[uint8](cpu[rt]))
  of SH: cpu.write[:uint16](cpu[rs] + imm.signExt, cast[uint16](cpu[rt]))
  of SWL:
    var dummyTime: int64 # On an actual MIPS this doesn't do a load
    let
      address = cpu[rs] + imm.signExt
      value = cpu.read[:word](address and not 3u32, dummyTime)
      newValue = value.replaceLeft(cpu[rt], address and 3)
    cpu.write[:word](address and not 3u32, newValue)
  of SWR:
    var dummyTime: int64 # On an actual MIPS this doesn't do a load
    let
      address = cpu[rs] + imm.signExt
      value = cpu.read[:word](address and not 3u32, dummyTime)
      newValue = value.replaceRight(cpu[rt], address and 3)
    cpu.write[:word](address and not 3u32, newValue)
  of BEQ: branchIf(cpu[rs] == cpu[rt])
  of BNE: branchIf(cpu[rs] != cpu[rt])
  of BGEZ: branchIf(cpu[rs].signed >= 0)
  of BGEZAL: branchIf(cpu[rs].signed >= 0); link()
  of BGTZ: branchIf(cpu[rs].signed > 0)
  of BLEZ: branchIf(cpu[rs].signed <= 0)
  of BLTZ: branchIf(cpu[rs].signed < 0)
  of BLTZAL: branchIf(cpu[rs].signed < 0); link()
  of J: absJump()
  of JR:
    newPC = cpu[rs]
    if rs.int == 31: logReturn()
  of JAL: absJump; link(); logCall()
  of JALR: newPC = cpu[rs]; link(rd); logCall()
  of SYSCALL: raise MachineError(error: SystemCall)
  of BREAK: raise MachineError(error: Breakpoint)
  of MFC0: rt.set(cpu.cop0[CoRegister(rd)])
  of MTC0: cpu.cop0[CoRegister(rd)] = cpu[rt]
  of RFE: leaveKernel(cpu.cop0)
  of COP2: cpu.gte.execute(copimm.int)
  of MFC2: rt.delayedSet(cpu.gte[rd.int.dataReg])
  of MTC2: cpu.gte[rd.int.dataReg] = cpu[rt]
  of CFC2: rt.delayedSet(cpu.gte[rd.int.controlReg])
  of CTC2: cpu.gte[rd.int.controlReg] = cpu[rt]

  cpu.pc = cpu.nextPC
  cpu.nextPC = newPC

  # The PSX bios has the following instruction sequence:
  #   LW r9, $0x64(r29)
  #   ADDIU r9, r0, $0x1
  # where (apparently) the final value of r9 should be taken from the ADDIU.
  # Hence we process the delayed update before the normal update.
  cpu[cpu.delayedUpdate.reg] = cpu.delayedUpdate.val
  cpu[update.reg] = update.val
  cpu.delayedUpdate = newDelayedUpdate

# Exception handling

func exceptionCode(error: MachineError): int =
  ## Convert a MachineError to an error code to be stored in COP0.SR.ExcCode.
  case error.error
  of Interrupt: 0
  of AddressError:
    case error.kind
    of Fetch, Load: 4
    of Store: 5
  of BusError:
    case error.kind
    of Fetch: 6
    of Load, Store: 7
  of SystemCall: 8
  of Breakpoint: 9
  of ReservedInstruction: 10
  of CoprocessorUnusable: 11
  of ArithmeticOverflow: 12

proc handleException(cpu: var CPU, error: MachineError) =
  ## Handle an exception, by jumping to the exception vector etc.

  if error.error.isError:
    info fmt"{error.error} interrupt: {cpu}"
  else:
    debug fmt"{error.error} exception: {cpu}"

  var branchDelay: bool = false

  if cpu.nextPC == cpu.pc + 4:
    cpu.cop0.epc = cpu.pc
  else:
    # Exception in branch delay slot
    branchDelay = true
    cpu.cop0.epc = cpu.pc - 4

  let cop =
    case error.error
    of CoprocessorUnusable: error.cop
    else: 0

  cpu.cop0.cause =
    (word(error.exceptionCode) shl 2) or
    (word(cop) shl 28) or
    (word(branchDelay) shl 31) or
    (cpu.cop0.cause and ip.toMask)

  if error.error == AddressError:
    cpu.cop0.badvaddr = error.address

  cpu.cop0.enterKernel

  cpu.jump(if cpu.cop0.sr[bev]: 0xbfc00180u32 else: 0x80000080u32)

# TODO: this violates aliasing rules - irq.nim modifies the global CPU.
proc step*(cpu: var CPU, time: var int64) {.inline.} =
  try:
    # Check for IRQs first.
    if cpu.cop0.sr[ie0] and (cpu.cop0.sr[im] and cpu.cop0.cause[ip]) != 0:
      raise MachineError(error: Interrupt)

    let
      oldCPU = cpu
      instr = cpu.fetch
    # The execute function is in charge of updating pc and nextPC.
    cpu.execute(instr, time)
    trace fmt"{instr.format} {cpuDiff(oldCPU, cpu)}"
  except MachineError as error:
    cpu.handleException(error)

var
  cpu*: CPU = initCPU ## The main processor
