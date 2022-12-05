## Geometry transformation engine.

import basics, utils
import std/[strformat, bitops, math]
import glm

const loggerComponent = logGTE
logGTE.level = lvlDebug

######################################################################
# Registers and state.

type
  Register* = enum
    ## A GTE register name.
    # Data registers
    VXY0, VZ0, VXY1, VZ1, VXY2, VZ2, RGBC, OTZ, IR0, IR1, IR2, IR3,
    SXY0, SXY1, SXY2, SXYP, SZ0, SZ1, SZ2, SZ3, RGB0, RGB1, RGB2,
    RES1, MAC0, MAC1, MAC2, MAC3, IRGB, ORGB, LZCS, LZCR,
    # Control registers
    RT1, RT2, RT3, RT4, RT5, TRX, TRY, TRZ, LL1, LL2, LL3, LL4, LL5,
    RBC, GBC, BBC, LC1, LC2, LC3, LC4, LC5, RFC, GFC, BFC,
    OFX, OFY, H, DQA, DQB, ZSF3, ZSF4, FLAG

  RegisterVal {.union.} = object
    ## The value held in a register, interpreted as some datatype.
    uint32: uint32
    int32: int32
    uint16s: tuple[low, high: uint16]
    int16s: tuple[low, high: int16]
    uint16: uint16
    int16: int16
    rgbc: PackedRGBC

  PackedRGBC = tuple[red, green, blue, code: uint8]

  GTE* = object
    ## The state of the GTE.
    # Note: we ignore the values of SXYP - this is a mirror of SXY2.
    # We also ignore the values of IRGB and ORGB - these are computed
    # from IR1-IR3.
    registers: array[Register, RegisterVal]
    ## Higher-accuracy versions of MAC1-MAC3.
    accMAC: Vec3l

static:
  assert Register.high.int == 63
  assert RegisterVal.sizeof == 4

proc `$`*(gte: GTE): string =
  for reg in Register:
    if reg notin {SXYP, IRGB, ORGB}:
      result &= fmt"{reg.int}/{reg}: {gte.registers[reg].uint32:x} "
  for i in 0..2:
    result &= fmt" accMAC{i}={gte.accMAC[i]:x}"

func gteDiff(gte1, gte2: GTE): string {.used.} =
  ## Show the difference between two GTE states.
  for r, x in gte1.registers:
    if x != gte2.registers[r]:
      result &= fmt "{r}={x.uint32:x}->{gte2.registers[r].uint32:x} "
  for i in 0..2:
    if gte1.accMAC[i] != gte2.accMAC[i]:
        result &= fmt "accMAC{i}={gte1.accMAC[i]:x}->{gte2.accMAC[i]:x} "

func signExtendFrom(vec: Vec3l, bit: int): Vec3l =
  for i in 0..2:
    result[i] = vec[i].extendFrom(bit)

######################################################################
# High-level access to the state.

# The GTE registers above are a mishmash of different things:
# * Some hold a signed 32-bit value (e.g. MAC0-3).
# * Some hold a 16-bit value, signed (IR0) or unsigned (H).
# * Sometimes a group of registers represents a 3-vector (IR1-3)
#   or 3x3 matrix (RT1-RT5). 3-vectors can be stored in three
#   registers or packed into two (if the elements are 16-bit).
#
# The functions here provide a higher-level view of the state.
# All registers have getter and setter functions. The getter takes
# care of extracting and zero-/sign-extending the value.
# The setter clamps the value to the correct range, and sets
# the appropriate flag bits on over-/underflow.
#
# Although GTE registers are (at most) 32-bit, when implementing the
# GTE, we need to do some internal calculations at 64-bit precision.
# The getters and setters therefore use int64 values.
# Scalar registers are modelled as a single int64:
#   proc MAC0(gte: GTE): int64
#   proc `MAC0=`(gte: var GTE, val: int64)
# Vector registers are modelled as a Vec3l = Vec3[int64]:
#   proc MAC(gte: GTE): Vec3l
#   proc `MAC=`(gte: var GTE, ir: Vec3l)
# Matrix registers are modelled as a Mat3x3l = Mat3x3[int64]:
#   proc RTM(gte: GTE): Mat3x3l

# First, helper functions used by the getters and setters
proc clamp[T](gte: var GTE, val: int64, lo, hi: T, bit: int): T =
  ## Clamp a value to a given range. Sets a flag on over-/underflow.
  if val < lo.int64 or val > hi.int64:
    gte.registers[FLAG].uint32.setBit bit
  val.clamp(lo.int64, hi.int64).T

proc vec3s(gte: GTE, xy: Register, z: Register): Vec3l =
  ## Convert a pair of XY register and Z register to a vector.
  ## The Z register is interpreted as signed.
  vec3(gte.registers[xy].int16s[0].int64,
       gte.registers[xy].int16s[1].int64,
       gte.registers[z].int16.int64)

proc vec3u(gte: GTE, xy: Register, z: Register): Vec3l =
  ## Convert a pair of XY register and Z register to a vector.
  ## The Z register is interpreted as unsigned.
  vec3(gte.registers[xy].int16s[0].int64,
       gte.registers[xy].int16s[1].int64,
       gte.registers[z].uint16.int64)

proc vec3(gte: GTE, x: Register, y: Register, z: Register): Vec3l =
  ## Convert a triple of registers to a vector.
  vec3(gte.registers[x].int32.int64,
       gte.registers[y].int32.int64,
       gte.registers[z].int32.int64)

proc mat3x3(gte: GTE, r1, r5: Register): Mat3x3l =
  ## Convert a sequence of 5 registers to a 3x3 matrix.
  let r2 = r1.succ
  let r3 = r2.succ
  let r4 = r3.succ
  assert r5 == r4.succ
  # glm stores matrices in column-major form
  result[0,0] = gte.registers[r1].int16s[0]
  result[1,0] = gte.registers[r1].int16s[1]
  result[2,0] = gte.registers[r2].int16s[0]
  result[0,1] = gte.registers[r2].int16s[1]
  result[1,1] = gte.registers[r3].int16s[0]
  result[2,1] = gte.registers[r3].int16s[1]
  result[0,2] = gte.registers[r4].int16s[0]
  result[1,2] = gte.registers[r4].int16s[1]
  result[2,2] = gte.registers[r5].int16

proc rgb(rgbc: PackedRGBC): Vec3l =
  ## Convert an RGBC value to an RGB 3-vector.
  vec3(rgbc.red.int64, rgbc.green.int64, rgbc.blue.int64)

# Now the getters and setters themselves
proc V0(gte: GTE): Vec3l = gte.vec3s(VXY0, VZ0)
proc V1(gte: GTE): Vec3l = gte.vec3s(VXY1, VZ1)
proc V2(gte: GTE): Vec3l = gte.vec3s(VXY2, VZ2)

proc RGBC(gte: GTE): PackedRGBC =
  gte.registers[RGBC].rgbc
proc RGB(gte: GTE): Vec3l = gte.RGBC.rgb

proc `OTZ=`(gte: var GTE, otz: int64) =
  gte.registers[OTZ].uint16 = gte.clamp(otz, 0u16, 0xffff, 18)

proc IR0(gte: GTE): int64 = gte.registers[IR0].int16.int64
proc `IR0=`(gte: var GTE, ir: int64) =
  gte.registers[IR0].int16 = gte.clamp(ir, 0i16, 0x1000, 12)

proc IR1(gte: GTE): int64 = gte.registers[IR1].int16.int64
proc IR2(gte: GTE): int64 = gte.registers[IR2].int16.int64
proc IR3(gte: GTE): int64 = gte.registers[IR3].int16.int64

proc IR(gte: GTE): Vec3l = vec3(gte.IR1, gte.IR2, gte.IR3)
proc setIR(gte: var GTE, lm: bool, ir: Vec3l, rtpsBug: bool = false) =
  let lo: int16 = if lm: 0 else: -0x8000
  gte.registers[IR1].int16 = gte.clamp(ir[0], lo, 0x7fff, 24)
  gte.registers[IR2].int16 = gte.clamp(ir[1], lo, 0x7fff, 23)
  if rtpsBug:
    # Handle the bug with IR3 saturation in RTPS/RTPT
    # (see Nocash "When using RTP with sf=0...")
    gte.registers[IR3].int16 = clamp(ir[2], lo, 0x7fff).int16
    discard gte.clamp(ir[2] shr 12, -0x8000, 0x7fff, 22)
  else:
    gte.registers[IR3].int16 = gte.clamp(ir[2], lo, 0x7fff, 22)

proc S0(gte: GTE): Vec3l = gte.vec3u(SXY0, SZ0)
proc S1(gte: GTE): Vec3l = gte.vec3u(SXY1, SZ1)
proc S2(gte: GTE): Vec3l = gte.vec3u(SXY2, SZ2)
proc SZ0(gte: GTE): int64 = gte.registers[SZ0].uint16.int64
proc SZ1(gte: GTE): int64 = gte.registers[SZ1].uint16.int64
proc SZ2(gte: GTE): int64 = gte.registers[SZ2].uint16.int64
proc SZ3(gte: GTE): int64 = gte.registers[SZ3].uint16.int64

proc `SX2=`(gte: var GTE, val: int64) =
  gte.registers[SXY2].int16s.low =
    gte.clamp[:int16](val, -0x400, 0x3ff, 14)

proc `SY2=`(gte: var GTE, val: int64) =
  gte.registers[SXY2].int16s.high =
    gte.clamp[:int16](val, -0x400, 0x3ff, 13)

proc `SZ3=`(gte: var GTE, val: int64) =
  gte.registers[SZ3].uint16 =
    gte.clamp[:uint16](val, 0, 0xffff, 18)

proc RGB0(gte: GTE): Vec3l = gte.registers[RGB0].rgbc.rgb
proc RGB1(gte: GTE): Vec3l {.used.} = gte.registers[RGB1].rgbc.rgb
proc RGB2(gte: GTE): Vec3l {.used.} = gte.registers[RGB2].rgbc.rgb
proc `RGB2=`(gte: var GTE, rgb: Vec3l) =
  gte.registers[RGB2].rgbc =
    (red: rgb[0].clamp(0, 255).uint8,
     green: rgb[1].clamp(0, 255).uint8,
     blue: rgb[2].clamp(0, 255).uint8,
     code: gte.RGBC.code)
  if rgb[0] < 0 or rgb[0] > 255: gte.registers[FLAG].uint32.setBit 21
  if rgb[1] < 0 or rgb[1] > 255: gte.registers[FLAG].uint32.setBit 20
  if rgb[2] < 0 or rgb[2] > 255: gte.registers[FLAG].uint32.setBit 19

proc MAC0(gte: GTE): int64 {.used.} = gte.registers[MAC0].int32.int64
proc `MAC0=`(gte: var GTE, val: int64) =
  gte.registers[MAC0].uint32 = cast[uint32](val)
  if val >= 2i64^31:   gte.registers[FLAG].uint32.setBit 16
  if val < -(2i64^31): gte.registers[FLAG].uint32.setBit 15

proc MAC(gte: GTE): Vec3l = gte.vec3(MAC1, MAC2, MAC3)
proc `MAC=`(gte: var GTE, mac: Vec3l) =
  gte.accMAC = mac.signExtendFrom(44)
  gte.registers[MAC1].uint32 = cast[uint32](mac[0])
  gte.registers[MAC2].uint32 = cast[uint32](mac[1])
  gte.registers[MAC3].uint32 = cast[uint32](mac[2])
  if mac[0] >= 2i64^43:   gte.registers[FLAG].uint32.setBit 30
  if mac[1] >= 2i64^43:   gte.registers[FLAG].uint32.setBit 29
  if mac[2] >= 2i64^43:   gte.registers[FLAG].uint32.setBit 28
  if mac[0] < -(2i64^43): gte.registers[FLAG].uint32.setBit 27
  if mac[1] < -(2i64^43): gte.registers[FLAG].uint32.setBit 26
  if mac[2] < -(2i64^43): gte.registers[FLAG].uint32.setBit 25

proc RTM(gte: GTE): Mat3x3l = gte.mat3x3(RT1, RT5)
proc TR(gte: GTE): Vec3l = gte.vec3(TRX, TRY, TRZ)
proc LLM(gte: GTE): Mat3x3l = gte.mat3x3(LL1, LL5)
proc BC(gte: GTE): Vec3l = gte.vec3(RBC, GBC, BBC)
proc LCM(gte: GTE): Mat3x3l = gte.mat3x3(LC1, LC5)
proc FC(gte: GTE): Vec3l = gte.vec3(RFC, GFC, BFC)
proc OFX(gte: GTE): int64 = gte.registers[OFX].int32.int64
proc OFY(gte: GTE): int64 = gte.registers[OFY].int32.int64
proc H(gte: GTE): int64 = gte.registers[H].uint16.int64
proc DQA(gte: GTE): int64 = gte.registers[DQA].int16.int64
proc DQB(gte: GTE): int64 = gte.registers[DQB].int32.int64
proc ZSF3(gte: GTE): int64 = gte.registers[ZSF3].int16.int64
proc ZSF4(gte: GTE): int64 = gte.registers[ZSF4].int16.int64

######################################################################
# Register access from the MIPS

func dataReg*(r: 0..31): Register =
  ## Convert a MFC2/MTC2 register number to a COP2 register number.
  Register(r)

func controlReg*(r: 0..31): Register =
  ## Convert a CFC2/CTC2 register number to a COP2 register number.
  Register(r.int+32)

# Bitfield for IRGB/ORGB
uint32.bitfield red5, uint32, 0, 5
uint32.bitfield green5, uint32, 5, 5
uint32.bitfield blue5, uint32, 10, 5

func `[]`*(gte: GTE, reg: Register): uint32 =
  ## Read a register, as with MFC2/CFC2.
  if reg == SXYP:
    # Mirror of SXY2
    result = gte.registers[SXY2].uint32
  elif reg in {IRGB, ORGB}:
    # Computed from IR1-IR3
    result.red5 =   (gte.IR1 div 0x80).clamp(0, 0x1f).uint32
    result.green5 = (gte.IR2 div 0x80).clamp(0, 0x1f).uint32
    result.blue5 =  (gte.IR3 div 0x80).clamp(0, 0x1f).uint32
  else:
    result = gte.registers[reg].uint32

  # Take care of registers that aren't the full 32 bits
  case reg
  of VZ0, VZ1, VZ2, IR0, IR1, IR2, IR3, RT5, LL5, LC5, H, DQA, ZSF3, ZSF4:
    # 16-bit sign-extended
    result = extendFrom(result.signed, 16).unsigned
  of OTZ, SZ0, SZ1, SZ2, SZ3:
    # 16-bit zero-extended
    result = result and 0xffff
  of FLAG:
    # Certain bits are fixed
    result = result and 0x7ffff000u32
    if (result and 0x7f87e000u32) != 0:
      result = result or 0x80000000u32
  else:
    discard

proc `[]=`*(gte: var GTE, reg: Register, value: uint32) =
  ## Write to a register, as with MTC2/CTC2.
  trace fmt"Setting register {reg}={reg.int} to {value:x}"

  # Take care of registers with special behaviour on write
  case reg
  of ORGB, LZCR:
    # Read-only registers
    return
  of SXYP:
    # Mirror of SXY2 plus FIFO
    gte.registers[SXY0] = gte.registers[SXY1]
    gte.registers[SXY1] = gte.registers[SXY2]
    gte.registers[SXY2].uint32 = value
  of IRGB:
    # Colour conversion
    gte.registers[IR1].uint32 = value.red5.uint32 * 0x80
    gte.registers[IR2].uint32 = value.green5.uint32 * 0x80
    gte.registers[IR3].uint32 = value.blue5.uint32 * 0x80
  of LZCS:
    # Count leading zeroes
    if value.signed == 0 or value.signed == -1:
      gte.registers[LZCR].uint32 = 32
    elif value.signed >= 0:
      gte.registers[LZCR].uint32 = countLeadingZeroBits(value).uint32
    else:
      gte.registers[LZCR].uint32 = countLeadingZeroBits(not value).uint32
  else:
    discard

  gte.registers[reg].uint32 = value

######################################################################
# Executing GTE opcodes.

proc `shl`(v: Vec3l, amount: int): Vec3l {.used.} =
  for i in 0..2:
    result[i] = v[i] shl amount

proc `shr`(v: Vec3l, amount: int): Vec3l =
  for i in 0..2:
    result[i] = v[i] shr amount

proc pushS(gte: var GTE) =
  ## Make room for a new entry in the S FIFO.
  gte.registers[SXY0] = gte.registers[SXY1]
  gte.registers[SXY1] = gte.registers[SXY2]
  gte.registers[SZ0] = gte.registers[SZ1]
  gte.registers[SZ1] = gte.registers[SZ2]
  gte.registers[SZ2] = gte.registers[SZ3]

proc pushRGB(gte: var GTE) =
  ## Make room for a new entry in the RGB FIFO.
  gte.registers[RGB0] = gte.registers[RGB1]
  gte.registers[RGB1] = gte.registers[RGB2]

template pushRGBfromMAC(gte: var GTE) =
  ## Transfer a computed colour from MAC to the RGB FIFO.
  gte.pushRGB
  gte.RGB2 = gte.MAC shr 4
  gte.IR = gte.MAC

proc perspectiveDivisor(gte: var GTE): int64 =
  ## Compute an approximation of ((H*0x20000/SZ3)+1)/2.
  ## Used in RTPS and RTPT.
  ## All information gratefully taken from Nocash.

  if gte.H >= gte.SZ3 * 2:
    # Out of bounds
    gte.registers[FLAG].uint32.setBit 17
    result = 0x1ffff
  elif gte.SZ3 == 1:
    # See "Some special cases" note in Nocash docs.
    if gte.H == 0: result = 0
    elif gte.H == 1: result = 0x10000
    # SZ3 == 1 and H > 1 => out of bounds
    else: raise newException(AssertionDefect, "unreachable")
  else:
    var z = gte.SZ3.uint16.countLeadingZeroBits
    assert 0 <= z and z <= 15
    let n = gte.H shl z
    assert 0 <= n and n <= 0x7fff8000
    var d = gte.SZ3 shl z
    assert 0x8000 <= d and d <= 0xffff
    let i = (d-0x7fc0) shr 7
    assert 0 <= i and i <= 0x100
    let u = max(0, (0x40000 div (i+0x100) + 1) div 2 - 0x101) + 0x101
    assert 0x101 <= u and u <= 0x200
    d = (0x2000080 - d*u) shr 8
    d = (0x80 + d*u) shr 8
    assert 0x20000 >= d and d >= 0x10000
    result = min(0x1ffff, ((n*d) + 0x8000) shr 16)

type
  Opcode {.pure.} = enum
    ## Opcode numbers.
    RTPS = 0x01, NCLIP = 0x06, OP = 0x0C, DPCS = 0x10, INTPL = 0x11,
    MVMVA = 0x12, NCDS = 0x13, CDP = 0x14, NCDT = 0x16, NCCS = 0x1B,
    CC = 0x1C, NCS = 0x1E, NCT = 0x20, SQR = 0x28, DCPL = 0x29,
    DPCT = 0x2A, AVSZ3 = 0x2D, AVSZ4 = 0x2E, RTPT = 0x30, GPF = 0x3D,
    GPL = 0x3E, NCCT = 0x3F

proc execute*(gte: var GTE, op: word) =
  ## Execute a GTE operation.
  word.bitfield opcode, int, 0, 6
  word.bitfield lm, bool, 10, 1
  word.bitfield sf, bool, 19, 1
  word.bitfield multiplyMatrix, int, 17, 2
  word.bitfield multiplyVector, int, 15, 2
  word.bitfield translationVector, int, 13, 2

  trace fmt"GTE op {op.opcode:x}"
  trace fmt"before: {gte}"
  gte.registers[FLAG].uint32 = 0
  gte.accMAC = gte.MAC

  let shift =
    if op.sf: 12
    else: 0

  template `IR=`(gte: var GTE, ir: Vec3l) =
    gte.setIR op.lm, ir

  template viaMAC0(val: int64): int64 =
    gte.MAC0 = val
    val

  template viaMAC(val: Vec3l, shift: int = shift): Vec3l =
    # Note: default shift value comes from op.sf

    # Overflow checks work on the preshifted value
    gte.MAC = val
    gte.MAC = gte.accMAC shr shift
    gte.MAC

  template matMul(mat: Mat3x3l, vec: Vec3l, shift: int = shift): Vec3l =
    # Matrix-vector multiplication, via MAC

    let col0 = mat[0] * vec[0]
    let col1 = mat[1] * vec[1]
    let col2 = mat[2] * vec[2]
    gte.MAC = col0 + col1
    viaMAC(gte.accMAC + col2, shift)

  template matMulPlus(mat: Mat3x3l, vec: Vec3l, plus: Vec3l, shift: int = shift): Vec3l =
    # Matrix-vector multiply-accumulate, via MAC

    let col0 = mat[0] * vec[0] + plus
    let col1 = mat[1] * vec[1]
    let col2 = mat[2] * vec[2]
    gte.MAC = col0
    gte.MAC = gte.accMAC + col1
    viaMAC(gte.accMAC + col2, shift)

  template interpolateColours(source: Vec3l, depth: bool) =
    gte.MAC = source
    let origMAC = gte.accMAC

    if depth:
      gte.setIR false, viaMAC(gte.FC shl 12 - origMAC)
      gte.IR = viaMAC(gte.IR * gte.IR0 + origMAC)
    else:
      gte.MAC = gte.accMAC shr shift
    gte.pushRGBfromMAC

  case op.opcode
  of RTPS.int, RTPT.int:
    template perspectiveTransform(v: Vec3l, last: bool) =
      gte.setIR op.lm, matMulPlus(gte.RTM, v, gte.TR shl 12),
        rtpsBug = not op.sf

      gte.pushS
      gte.SZ3 = gte.accMAC[2] shr (12 - shift)
      let divisor = gte.perspectiveDivisor
      gte.SX2 = viaMAC0(divisor*gte.IR1 + gte.OFX) shr 16
      gte.SY2 = viaMAC0(divisor*gte.IR2 + gte.OFY) shr 16
      if last: gte.IR0 = viaMAC0(divisor*gte.DQA + gte.DQB) shr 12

    if op.opcode == RTPT.int:
      perspectiveTransform gte.V0, false
      perspectiveTransform gte.V1, false
      perspectiveTransform gte.V2, true
    else:
      perspectiveTransform gte.V0, true

  of MVMVA.int:
    let m =
      case op.multiplyMatrix
      of 0: gte.RTM
      of 1: gte.LLM
      of 2: gte.LCM
      else:
        # m=3 is not allowed, but using it produces the following
        # "garbage matrix" as Nocash calls it
        block:
          let funny = gte.RGBC.red.int64 shl 4 # From Duckstation
          mat3x3[int64](
            vec3[int64](-funny,  gte.RTM[2,0], gte.RTM[1,1]),
            vec3[int64](funny,   gte.RTM[2,0], gte.RTM[1,1]),
            vec3[int64](gte.IR0, gte.RTM[2,0], gte.RTM[1,1])
          )
    let v =
      case op.multiplyVector
      of 0: gte.V0
      of 1: gte.V1
      of 2: gte.V2
      of 3: gte.IR
      else: raise newException(AssertionDefect, "unreachable")
    let t =
      case op.translationVector
      of 0: gte.TR
      of 1: gte.BC
      of 2: gte.FC
      of 3: Vec3l()
      else: raise newException(AssertionDefect, "unreachable")
    if op.translationVector == 2:
      # Buggy matrix multiplcation - from Duckstation
      gte.setIR false, viaMAC(t shl 12 + m[0] * v[0])
      gte.IR = viaMAC(m[1] * v[1] + m[2] * v[2])
    else:
      gte.IR = matMulPlus(m, v, t shl 12)

  of DCPL.int, DPCS.int, DPCT.int, INTPL.int:
    if op.opcode == DCPL.int:
      interpolateColours((gte.RGB * gte.IR) shl 4, true)
    elif op.opcode == INTPL.int:
      interpolateColours(gte.IR shl 12, true)
    elif op.opcode == DPCS.int:
      interpolateColours(gte.RGB shl 16, true)
    else: # DCPT
      for i in 0..2:
        interpolateColours(gte.RGB0 shl 16, true)

  of SQR.int:
    gte.IR = viaMAC((gte.IR * gte.IR))

  of NCS.int, NCT.int, NCCS.int, NCCT.int, NCDS.int, NCDT.int:
    type Mode = enum Plain, Colour, Depth
    template normalColour(v: Vec3l, mode: Mode) =
      gte.IR = matMul(gte.LLM, v)
      gte.IR = matMulPlus(gte.LCM, gte.IR, gte.BC shl 12)
      if mode >= Colour:
        interpolateColours((gte.RGB * gte.IR) shl 4, mode == Depth)
      else:
        gte.pushRGBfromMAC

    template normalColours(mode: Mode, triple: bool) =
      if triple:
        normalColour(gte.V0, mode)
        normalColour(gte.V1, mode)
        normalColour(gte.V2, mode)
      else:
        normalColour(gte.V0, mode)

    case op.opcode
    of NCS.int: normalColours(Plain, false)
    of NCT.int: normalColours(Plain, true)
    of NCCS.int: normalColours(Colour, false)
    of NCCT.int: normalColours(Colour, true)
    of NCDS.int: normalColours(Depth, false)
    of NCDT.int: normalColours(Depth, true)
    else: raise newException(AssertionDefect, "unreachable")

  of CC.int, CDP.int:
    gte.IR = matMulPlus(gte.LCM, gte.IR, gte.BC shl 12)
    interpolateColours((gte.RGB * gte.IR) shl 4, op.opcode == CDP.int)

  of NCLIP.int:
    gte.MAC0 =
      (gte.S0[0]*gte.S1[1] + gte.S1[0]*gte.S2[1] + gte.S2[0]*gte.S0[1] -
       gte.S0[0]*gte.S2[1] - gte.S1[0]*gte.S0[1] - gte.S2[0]*gte.S1[1])

  of AVSZ3.int:
    gte.OTZ = viaMAC0(gte.ZSF3*(gte.SZ1+gte.SZ2+gte.SZ3)) shr 12

  of AVSZ4.int:
    gte.OTZ = viaMAC0(gte.ZSF4*(gte.SZ0+gte.SZ1+gte.SZ2+gte.SZ3)) shr 12

  of OP.int:
    gte.IR =
      viaMAC(
      vec3(gte.IR3*gte.RTM[1,1] - gte.IR2*gte.RTM[2,2],
           gte.IR1*gte.RTM[2,2] - gte.IR3*gte.RTM[0,0],
           gte.IR2*gte.RTM[0,0] - gte.IR1*gte.RTM[1,1]))

  of GPF.int, GPL.int:
    if op.opcode == GPF.int:
      gte.MAC = Vec3l()
    else: # GPL
      gte.MAC = gte.MAC shl shift
    discard viaMAC(gte.IR * gte.IR0 + gte.accMAC)
    gte.pushRGBfromMAC
  else:
    warn fmt"Unrecognised GTE op {op.opcode:x}"

  trace fmt"after: {gte}"
