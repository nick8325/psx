## Geometry transformation engine.

import basics, utils
import std/[strformat, bitops, math]
import glm

const loggerComponent = logGTE
logGTE.level = lvlDebug

type
  Register* = enum
    # Data registers
    VXY0, VZ0, VXY1, VZ1, VXY2, VZ2,
    RGBC, OTZ, IR0, IR1, IR2, IR3,
    SXY0, SXY1, SXY2, SXYP,
    SZ0, SZ1, SZ2, SZ3,
    RGB0, RGB1, RGB2,
    RES1,
    MAC0, MAC1, MAC2, MAC3,
    IRGB, ORGB, LZCS, LZCR,
    # Control registers
    RT1, RT2, RT3, RT4, RT5,
    TRX, TRY, TRZ,
    L1, L2, L3, L4, L5,
    RBC, GBC, BBC,
    LC1, LC2, LC3, LC4, LC5,
    RFC, GFC, BFC,
    OFX, OFY,
    H,
    DQA, DQB,
    ZSF3, ZSF4,
    FLAG

static:
  assert Register.high.int == 63

type
  GTE* = object
    # Note: we ignore the values of SXYP and ORGB - these are mirrors
    # of SXY2 and IRGB, respectively
    registers: array[Register, RegisterVal]
    sf: bool
    lm: bool
  RegisterVal {.union.} = object
    uint32: uint32
    int32: int32
    uint16s: tuple[low, high: uint16]
    int16s: tuple[low, high: int16]
    uint16: uint16
    int16: int16
    rgbc: tuple[red, green, blue, code: uint8]

static:
  assert RegisterVal.sizeof == 4

func dataReg*(r: 0..31): Register =
  Register(r)

func controlReg*(r: 0..31): Register =
  Register(r.int+32)

func `[]`*(gte: GTE, reg: Register): uint32 =
  if reg == SXYP: # mirror of SXY2
    result = gte.registers[SXY2].uint32
  elif reg == ORGB: # mirror of IRGB
    result = gte.registers[IRGB].uint32
  else:
    result = gte.registers[reg].uint32

  # Take care of registers that aren't the full 32 bits
  case reg
  of VZ0, VZ1, VZ2, IR0, IR1, IR2, IR3, RT5, L5, LC5, H, DQA, ZSF3, ZSF4:
    # 16-bit sign-extended
    result = signExtendFrom(result.signed, 16).unsigned
  of SZ0, SZ1, SZ2, SZ3, OTZ:
    # 16-bit zero-extended
    result = result and 0xffff
  of IRGB, ORGB:
    # 15-bit zero-extended
    result = result and 0x7fff
  of FLAG:
    # Certain bits are fixed
    result = result and 0x7ffff000u32
    if (result and 0x7f87e000u32) != 0:
      result = result or 0x80000000u32
  else:
    discard

proc `[]=`*(gte: var GTE, reg: Register, valueIn: uint32) =
  trace fmt"Setting register {reg}={reg.int} to {valueIn:x}"
  var value = valueIn

  # Used for IRGB/ORGB
  uint32.bitfield red5, uint32, 0, 5
  uint32.bitfield green5, uint32, 5, 5
  uint32.bitfield blue5, uint32, 10, 5

  # Ignore writes to read-only registers
  if reg in {ORGB, LZCR}:
    return

  gte.registers[reg].uint32 = value

  # Take care of registers with special behaviour on write
  case reg
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
    gte.registers[ORGB].uint32 = value
  of IR1, IR2, IR3:
    # Colour conversion
    var conv: uint32
    conv.red5 =   (gte[IR1] div 0x80).clamp(0, 0x1f)
    conv.green5 = (gte[IR2] div 0x80).clamp(0, 0x1f)
    conv.blue5 =  (gte[IR3] div 0x80).clamp(0, 0x1f)
    gte.registers[IRGB].uint32 = conv
    gte.registers[ORGB].uint32 = conv
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

proc clamp(gte: var GTE, val, lo, hi: float, bit: int): float =
  if val < lo or val > hi:
    gte.registers[FLAG].uint32.setBit bit
  val.clamp(lo, hi)
proc clampedSet(gte: var GTE, reg: Register, val, lo, hi: float, bit: int) =
  let clampedVal = gte.clamp(val, lo, hi, bit)
  gte[reg] = clampedVal.int32.unsigned
proc truncate(val: float): uint32 =
  let val64 = val.clamp(int64.low.float, int64.high.float).int64
  uint32(val64.euclMod(1i64 shl 32))

static:
  assert truncate(-1) == 0xffffffffu32

proc vec3(gte: GTE, xy: Register, z: Register): Vec3d =
  vec3(gte.registers[xy].int16s[0].float,
       gte.registers[xy].int16s[1].float,
       gte.registers[z].uint16.float)

proc vec3(gte: GTE, x: Register, y: Register, z: Register): Vec3d =
  vec3(gte.registers[x].int32.float,
       gte.registers[y].int32.float,
       gte.registers[z].int32.float)

proc mat3x3(gte: GTE, r1, r5: Register): Mat3x3d =
  let r2 = r1.succ
  let r3 = r2.succ
  let r4 = r3.succ
  assert r5 == r4.succ
  result[0,0] = gte.registers[r1].int16s[0].float
  result[0,1] = gte.registers[r1].int16s[1].float
  result[0,2] = gte.registers[r2].int16s[0].float
  result[1,0] = gte.registers[r2].int16s[1].float
  result[1,1] = gte.registers[r3].int16s[0].float
  result[1,2] = gte.registers[r3].int16s[1].float
  result[2,0] = gte.registers[r4].int16s[0].float
  result[2,1] = gte.registers[r4].int16s[1].float
  result[2,2] = gte.registers[r5].int16.float

proc rgb(gte: GTE, reg: Register): Vec3d =
  vec3(gte.registers[reg].rgbc.red.float,
       gte.registers[reg].rgbc.green.float,
       gte.registers[reg].rgbc.blue.float)

proc V0(gte: GTE): Vec3d = gte.vec3(VXY0, VZ0)
proc V1(gte: GTE): Vec3d = gte.vec3(VXY1, VZ1)
proc V2(gte: GTE): Vec3d = gte.vec3(VXY2, VZ2)

proc RGBC(gte: GTE): tuple[red, green, blue, code: uint8] =
  gte.registers[RGBC].rgbc
proc RGB(gte: GTE): Vec3d = gte.rgb(RGBC)

proc `OTZ=`(gte: var GTE, otz: float) =
  gte.clampedSet OTZ, otz, 0, 0xffff, 18

proc IR0(gte: GTE): float =
  gte.registers[IR0].int16.float
proc `IR0=`(gte: var GTE, ir: float) =
  gte.clampedSet IR0, ir, 0, 0x1000, 12

proc IR(gte: GTE): Vec3d =
  vec3(gte.registers[IR1].int16.float,
       gte.registers[IR2].int16.float,
       gte.registers[IR3].int16.float)
proc `IR=`(gte: var GTE, ir: Vec3d) =
  let lo: float = if gte.lm: 0 else: -0x8000
  gte.clampedSet IR1, ir[0], lo, 0x7fff, 24
  gte.clampedSet IR2, ir[1], lo, 0x7fff, 23
  gte.clampedSet IR3, ir[2], lo, 0x7fff, 22

proc S0(gte: GTE): Vec3d = gte.vec3(SXY0, SZ0)
proc S1(gte: GTE): Vec3d = gte.vec3(SXY1, SZ1)
proc S2(gte: GTE): Vec3d = gte.vec3(SXY2, SZ2)
proc SZ0(gte: GTE): float = gte.registers[SZ0].uint16.float
proc SZ1(gte: GTE): float = gte.registers[SZ1].uint16.float
proc SZ2(gte: GTE): float = gte.registers[SZ2].uint16.float
proc SZ3(gte: GTE): float = gte.registers[SZ3].uint16.float

proc `SX2=`(gte: var GTE, val: float) =
  let x = gte.clamp(val, -0x400, 0x3ff, 14)
  gte.registers[SXY2].int16s.low = x.int16

proc `SY2=`(gte: var GTE, val: float) =
  let y = gte.clamp(val, -0x400, 0x3ff, 13)
  gte.registers[SXY2].int16s.high = y.int16

proc `SZ3=`(gte: var GTE, val: float) =
  let z = gte.clamp(val, 0, 0xffff, 18)
  gte.registers[SZ3].uint16 = z.uint16

proc pushS(gte: var GTE) =
  gte.registers[SXY0] = gte.registers[SXY1]
  gte.registers[SXY1] = gte.registers[SXY2]
  gte.registers[SZ0] = gte.registers[SZ1]
  gte.registers[SZ1] = gte.registers[SZ2]
  gte.registers[SZ2] = gte.registers[SZ3]

proc RGB0(gte: GTE): Vec3d = gte.rgb(RGB0)
proc RGB1(gte: GTE): Vec3d {.used.} = gte.rgb(RGB1)
proc RGB2(gte: GTE): Vec3d = gte.rgb(RGB2)
proc `RGB2=`(gte: var GTE, rgb: Vec3d) =
  gte.registers[RGB2].rgbc =
    (red: rgb[0].clamp(0, 255).uint8,
     green: rgb[1].clamp(0, 255).uint8,
     blue: rgb[2].clamp(0, 255).uint8,
     code: gte.RGBC.code)
  if rgb[0] < 0 or rgb[0] > 255: gte.registers[FLAG].uint32.setBit 21
  if rgb[1] < 0 or rgb[1] > 255: gte.registers[FLAG].uint32.setBit 20
  if rgb[2] < 0 or rgb[2] > 255: gte.registers[FLAG].uint32.setBit 19

proc pushRGB(gte: var GTE) =
  gte.registers[RGB0] = gte.registers[RGB1]
  gte.registers[RGB1] = gte.registers[RGB2]

proc MAC0(gte: GTE): float = gte.registers[MAC0].int32.float
proc `MAC0=`(gte: var GTE, val: float) =
  gte[MAC0] = val.truncate
  if val >= 2f^31: gte.registers[FLAG].uint32.setBit 16
  if val < -2f^31: gte.registers[FLAG].uint32.setBit 15

proc MAC(gte: GTE): Vec3d = gte.vec3(MAC1, MAC2, MAC3)
proc `MAC=`(gte: var GTE, mac: Vec3d) =
  gte[MAC1] = mac[0].truncate
  gte[MAC2] = mac[1].truncate
  gte[MAC3] = mac[2].truncate
  if mac[0] >= 2f^43: gte.registers[FLAG].uint32.setBit 30
  if mac[1] >= 2f^43: gte.registers[FLAG].uint32.setBit 29
  if mac[2] >= 2f^43: gte.registers[FLAG].uint32.setBit 28
  if mac[0] < -2f^43: gte.registers[FLAG].uint32.setBit 27
  if mac[1] < -2f^43: gte.registers[FLAG].uint32.setBit 26
  if mac[2] < -2f^43: gte.registers[FLAG].uint32.setBit 25

proc RTM(gte: GTE): Mat3x3d = gte.mat3x3(RT1, RT5)
proc TR(gte: GTE): Vec3d = gte.vec3(TRX, TRY, TRZ)
proc LLM(gte: GTE): Mat3x3d = gte.mat3x3(L1, L5)
proc BC(gte: GTE): Vec3d = gte.vec3(RBC, GBC, BBC)
proc LCM(gte: GTE): Mat3x3d = gte.mat3x3(LC1, LC5)
proc FC(gte: GTE): Vec3d = gte.vec3(RFC, GFC, BFC)
proc OFX(gte: GTE): float = gte.registers[OFX].int32.float
proc OFY(gte: GTE): float = gte.registers[OFY].int32.float
proc H(gte: GTE): float = gte.registers[H].uint16.float
proc DQA(gte: GTE): float = gte.registers[DQA].int16.float
proc DQB(gte: GTE): float = gte.registers[DQB].int32.float
proc ZSF3(gte: GTE): float = gte.registers[ZSF3].int16.float
proc ZSF4(gte: GTE): float = gte.registers[ZSF4].int16.float

proc `IRandMAC=`(gte: var GTE, val: Vec3d) =
    gte.IR = val
    gte.MAC = val

proc `***`(v1, v2: Vec3d): Vec3d =
  for i in 0..2:
    result[i] = v1[i] * v2[i]

type
  Opcode {.pure.} = enum
    RTPS = 0x01, NCLIP = 0x06, OP = 0x0C, DPCS = 0x10, INTPL = 0x11,
    MVMVA = 0x12, NCDS = 0x13, CDP = 0x14, NCDT = 0x16, NCCS = 0x1B,
    CC = 0x1C, NCS = 0x1E, NCT = 0x20, SQR = 0x28, DCPL = 0x29,
    DPCT = 0x2A, AVSZ3 = 0x2D, AVSZ4 = 0x2E, RTPT = 0x30, GPF = 0x3D,
    GPL = 0x3E, NCCT = 0x3F

proc execute*(gte: var GTE, op: word) =
  word.bitfield opcode, int, 0, 6
  word.bitfield lm, bool, 10, 1
  word.bitfield sf, bool, 19, 1
  word.bitfield multiplyMatrix, int, 17, 2
  word.bitfield multiplyVector, int, 15, 2
  word.bitfield translationVector, int, 13, 2

  trace fmt"GTE op {op.opcode:x}"
  gte[FLAG] = 0
  gte.lm = op.lm
  gte.sf = op.sf

  template sf(v: Vec3d): Vec3d =
    if op.sf: v / (2f^12)
    else: v

  template invsf[T](v: T): T =
    if op.sf: v
    else: v / (2f^12)

  case op.opcode
  of RTPS.int, RTPT.int:
    gte.lm = false
    template rtp(v: untyped) =
      gte.IRandMac = (gte.TR * 0x1000 + gte.RTM*gte.v).sf
      gte.pushS
      gte.SZ3 = gte.MAC[2].invsf
      var divisor = (gte.H * 0x20000 / gte.SZ3 + 1) / 2
      divisor = gte.clamp(divisor, 0, 0x1ffff, 17)
      gte.MAC0 = divisor*gte.IR[0] + gte.OFX
      gte.SX2 = gte.MAC0/0x10000
      gte.MAC0 = divisor*gte.IR[1] + gte.OFY
      gte.SY2 = gte.MAC0/0x10000
      gte.MAC0 = divisor*gte.DQA + gte.DQB
      gte.IR0 = gte.MAC0/0x1000
    rtp V0
    if op.opcode == RTPT.int:
      rtp V1
      rtp V2
  of NCLIP.int:
    gte.MAC0 =
      (gte.S0[0]*gte.S1[1] + gte.S1[0]*gte.S2[1] + gte.S2[0]*gte.S0[1] -
       gte.S0[0]*gte.S2[1] - gte.S1[0]*gte.S0[1] - gte.S2[0]*gte.S1[1])
  of AVSZ3.int:
    gte.MAC0 = gte.ZSF3*(gte.SZ1+gte.SZ2+gte.SZ3)
    gte.OTZ = gte.MAC0 / 0x1000
  of AVSZ4.int:
    gte.MAC0 = gte.ZSF4*(gte.SZ0+gte.SZ1+gte.SZ2+gte.SZ3)
    gte.OTZ = gte.MAC0 / 0x1000
  of MVMVA.int:
    let m =
      case op.multiplyMatrix
      of 0: gte.RTM
      of 1: gte.LLM
      of 2: gte.LCM
      else: Mat3x3d() # use whatever
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
      of 3: Vec3d()
      else: raise newException(AssertionDefect, "unreachable")
    gte.MAC = (t * 0x1000 + m * v).sf
    gte.IR = gte.MAC
  of SQR.int:
    gte.MAC = (gte.IR *** gte.IR).sf
    gte.IR = gte.MAC
  of OP.int:
    gte.MAC =
      vec3(gte.IR[2]*gte.RTM[1,1] - gte.IR[1]*gte.RTM[2,2],
           gte.IR[0]*gte.RTM[2,2] - gte.IR[2]*gte.RTM[0,0],
           gte.IR[1]*gte.RTM[0,0] - gte.IR[0]*gte.RTM[1,1]).sf
    gte.IR = gte.MAC
  of NCS.int, NCT.int, NCCS.int, NCCT.int, NCDS.int, NCDT.int:
    let triple = op.opcode in [NCT.int, NCCT.int, NCDT.int]
    let cc = op.opcode in [NCCS.int, NCCT.int]
    let depth = op.opcode in [NCDS.int, NCDT.int]

    template nc(v: untyped) =
      gte.IRandMAC = (gte.LLM * gte.V0).sf
      gte.IRandMAC = (gte.BC * 0x1000 + gte.LCM * gte.IR).sf
      if cc or depth:
        gte.MAC = gte.RGB *** gte.IR * 16
      if depth:
        gte.MAC = gte.MAC + (gte.FC-gte.MAC)*gte.IR0
      if cc or depth:
        gte.MAC = gte.MAC.sf
      gte.pushRGB
      gte.RGB2 = gte.MAC / 16
      gte.IR = gte.MAC

    nc V0
    if triple:
      nc V1
      nc V2
  of CC.int, CDP.int:
    gte.IRandMAC = (gte.BC * 0x1000 + gte.LCM * gte.IR).sf
    gte.MAC = (gte.RGB *** gte.IR) * 16
    if op.opcode == CDP.int:
      gte.MAC = gte.MAC + (gte.FC - gte.MAC)*gte.IR0
    gte.MAC = gte.MAC.sf
    gte.pushRGB
    gte.RGB2 = gte.MAC / 16
    gte.IR = gte.MAC
  of DCPL.int, DPCS.int, DPCT.int, INTPL.int:
    template dc =
      if op.opcode == DCPL.int:
        gte.MAC = (gte.RGB *** gte.IR) * 16
      elif op.opcode == INTPL.int:
        gte.MAC = gte.IR * (2f^12)
      elif op.opcode == DPCS.int:
        gte.MAC = gte.RGB * (2f^16)
      else: # DCPT
        gte.MAC = gte.RGB0 * (2f^16)
      gte.MAC = gte.MAC + (gte.FC - gte.MAC)*gte.IR0
      gte.MAC = gte.MAC.sf
      gte.pushRGB
      gte.RGB2 = gte.MAC / 16
      gte.IR = gte.MAC
    dc
    if op.opcode == DPCT.int:
      dc
      dc
  of GPF.int, GPL.int:
    if op.opcode == GPF.int:
      gte.MAC = Vec3d()
    else: # GPL
      gte.MAC = gte.MAC.sf
    gte.MAC = (gte.IR * gte.IR0 + gte.MAC).sf
    gte.pushRGB
    gte.RGB2 = gte.MAC / 16
    gte.IR = gte.MAC
  else:
    warn fmt"Unrecognised GTE op {op.opcode:x}"

