## The sound processing unit (SPU).
import basics, utils, irq, eventqueue, savestates
import std/[strformat, deques, sugar, bitops]

const loggerComponent = logSPU
logSPU.level = lvlDebug

proc mix(x, y: int16): int16 =
  ((x.int * y.int) div 0x8000).int16

######################################################################
## SPU RAM. Reading writing, triggering IRQs.

type
  SPURam = object
    raw: array[0x80000, uint8]
    watchAddress: uint32
    watchTriggered: bool

var
  spuram {.saved.}: SPURam

proc checkWatch(spuram: var SPURam, address: uint32) =
  if (address and not 0x7u32) == (spuram.watchAddress and not 0x7u32):
    spuram.watchTriggered = true

proc read8(spuram: var SPURam, address: uint32): uint8 =
  let address = address mod spuram.raw.len.word
  spuram.checkWatch address
  spuram.raw[address]

proc write8(spuram: var SPURam, address: uint32, value: uint8) =
  let address = address mod spuram.raw.len.word
  spuram.checkWatch address
  spuram.raw[address] = value

proc read16(spuram: var SPURam, address: uint32): uint16 =
  let x = spuram.read8(address).uint16
  let y = spuram.read8(address+1).uint16
  x + y shl 8

proc write16(spuram: var SPURam, address: uint32, val: uint16) =
  spuram.write8 address, (val and 0xff).uint8
  spuram.write8 address+1, (val shr 8).uint8

proc read32(spuram: var SPURam, address: uint32): uint32 =
  let x = spuram.read16(address).uint32
  let y = spuram.read16(address+2).uint32
  x + y shl 16

proc write32(spuram: var SPURam, address: uint32, val: uint32) =
  spuram.write16 address, (val and 0xffff).uint16
  spuram.write16 address+2, (val shr 16).uint16

proc `[]`(spuram: var SPURam, address: uint32): uint16 =
  spuram.read16(address)

proc `[]=`(spuram: var SPURam, address: uint32, val: uint16) =
  spuram.write16(address, val)

######################################################################
## ADSR envelope (per-voice).

type
  Adsr = object
    flags: AdsrFlags
    volume: int16
    phase: AdsrPhase
    waitFor: int
    stepAfter: int

  AdsrFlags = distinct uint32
  Mode = enum mLinear, mExponential
  Direction = enum dIncrease, dDecrease

  AdsrPhase = enum apAttack, apDecay, apSustain, apRelease
  RampSettings = object
    mode: Mode
    direction: Direction
    target: int16
    shift: int
    rawStep: int

proc step(settings: RampSettings): int =
  case settings.direction
  of dIncrease: 7 - settings.rawStep
  of dDecrease: -8 - settings.rawStep

AdsrFlags.bitfield sustainMode, Mode, 31, 1
AdsrFlags.bitfield sustainDirection, Direction, 30, 1
AdsrFlags.bitfield sustainShift, int, 24, 5
AdsrFlags.bitfield sustainStep, int, 22, 2
AdsrFlags.bitfield releaseMode, Mode, 21, 1
AdsrFlags.bitfield releaseShift, int, 16, 5
AdsrFlags.bitfield attackMode, Mode, 15, 1
AdsrFlags.bitfield attackShift, int, 10, 5
AdsrFlags.bitfield attackStep, int, 8, 2
AdsrFlags.bitfield decayShift, int, 4, 4
AdsrFlags.bitfield sustainLevel, int, 0, 4
AdsrFlags.bitfield lower, uint16, 0, 16
AdsrFlags.bitfield upper, uint16, 16, 16

proc settings(adsr: Adsr): RampSettings =
  case adsr.phase
  of apAttack:
    RampSettings(
      mode: adsr.flags.attackMode,
      direction: dIncrease,
      target: 0x7fff,
      shift: adsr.flags.attackShift,
      rawStep: adsr.flags.attackStep)
  of apDecay:
    RampSettings(
      mode: mExponential,
      direction: dDecrease,
      target: ((adsr.flags.sustainLevel+1)*0x800).clamp(0, 0x7fff).int16,
      shift: adsr.flags.decayShift,
      rawStep: 0)
  of apSustain:
    RampSettings(
      mode: adsr.flags.sustainMode,
      direction: adsr.flags.sustainDirection,
      target: 0, # not used
      shift: adsr.flags.sustainShift,
      rawStep: adsr.flags.sustainStep)
  of apRelease:
    RampSettings(
      mode: adsr.flags.releaseMode,
      direction: dDecrease,
      target: 0,
      shift: adsr.flags.releaseShift,
      rawStep: 0)

proc rampStep(volume: int16, settings: RampSettings): tuple[waitFor: int, stepAfter: int] =
  var waitFor = 1 shl max(0, settings.shift - 11)
  var stepAfter = settings.step shl max(0, 11 - settings.shift)
  if settings.mode == mExponential:
    case settings.direction
    of dIncrease:
      if volume > 0x6000: waitFor *= 4
    of dDecrease:
      stepAfter = (stepAfter * volume.int) div 0x8000
  return (waitFor: waitFor, stepAfter: stepAfter)

proc keyOn(adsr: var Adsr) =
  adsr.phase = apAttack
  adsr.volume = 0
  adsr.waitFor = 0
  adsr.stepAfter = 0

proc keyOff(adsr: var Adsr) =
  adsr.phase = apRelease
  adsr.waitFor = 0
  adsr.stepAfter = 0

proc mute(adsr: var Adsr) =
  adsr.keyOff()
  adsr.volume = 0

proc cycle(adsr: var Adsr) =
  if adsr.waitFor > 0: adsr.waitFor.dec

  if adsr.waitFor == 0:
    adsr.volume = (adsr.volume.int + adsr.stepAfter).clamp(0, 0x7fff).int16

    var newPhase = false
    if adsr.phase in {apAttack, apDecay}:
       if (adsr.settings.direction == dIncrease and adsr.volume >= adsr.settings.target) or
          (adsr.settings.direction == dDecrease and adsr.volume <= adsr.settings.target):
         newPhase = true
         adsr.phase.inc

    let ramp = rampStep(adsr.volume, adsr.settings)
    adsr.waitFor = ramp.waitFor
    adsr.stepAfter = ramp.stepAfter

    if newPhase:
      debug fmt"new phase {adsr.phase}"
      debug fmt"volume={adsr.volume}, settings={adsr.settings}, step={adsr.settings.step}, waitFor={adsr.waitFor}, stepAfter={adsr.stepAfter}"

######################################################################
## Sweep envelope (per-voice).

type
  Sweep = object
    flags: SweepFlags
    volume: int16
    phase: AdsrPhase
    waitFor: int
    stepAfter: int
  SweepFlags = distinct uint16
  SweepPhase = enum spPositive, spNegative

SweepFlags.bitfield sweep, bool, 15, 1
SweepFlags.bitfield volumeDiv2, int16, 0, 15
SweepFlags.bitfield sweepMode, Mode, 14, 1
SweepFlags.bitfield sweepDirection, Direction, 13, 1
SweepFlags.bitfield sweepPhase, SweepPhase, 12, 1
SweepFlags.bitfield sweepShift, int, 2, 5
SweepFlags.bitfield sweepStep, int, 0, 2

proc settings(sweep: Sweep): RampSettings =
  RampSettings(
    mode: sweep.flags.sweepMode,
    direction: sweep.flags.sweepDirection,
    target:
      case sweep.flags.sweepDirection
      of dIncrease: 0x7fff
      of dDecrease: 0,
    shift: sweep.flags.sweepShift,
    rawStep: sweep.flags.sweepStep)

proc cycle(sweep: var Sweep) =
  if not sweep.flags.sweep:
    sweep.volume = sweep.flags.volumeDiv2 * 2
    return
  if sweep.waitFor > 0: sweep.waitFor.dec
  if sweep.waitFor == 0:
    sweep.volume = (sweep.volume.int + sweep.stepAfter).clamp(0, 0x7fff).int16
    let ramp = rampStep(sweep.volume, sweep.settings)
    sweep.waitFor = ramp.waitFor
    sweep.stepAfter = ramp.stepAfter

######################################################################
## Sample generator (per-voice).

type
  SampleGenerator = object
    startAddress: uint16
    currentAddress: uint32
    repeatAddress: uint32
    sampleRate: uint16
    reachedLoopEnd: bool
    counter: int
    previous: int16
    value: int16

proc sampleNumber(generator: SampleGenerator): int =
  generator.counter shr 12

proc interpolationIndex(generator: SampleGenerator): int =
  (generator.counter shr 4) and 0xff

proc keyOn(generator: var SampleGenerator) =
  generator.currentAddress = generator.startAddress
  generator.reachedLoopEnd = false
  generator.counter = 0
  generator.value = 0
  generator.previous = 0

proc cycle(generator: var SampleGenerator, shouldMute: var bool) =
  shouldMute = false

  var step = generator.sampleRate
  # TODO: pitch modulation
  step = step.clamp(0, 0x3fff)
  generator.counter += step.int

  let flags = spuram.read8(generator.currentAddress + 1)
  if flags.testBit 2:
    generator.repeatAddress = generator.currentAddress

  while generator.sampleNumber >= 28:
    generator.counter -= 28 shl 12
    if flags.testBit 0:
      generator.reachedLoopEnd = true
      generator.currentAddress = generator.repeatAddress
      if not flags.testBit 1:
        shouldMute = true
    else:
      generator.currentAddress += 16

  var shift = spuram.read8(generator.currentAddress) and 0xf
  if shift > 12: shift = 9
  let bytePos = generator.sampleNumber div 2
  let nybblePos = generator.sampleNumber mod 2
  assert 0 <= bytePos and bytePos < 14 and 0 <= nybblePos and nybblePos < 2
  let byte = spuram.read8(generator.currentAddress + bytePos.uint16 + 2)
  let nybble =
    if nybblePos == 0: byte and 0xf
    else: byte shr 4

  var sample = (nybble.int16 shl 12 shr shift).int

  let filter = ((spuram.read8(generator.currentAddress) and 0x30) shr 4).clamp(0, 4)
  const filterTablePos = [0, 60, 115, 98, 122]
  const filterTableNeg = [0, 0, -52, -55, 60]
  let filterPos = filterTablePos[filter]
  let filterNeg = filterTableNeg[filter]
  sample += (generator.value.int * filterPos) shr 6
  sample += (generator.previous.int * filterNeg) shr 6

  generator.previous = generator.value
  generator.value = clampedConvert[int16](sample)

proc sample(generator: SampleGenerator): int16 =
  generator.value

######################################################################
## Gaussian interpolation.

proc interpolate(index: int, samples: array[4, int16]): int16 =
  const table = [
    0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,
    0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0001,0x0001,0x0001,0x0001,0x0002,0x0002,0x0002,0x0003,0x0003,
    0x0003,0x0004,0x0004,0x0005,0x0005,0x0006,0x0007,0x0007,0x0008,0x0009,0x0009,0x000A,0x000B,0x000C,0x000D,0x000E,
    0x000F,0x0010,0x0011,0x0012,0x0013,0x0015,0x0016,0x0018,0x0019,0x001B,0x001C,0x001E,0x0020,0x0021,0x0023,0x0025,
    0x0027,0x0029,0x002C,0x002E,0x0030,0x0033,0x0035,0x0038,0x003A,0x003D,0x0040,0x0043,0x0046,0x0049,0x004D,0x0050,
    0x0054,0x0057,0x005B,0x005F,0x0063,0x0067,0x006B,0x006F,0x0074,0x0078,0x007D,0x0082,0x0087,0x008C,0x0091,0x0096,
    0x009C,0x00A1,0x00A7,0x00AD,0x00B3,0x00BA,0x00C0,0x00C7,0x00CD,0x00D4,0x00DB,0x00E3,0x00EA,0x00F2,0x00FA,0x0101,
    0x010A,0x0112,0x011B,0x0123,0x012C,0x0135,0x013F,0x0148,0x0152,0x015C,0x0166,0x0171,0x017B,0x0186,0x0191,0x019C,
    0x01A8,0x01B4,0x01C0,0x01CC,0x01D9,0x01E5,0x01F2,0x0200,0x020D,0x021B,0x0229,0x0237,0x0246,0x0255,0x0264,0x0273,
    0x0283,0x0293,0x02A3,0x02B4,0x02C4,0x02D6,0x02E7,0x02F9,0x030B,0x031D,0x0330,0x0343,0x0356,0x036A,0x037E,0x0392,
    0x03A7,0x03BC,0x03D1,0x03E7,0x03FC,0x0413,0x042A,0x0441,0x0458,0x0470,0x0488,0x04A0,0x04B9,0x04D2,0x04EC,0x0506,
    0x0520,0x053B,0x0556,0x0572,0x058E,0x05AA,0x05C7,0x05E4,0x0601,0x061F,0x063E,0x065C,0x067C,0x069B,0x06BB,0x06DC,
    0x06FD,0x071E,0x0740,0x0762,0x0784,0x07A7,0x07CB,0x07EF,0x0813,0x0838,0x085D,0x0883,0x08A9,0x08D0,0x08F7,0x091E,
    0x0946,0x096F,0x0998,0x09C1,0x09EB,0x0A16,0x0A40,0x0A6C,0x0A98,0x0AC4,0x0AF1,0x0B1E,0x0B4C,0x0B7A,0x0BA9,0x0BD8,
    0x0C07,0x0C38,0x0C68,0x0C99,0x0CCB,0x0CFD,0x0D30,0x0D63,0x0D97,0x0DCB,0x0E00,0x0E35,0x0E6B,0x0EA1,0x0ED7,0x0F0F,
    0x0F46,0x0F7F,0x0FB7,0x0FF1,0x102A,0x1065,0x109F,0x10DB,0x1116,0x1153,0x118F,0x11CD,0x120B,0x1249,0x1288,0x12C7,
    0x1307,0x1347,0x1388,0x13C9,0x140B,0x144D,0x1490,0x14D4,0x1517,0x155C,0x15A0,0x15E6,0x162C,0x1672,0x16B9,0x1700,
    0x1747,0x1790,0x17D8,0x1821,0x186B,0x18B5,0x1900,0x194B,0x1996,0x19E2,0x1A2E,0x1A7B,0x1AC8,0x1B16,0x1B64,0x1BB3,
    0x1C02,0x1C51,0x1CA1,0x1CF1,0x1D42,0x1D93,0x1DE5,0x1E37,0x1E89,0x1EDC,0x1F2F,0x1F82,0x1FD6,0x202A,0x207F,0x20D4,
    0x2129,0x217F,0x21D5,0x222C,0x2282,0x22DA,0x2331,0x2389,0x23E1,0x2439,0x2492,0x24EB,0x2545,0x259E,0x25F8,0x2653,
    0x26AD,0x2708,0x2763,0x27BE,0x281A,0x2876,0x28D2,0x292E,0x298B,0x29E7,0x2A44,0x2AA1,0x2AFF,0x2B5C,0x2BBA,0x2C18,
    0x2C76,0x2CD4,0x2D33,0x2D91,0x2DF0,0x2E4F,0x2EAE,0x2F0D,0x2F6C,0x2FCC,0x302B,0x308B,0x30EA,0x314A,0x31AA,0x3209,
    0x3269,0x32C9,0x3329,0x3389,0x33E9,0x3449,0x34A9,0x3509,0x3569,0x35C9,0x3629,0x3689,0x36E8,0x3748,0x37A8,0x3807,
    0x3867,0x38C6,0x3926,0x3985,0x39E4,0x3A43,0x3AA2,0x3B00,0x3B5F,0x3BBD,0x3C1B,0x3C79,0x3CD7,0x3D35,0x3D92,0x3DEF,
    0x3E4C,0x3EA9,0x3F05,0x3F62,0x3FBD,0x4019,0x4074,0x40D0,0x412A,0x4185,0x41DF,0x4239,0x4292,0x42EB,0x4344,0x439C,
    0x43F4,0x444C,0x44A3,0x44FA,0x4550,0x45A6,0x45FC,0x4651,0x46A6,0x46FA,0x474E,0x47A1,0x47F4,0x4846,0x4898,0x48E9,
    0x493A,0x498A,0x49D9,0x4A29,0x4A77,0x4AC5,0x4B13,0x4B5F,0x4BAC,0x4BF7,0x4C42,0x4C8D,0x4CD7,0x4D20,0x4D68,0x4DB0,
    0x4DF7,0x4E3E,0x4E84,0x4EC9,0x4F0E,0x4F52,0x4F95,0x4FD7,0x5019,0x505A,0x509A,0x50DA,0x5118,0x5156,0x5194,0x51D0,
    0x520C,0x5247,0x5281,0x52BA,0x52F3,0x532A,0x5361,0x5397,0x53CC,0x5401,0x5434,0x5467,0x5499,0x54CA,0x54FA,0x5529,
    0x5558,0x5585,0x55B2,0x55DE,0x5609,0x5632,0x565B,0x5684,0x56AB,0x56D1,0x56F6,0x571B,0x573E,0x5761,0x5782,0x57A3,
    0x57C3,0x57E2,0x57FF,0x581C,0x5838,0x5853,0x586D,0x5886,0x589E,0x58B5,0x58CB,0x58E0,0x58F4,0x5907,0x5919,0x592A,
    0x593A,0x5949,0x5958,0x5965,0x5971,0x597C,0x5986,0x598F,0x5997,0x599E,0x59A4,0x59A9,0x59AD,0x59B0,0x59B2,0x59B3]

  result = cast[int16](table[0xff - index]).mix(samples[0])
  result += cast[int16](table[0x1ff - index]).mix(samples[1])
  result += cast[int16](table[0x100 + index]).mix(samples[2])
  result += cast[int16](table[index]).mix(samples[3])

type
  Interpolator = object
    history: array[4, int16]

proc keyOn(interpolator: var Interpolator) =
  interpolator.history = [0, 0, 0, 0]

proc cycle(interpolator: var Interpolator, sample: int16) =
  for i in 0..2:
    interpolator.history[i] = interpolator.history[i+1]
  interpolator.history[3] = sample

proc sample(interpolator: Interpolator, index: int): int16 =
  interpolate(index, interpolator.history)

######################################################################
## A single voice.

type
  Voice = object
    sampleGenerator: SampleGenerator
    interpolator: Interpolator
    adsr: Adsr
    volumeLeft, volumeRight: Sweep
    modulate: bool
    noise: bool
    reverb: bool

proc keyOn(voice: var Voice) =
  voice.sampleGenerator.keyOn()
  voice.interpolator.keyOn()
  voice.adsr.keyOn()

proc keyOff(voice: var Voice) =
  voice.adsr.keyOff()

proc sample(voice: Voice): int =
  if voice.noise: return 0
  voice.interpolator.sample(voice.sampleGenerator.interpolationIndex).mix(voice.adsr.volume).mix(voice.volumeLeft.volume) # TODO stereo

proc cycle(voice: var Voice) =
  var shouldMute: bool
  voice.sampleGenerator.cycle(shouldMute)
  voice.interpolator.cycle(voice.sampleGenerator.sample)
  if shouldMute:
    voice.adsr.mute()
  else:
    voice.adsr.cycle()
  voice.volumeLeft.cycle()
  voice.volumeRight.cycle()

var
  voices: array[24, Voice]

######################################################################
## Reverb.

var
  vLOUT, vROUT, mBASE, dAPF1, dAPF2, vIIR, vCOMB1, vCOMB2, vCOMB3, vCOMB4,
    vWALL, vAPF1, vAPF2, mLSAME, mRSAME, mLCOMB1, mRCOMB1, mLCOMB2, mRCOMB2,
    dLSAME, dRSAME, mLDIFF, mRDIFF, mLCOMB3, mRCOMB3, mLCOMB4, mRCOMB4,
    dLDIFF, dRDIFF, mLAPF1, mRAPF1, mLAPF2, mRAPF2, vLIN, vRIN: int16

######################################################################
## Main volume.

var
  volumeLeft, volumeRight: Sweep
  cdVolumeLeft, cdVolumeRight: int16
  externalInputVolumeLeft, externalInputVolumeRight: int16

######################################################################
## Control flags and DMA.

type
  Control = distinct uint16
  Status = distinct uint16
  TransferMode = enum tmStop, tmManualWrite, tmDMAWrite, tmDMARead
  CaptureBufferHalf = enum cbhFirst, cbhSecond

Control.bitfield enable, bool, 15, 1
Control.bitfield unmute, bool, 14, 1
Control.bitfield noiseFrequencyShift, int, 10, 4
Control.bitfield noiseFrequencyStep, int, 8, 2
Control.bitfield reverbMasterEnable, bool, 7, 1
Control.bitfield irqEnable, bool, 6, 1
Control.bitfield transferMode, TransferMode, 4, 2
Control.bitfield externalAudioReverb, bool, 3, 1
Control.bitfield cdAudioReverb, bool, 2, 1
Control.bitfield externalAudioEnable, bool, 1, 1
Control.bitfield cdAudioEnable, bool, 0, 1

Status.bitfield captureBufferHalf, CaptureBufferHalf, 11, 1
Status.bitfield dataTransferBusy, bool, 10, 1
Status.bitfield dmaReadRequest, bool, 9, 1
Status.bitfield dmaWriteRequest, bool, 8, 1
Status.bitfield dmaRequest, bool, 7, 1
Status.bitfield irq9, bool, 6, 1
Status.bitfield spuMode, int, 0, 5

var
  control: Control
  status: Status
  fifo: Deque[uint16]
  transferAddressDiv8 {.saved.}: uint16
  transferAddress {.saved.}: uint32

proc updateIRQ =
  if control.enable and control.irqEnable:
    status.irq9 = status.irq9 or spuram.watchTriggered
  else:
    status.irq9 = false
  spuram.watchTriggered = false
  irqs.set 9, status.irq9

proc spuReadDMA*: uint32 =
  if control.transferMode == tmDMARead:
    result = spuram.read32(transferAddress)
    transferAddress += 4
  else:
    warn fmt"SPU DMA read when transfer mode = {control.transferMode}"

proc spuWriteDMA*(value: uint32) =
  if control.transferMode == tmDMAWrite:
    trace fmt"Writing {value:x} to {transferAddress:x}"
    spuram.write32(transferAddress, value)
    transferAddress += 4
  else:
    warn fmt"SPU DMA write when transfer mode = {control.transferMode}"

######################################################################
## Memory map.

type
  Cell = object
    read: proc(): uint16
    write: proc(val: uint16)

template rw(v: untyped): Cell =
  assert v.sizeof == 2
  Cell(read: () => cast[uint16](v), write: (val: uint16) => (v = cast[typeof(v)](val)))
template rwDiv8(v: untyped): Cell =
  Cell(read: () => (v div 8).uint16, write: (val: uint16) => (v = typeof(v)(val) * 8))
template ro(v: untyped): Cell =
  assert v.sizeof == 2
  Cell(read: () => cast[uint16](v), write: proc (val: uint16) = discard)

template voiceBitLow(varname: untyped): Cell =
  Cell(
    read: proc(): uint16 =
      var res: uint16
      for i in 0..15:
        if voices[i].varname:
          res.setBit(i)
      res,
    write: proc(val: uint16) =
      for i in 0..15:
        voices[i].varname = val.testBit(i)
  )

template voiceBitHigh(varname: untyped): Cell =
  Cell(
    read: proc(): uint16 =
      var res: uint16
      for i in 16..23:
        if voices[i].varname:
          res.setBit(i-16)
      res,
    write: proc(val: uint16) =
      for i in 16..23:
        voices[i].varname = val.testBit(i-16)
  )

template generatorBitLow(varname: untyped): Cell =
  Cell(
    read: proc(): uint16 =
      var res: uint16
      for i in 0..15:
        if voices[i].sampleGenerator.varname:
          res.setBit(i)
      res,
    write: proc(val: uint16) =
      for i in 0..15:
        voices[i].sampleGenerator.varname = val.testBit(i)
  )

template generatorBitHigh(varname: untyped): Cell =
  Cell(
    read: proc(): uint16 =
      var res: uint16
      for i in 16..23:
        if voices[i].sampleGenerator.varname:
          res.setBit(i-16)
      res,
    write: proc(val: uint16) =
      for i in 16..23:
        voices[i].sampleGenerator.varname = val.testBit(i-16)
  )

var ports: array[512, Cell]

for i in 0..23:
  capture i:
    ports[i*8] = rw(voices[i].volumeLeft.flags)
    ports[i*8 + 1] = rw(voices[i].volumeRight.flags)
    ports[i*8 + 2] = rw(voices[i].sampleGenerator.sampleRate)
    ports[i*8 + 3] = rwDiv8(voices[i].sampleGenerator.startAddress)
    ports[i*8 + 4] = rw(voices[i].adsr.flags.lower)
    ports[i*8 + 5] = rw(voices[i].adsr.flags.upper)
    ports[i*8 + 6] = ro(voices[i].adsr.volume)
    ports[i*8 + 7] = rwDiv8(voices[i].sampleGenerator.repeatAddress)
    ports[i*2 + 0x100] = rw(voices[i].volumeLeft.volume)
    ports[i*2 + 0x101] = rw(voices[i].volumeRight.volume)

ports[0x190 div 2] = voiceBitLow(modulate)
ports[0x192 div 2] = voiceBitHigh(modulate)
ports[0x180 div 2] = rw(volumeLeft.flags)
ports[0x182 div 2] = rw(volumeRight.flags)
ports[0x1b8 div 2] = rw(volumeLeft.volume)
ports[0x1ba div 2] = rw(volumeRight.volume)
ports[0x1b0 div 2] = rw(cdVolumeLeft)
ports[0x1b2 div 2] = rw(cdVolumeRight)
ports[0x1b4 div 2] = rw(externalInputVolumeLeft)
ports[0x1b6 div 2] = rw(externalInputVolumeRight)

ports[0x188 div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 0..15:
      if val.testBit(i):
        voices[i].keyOn()
        debug fmt "key on {i}, {voices[i]}"
)

ports[0x18a div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        voices[i].keyOn()
        debug fmt "key on {i}, {voices[i]}"
)

ports[0x18c div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 0..15:
      if val.testBit(i):
        voices[i].keyOff()
        debug fmt "key off {i}, {voices[i]}"
)

ports[0x18e div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        voices[i].keyOff()
        debug fmt "key off {i}, {voices[i]}"
)

ports[0x19c div 2] = generatorBitLow(reachedLoopEnd)
ports[0x19e div 2] = generatorBitHigh(reachedLoopEnd)
ports[0x194 div 2] = voiceBitLow(noise)
ports[0x196 div 2] = voiceBitHigh(noise)
ports[0x1aa div 2] = rw(control)
ports[0x1ae div 2] = ro(status)
ports[0x1a6 div 2] = Cell(
  read: proc(): uint16 = transferAddressDiv8,
  write: proc(val: uint16) =
    transferAddressDiv8 = val
    transferAddress = val.uint32 * 8)
ports[0x1a8 div 2] = Cell(
  read: () => 0u16,
  write: (val: uint16) => fifo.addLast(val))
ports[0x1ac div 2] = Cell(
  read: () => 4u16,
  write: proc(val: uint16) =
    if val != 4:
      warn "Unknown transfer control")
ports[0x1a4 div 2] = rwDiv8(spuram.watchAddress)
ports[0x198 div 2] = voiceBitLow(reverb)
ports[0x19a div 2] = voiceBitHigh(reverb)

# Reverb registers
ports[0x184 div 2] = rw(vLOUT)
ports[0x186 div 2] = rw(vROUT)
ports[0x1A2 div 2] = rw(mBASE)
ports[0x1C0 div 2] = rw(dAPF1)
ports[0x1C2 div 2] = rw(dAPF2)
ports[0x1C4 div 2] = rw(vIIR)
ports[0x1C6 div 2] = rw(vCOMB1)
ports[0x1C8 div 2] = rw(vCOMB2)
ports[0x1CA div 2] = rw(vCOMB3)
ports[0x1CC div 2] = rw(vCOMB4)
ports[0x1CE div 2] = rw(vWALL)
ports[0x1D0 div 2] = rw(vAPF1)
ports[0x1D2 div 2] = rw(vAPF2)
ports[0x1D4 div 2] = rw(mLSAME)
ports[0x1D6 div 2] = rw(mRSAME)
ports[0x1D8 div 2] = rw(mLCOMB1)
ports[0x1DA div 2] = rw(mRCOMB1)
ports[0x1DC div 2] = rw(mLCOMB2)
ports[0x1DE div 2] = rw(mRCOMB2)
ports[0x1E0 div 2] = rw(dLSAME)
ports[0x1E2 div 2] = rw(dRSAME)
ports[0x1E4 div 2] = rw(mLDIFF)
ports[0x1E6 div 2] = rw(mRDIFF)
ports[0x1E8 div 2] = rw(mLCOMB3)
ports[0x1EA div 2] = rw(mRCOMB3)
ports[0x1EC div 2] = rw(mLCOMB4)
ports[0x1EE div 2] = rw(mRCOMB4)
ports[0x1F0 div 2] = rw(dLDIFF)
ports[0x1F2 div 2] = rw(dRDIFF)
ports[0x1F4 div 2] = rw(mLAPF1)
ports[0x1F6 div 2] = rw(mRAPF1)
ports[0x1F8 div 2] = rw(mLAPF2)
ports[0x1FA div 2] = rw(mRAPF2)
ports[0x1FC div 2] = rw(vLIN)
ports[0x1FE div 2] = rw(vRIN)

proc spuRead*(address: uint32): uint16 =
  assert address >= 0x1f801c00u32 and address < 0x1f802000u32
  assert address mod 2 == 0

  let read = ports[(address - 0x1f801c00u32) div 2].read
  if read == nil:
    warn fmt"Read from unknown SPU address {address:x}"
  else:
    result = read()

  updateIRQ()

proc spuWrite*(address: uint32, value: uint16) =
  assert address >= 0x1f801c00u32 and address < 0x1f802000u32
  assert address mod 2 == 0

  let write = ports[(address - 0x1f801c00u32) div 2].write
  if write == nil:
    warn fmt"Write of {value:x} to unknown SPU address {address:x}"
  else:
    write(value)
    trace fmt"Write of {value:x} to SPU address {address:x}"

  if control.transferMode == tmManualWrite:
    for value in fifo:
      trace fmt"Writing {value:x} to {transferAddress:x}"
      spuram[transferAddress] = value
      transferAddress += 2
    fifo.clear

  status.dmaReadRequest = control.transferMode == tmDMARead
  status.dmaWriteRequest = control.transferMode == tmDMAWrite
  status.dmaRequest = status.dmaReadRequest or status.dmaWriteRequest
  status.spuMode = control.int and 0x3f

  updateIRQ()

######################################################################
## Audio output.

var
  audioBuffer*: seq[int16]

proc processSPU =
  var sum = 0
  for i in 0..<24:
    voices[i].cycle()
    sum += voices[i].sample().int

  audioBuffer.add sum.clampedConvert[:int16]

  updateIRQ()

events.every(() => 44100.hz, "SPU processing") do():
  processSPU()
