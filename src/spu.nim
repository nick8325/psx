## The sound processing unit (SPU).
import basics, utils, irq, eventqueue, savestates
import std/[strformat, deques, sugar, bitops]

const loggerComponent = logSPU
logSPU.level = lvlDebug

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
      target: ((adsr.flags.sustainLevel)+1*0x800).int16,
      shift: adsr.flags.decayShift,
      rawStep: 0)
  of apSustain:
    RampSettings(
      mode: adsr.flags.sustainMode,
      direction: adsr.flags.sustainDirection,
      target:
        case adsr.flags.sustainDirection
        of dIncrease: 0x7ffff
        of dDecrease: 0,
      shift: adsr.flags.sustainShift,
      rawStep: adsr.flags.sustainStep)
  of apRelease:
    RampSettings(
      mode: adsr.flags.releaseMode,
      direction: dDecrease,
      target: 0,
      shift: adsr.flags.attackShift,
      rawStep: 0)

proc rampStep(volume: int16, settings: RampSettings): tuple[waitFor: int, stepAfter: int] =
  var waitFor = 1 shl max(0, settings.shift - 11)
  var stepAfter = settings.step shl max(0, 11 - settings.shift)
  if settings.mode == mExponential:
    case settings.direction
    of dIncrease:
      if volume > 0x6000: waitFor *= 4
    of dDecrease:
      stepAfter = (stepAfter * volume.int div 0x8000)
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
  else:
    adsr.waitFor.dec

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
  elif sweep.waitFor == 0:
    sweep.volume = (sweep.volume.int + sweep.stepAfter).clampedConvert[:int16]
    let ramp = rampStep(sweep.volume, sweep.settings)
    sweep.waitFor = ramp.waitFor
    sweep.stepAfter = ramp.stepAfter
  else:
    sweep.waitFor -= 1

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

proc sampleNumber(generator: SampleGenerator): int =
  generator.counter shr 12

proc keyOn(generator: var SampleGenerator) =
  generator.currentAddress = generator.startAddress
  generator.reachedLoopEnd = false
  generator.counter = 0

proc cycle(generator: var SampleGenerator, shouldMute: var bool) =
  shouldMute = false

  var step = generator.sampleRate
  # TODO: pitch modulation
  step = step.clamp(0, 0x3fff)
  generator.counter += step.int

  let flags = spuram.read8(generator.currentAddress + 1)
  if flags.testBit 2:
    generator.repeatAddress = generator.currentAddress

  if generator.sampleNumber >= 28:
    generator.counter -= 28 shl 12
    if flags.testBit 0:
      generator.reachedLoopEnd = true
      generator.currentAddress = generator.repeatAddress
      if not flags.testBit 1:
        shouldMute = true
    else:
      generator.currentAddress += 16

proc volume(x, y: int16): int16 =
  ((x.int * y.int) div 0x8000).int16

proc sample(generator: SampleGenerator): int16 =
  var shift = spuram.read8(generator.currentAddress) and 0xf
  if shift > 12: shift = 9
  let bytePos = generator.sampleNumber div 2
  let nybblePos = generator.sampleNumber mod 2
  let byte = spuram.read8(generator.currentAddress + bytePos.uint16 + 2)
  let nybble =
    if nybblePos == 0: byte and 0xf
    else: byte shr 8
  nybble.int16 shl 12 shr shift

######################################################################
## A single voice.

type
  Voice = object
    sampleGenerator: SampleGenerator
    adsr: Adsr
    volumeLeft, volumeRight: Sweep

proc keyOn(voice: var Voice) =
  voice.adsr.keyOn()
  voice.sampleGenerator.keyOn()

proc keyOff(voice: var Voice) =
  voice.adsr.keyOff()

proc sample(voice: Voice): int =
  voice.sampleGenerator.sample().volume(voice.adsr.volume).volume(voice.volumeLeft.volume) # TODO stereo

proc cycle(voice: var Voice) =
  var shouldMute: bool
  voice.sampleGenerator.cycle(shouldMute)
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

# ports[0x190 div 2] = voiceBitLow(modulate)
# ports[0x192 div 2] = voiceBitHigh(modulate)
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
        debug fmt "key on {i}, {voices[i]}"
        voices[i].keyOn()
)

ports[0x18a div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        debug fmt "key on {i}, {voices[i]}"
        voices[i].keyOn()
)

ports[0x18c div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 0..15:
      if val.testBit(i):
        debug fmt "key off {i}, {voices[i]}"
        voices[i].keyOff()
)

ports[0x18e div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        debug fmt "key off {i}, {voices[i]}"
        voices[i].keyOff()
)

ports[0x19c div 2] = generatorBitLow(reachedLoopEnd)
ports[0x19e div 2] = generatorBitHigh(reachedLoopEnd)
# ports[0x194 div 2] = voiceBitLow(noise)
# ports[0x196 div 2] = voiceBitHigh(noise)
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
# ports[0x198 div 2] = voiceBitLow(reverb)
# ports[0x19a div 2] = voiceBitHigh(reverb)

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
    sum += voices[i].sample().clampedConvert[:int16]

  audioBuffer.add sum.clampedConvert[:int16]

  updateIRQ()

events.every(() => 44100.hz, "SPU processing") do():
  processSPU()
