## The sound processing unit (SPU).
import basics, utils, irq, eventqueue, savestates
import std/[strformat, deques, sugar, bitops]

const loggerComponent = logSPU
logSPU.level = lvlDebug

var
  spuramRaw {.saved.}: array[0x40000, uint16]

type
  Voice = object
    id: int
    startAddress: uint16
    currentAddress: uint16
    repeatAddress: uint16
    sampleRate: uint16
    modulate: bool
    sweepLeft, sweepRight: Sweep
    adsr: Adsr
    adsrVolume: int16
    volumeLeft, volumeRight: int16
    currentVolumeLeft, currentVolumeRight: int16
    reachedLoopEnd: bool
    noise: bool
    reverb: bool

  Adsr = distinct uint32
  Sweep = distinct uint16
  Mode = enum mLinear, mExponential
  Direction = enum dIncrease, dDecrease
  SweepPhase = enum spPositive, spNegative

  Control = distinct uint16
  Status = distinct uint16
  TransferMode = enum tmStop, tmManualWrite, tmDMAWrite, tmDMARead
  CaptureBufferHalf = enum cbhFirst, cbhSecond

Adsr.bitfield sustainMode, Mode, 31, 1
Adsr.bitfield sustainDirection, Direction, 30, 1
Adsr.bitfield sustainShift, int, 24, 5
Adsr.bitfield sustainStep, int, 22, 2
Adsr.bitfield releaseMode, Mode, 21, 1
Adsr.bitfield releaseShift, int, 16, 5
Adsr.bitfield attackMode, Mode, 15, 1
Adsr.bitfield attackShift, int, 10, 5
Adsr.bitfield attackStep, int, 8, 2
Adsr.bitfield decayShift, int, 4, 4
Adsr.bitfield sustainLevel, int, 0, 4
Adsr.bitfield lower, uint16, 0, 16
Adsr.bitfield upper, uint16, 16, 16

Sweep.bitfield sweep, bool, 15, 1
Sweep.bitfield volumeDiv2, int16, 0, 15
Sweep.bitfield sweepMode, Mode, 14, 1
Sweep.bitfield sweepDirection, Direction, 13, 1
Sweep.bitfield sweepPhase, SweepPhase, 12, 1
Sweep.bitfield sweepShift, int, 2, 5
Sweep.bitfield sweepStep, int, 0, 2

Control.bitfield enable, bool, 15, 1
Control.bitfield unmute, bool, 14, 1
Control.bitfield noiseFrequencyShift, int, 10, 4
Control.bitfield noiseFrequencyStep, int, 8, 2
Control.bitfield reverbMasterEnable, bool, 7, 1
Control.bitfield irqEnable, bool, 6, 1
Control.bitfield soundRAMTransferMode, TransferMode, 4, 2
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
  sweepLeft, sweepRight: Sweep # Correct? Or just normal volume?
  cdVolumeLeft, cdVolumeRight: int16
  externalInputVolumeLeft, externalInputVolumeRight: int16
  volumeLeft, volumeRight: int16
  currentVolumeLeft, currentVolumeRight: int16
  transferAddressDiv8: uint16
  fifo: Deque[uint16]
  irqAddressDiv8: uint16
  control: Control
  status: Status
  voices: array[24, Voice]

var
  vLOUT, vROUT, mBASE, dAPF1, dAPF2, vIIR, vCOMB1, vCOMB2, vCOMB3, vCOMB4,
    vWALL, vAPF1, vAPF2, mLSAME, mRSAME, mLCOMB1, mRCOMB1, mLCOMB2, mRCOMB2,
    dLSAME, dRSAME, mLDIFF, mRDIFF, mLCOMB3, mRCOMB3, mLCOMB4, mRCOMB4,
    dLDIFF, dRDIFF, mLAPF1, mRAPF1, mLAPF2, mRAPF2, vLIN, vRIN: int16

type
  Cell = object
    read: proc(): uint16
    write: proc(val: uint16)

template rw(v: untyped): Cell =
  Cell(read: () => v.uint16, write: (val: uint16) => (v = typeof(v)(val)))
template ro(v: untyped): Cell =
  Cell(read: () => v.uint16, write: proc (val: uint16) = discard)

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

var ports: array[512, Cell]

for i in 0..23:
  ports[i*8] = rw(voices[i].volumeLeft)
  ports[i*8 + 1] = rw(voices[i].volumeRight)
  ports[i*8 + 2] = rw(voices[i].sampleRate)
  ports[i*8 + 3] = rw(voices[i].startAddress)
  ports[i*8 + 4] = rw(voices[i].adsr.lower)
  ports[i*8 + 5] = rw(voices[i].adsr.upper)
  ports[i*8 + 6] = rw(voices[i].adsrVolume)
  ports[i*8 + 7] = rw(voices[i].repeatAddress)
  ports[i*2 + 0x100] = rw(voices[i].currentVolumeLeft)
  ports[i*2 + 0x101] = rw(voices[i].currentVolumeRight)

ports[0x190 div 2] = voiceBitLow(modulate)
ports[0x192 div 2] = voiceBitHigh(modulate)
ports[0x180 div 2] = rw(volumeLeft)
ports[0x182 div 2] = rw(volumeRight)
ports[0x1b8 div 2] = rw(currentVolumeLeft)
ports[0x1ba div 2] = rw(currentVolumeRight)
ports[0x1b0 div 2] = rw(cdVolumeLeft)
ports[0x1b2 div 2] = rw(cdVolumeRight)
ports[0x1b4 div 2] = rw(externalInputVolumeLeft)
ports[0x1b6 div 2] = rw(externalInputVolumeRight)

ports[0x188 div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 0..15:
      if val.testBit(i):
        # Start the ADSR envelope, set ADSR volume to 0, and copy start address to repeat address
        discard
)

ports[0x18a div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        # Start the ADSR envelope, set ADSR volume to 0, and copy start address to repeat address
        discard
)

ports[0x18c div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 0..15:
      if val.testBit(i):
        # key off
        discard
)

ports[0x18e div 2] = Cell(
  read: proc(): uint16 = 0,
  write: proc(val: uint16) =
    for i in 16..23:
      if val.testBit(i-16):
        # key off
        discard
)

# TODO: voiceBit is 32 bits so this doesn't really work!
ports[0x19c div 2] = voiceBitLow(reachedLoopEnd)
ports[0x19e div 2] = voiceBitHigh(reachedLoopEnd)
ports[0x194 div 2] = voiceBitLow(noise)
ports[0x196 div 2] = voiceBitHigh(noise)
ports[0x1aa div 2] = rw(control)
ports[0x1ae div 2] = ro(status)
ports[0x1a6 div 2] = rw(transferAddressDiv8)
ports[0x1a8 div 2] = Cell(
  read: () => 0u16,
  write: (val: uint16) => fifo.addLast(val))
ports[0x1ac div 2] = Cell(
  read: () => 4u16,
  write: proc(val: uint16) =
    if val != 4:
      warn "Unknown transfer control")
ports[0x1a4 div 2] = rw(irqAddressDiv8)
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
