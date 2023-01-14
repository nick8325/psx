## The CD-ROM controller.

import utils, irq, eventqueue, basics, savestates
import std/[bitops, strformat, deques, options]
import cdrom/image

const loggerComponent = logCDROM
#logCDROM.level = lvlDebug

let
  cd = readCUE "test.cue"

const
  framesPerSec = 75

proc toFrame(min, sec, sect: int): int =
  min*60*framesPerSec + sec*framesPerSec + sect

proc toTime(frame: int): tuple[min: int, sec: int, sect: int] =
  let totalSecs = frame div framesPerSec
  result.min = totalSecs div 60
  result.sec = totalSecs mod 60
  result.sect = frame mod framesPerSec

proc bcd(x: uint8): uint8 =
  assert x < 100
  return (x div 10) * 16 + (x mod 10)

proc unBCD(x: uint8): uint8 =
  assert (x mod 16) < 10
  return (x div 16) * 10 + (x mod 16)

type
  Channel {.pure.} = enum Left, Right

var
  # This selects which register gets read/written
  index {.saved.}: 0..3

  # Various FIFOs
  parameters {.saved.} = initDeque[uint8]()
  data {.saved.} = initDeque[uint8]()
  buffer {.saved.} = initDeque[seq[uint8]]()
  response {.saved.} = initDeque[uint8]()

  # First argument: CD out Left/Right, second argument: SPU in Left/Right
  volume {.saved.}: array[Channel, array[Channel, uint8]]

  smen {.saved.}: bool # Command Start Interrupt on next command
  bfrd {.saved.}: bool # Load data into FIFO

  # Queued interrupts
  interrupts {.saved.} = initDeque[range[0..5]]()
  commandStart {.saved.}: bool

  # Interrupt enable register
  enabledInterrupts {.saved.}: uint8

  # "Setmode" mode register
  mode {.saved.}: uint8

  # Location seeked to
  seekPos {.saved.}: int
  firstStat {.saved.}: bool = true
  reading {.saved.}: bool
  busy {.saved.}: bool

proc interruptPending: bool =
  if interrupts.len > 0 and (enabledInterrupts and 7) != 0:
    return true
  if commandStart and enabledInterrupts.testBit(4):
    return true
  return false

proc checkInterrupts =
  trace fmt"{interrupts} {commandStart} {enabledInterrupts:08x} {smen}"
  irqs.set(2, interruptPending())

proc dumpCDROM*: string =
  fmt"""
index={index}
parameters={parameters}
response={response}
smen={smen}
bfrd={bfrd}
interrupts={interrupts}
commandStart={commandStart}
enabledInterrupts={enabledInterrupts}
mode={mode}
seekPos={seekPos}
firstStat={firstStat}
interruptPending={interruptPending()}
reading={reading}"""

proc respond*(interrupt: 0..5, values: openarray[uint8]) =
  var msg = fmt"Response {interrupt}:"
  for val in values: msg &= fmt" {val:02x}"
  debug msg
  if interrupt in interrupts:
    debug "Skipping interrupt because already triggered"
  else:
    if not (interrupt in interrupts):
      interrupts.addLast interrupt
      for value in values:
        response.addLast value
    checkInterrupts()

var emptyFIFOs = 0
proc readFIFO(fifo: var Deque[uint8], res: var uint8): bool =
  if fifo.len >= 1:
    res = fifo.popFirst()
    return true
  else:
    emptyFIFOs.inc
    warn fmt"empty FIFO {emptyFIFOs}"
    return false

proc readData8*: uint8 =
  trace fmt"reading from data FIFO, length {data.len}"
  discard readFIFO(data, result)
  if data.len == 0:
    debug "Data FIFO is empty now"

proc readData16*: uint16 =
  let x = readData8()
  let y = readData8()
  return x.uint16 + y.uint16 shl 8

proc cdromReadDMA*: word =
  let a = readData8()
  let b = readData8()
  let c = readData8()
  let d = readData8()
  result = a.word + b.word shl 8 + c.word shl 16 + d.word shl 24

proc stat: uint8 =
  if reading: result = result or 0x20
  if firstStat: result = result or 0x10
  result = result or 0x2

proc scheduleRead* =
  events.after(400000*cpuClock, "CDROM delay") do():
    if reading:
      var offset, limit: int
      if (mode and 0x20) != 0:
        # Raw read
        offset = 0xc
        limit = 2340
      else:
        # Non-raw read
        offset = 0x18
        limit = 2048
      debug fmt"Reading sector of {limit} bytes from sector {seekPos}, offset {offset}"
      let sector = cd.read(seekPos)[offset ..< offset+limit]
      buffer.addLast sector
      #var msg = "Data: "
      #for x in data: msg &= fmt"{x:02x}"
      #debug msg
      #debug fmt"Data FIFO has length {data.len}"
      respond 1, [stat()]
      seekPos += 1
      scheduleRead()

proc command*(value: uint8) =
  busy = false

  if response.len != 0:
    warn "Response buffer not empty when command sent"
    echo dumpCDROM()

  checkInterrupts()
  case value
  of 0x1:
    debug "Getstat"
    respond 3, [stat()]
    firstStat = false
  of 0x2:
    debug "Setloc"
    var min, sec, sect: uint8
    if not parameters.readFIFO(min): return
    if not parameters.readFIFO(sec): return
    if not parameters.readFIFO(sect): return
    min = min.unBCD
    sec = sec.unBCD
    sect = sect.unBCD
    debug fmt"Seek to {min:02}:{sec:02}/{sect}"
    seekPos = toFrame(min.int, sec.int, sect.int)
    respond 3, [stat()]
  of 0x15:
    debug "SeekL"
    respond 3, [stat()]
    events.after(4000000*cpuClock, "CDROM delay") do(): respond 2, [stat()]
  of 0x19:
    debug "Test"
    var param: uint8
    if not parameters.readFIFO(param): return
    case param
    of 0x20:
      respond 3, [0x95u8, 0x05, 0x16, 0xc1]
    else:
      warn fmt"Unknown test command {param:02x}"
      respond 5, []
  of 0x1a:
    debug "GetID"
    respond 3, [stat()]
    events.after(400000*cpuClock, "CDROM delay") do(): respond 2, [stat(), 0x00, 0x20, 0x00, 0x53, 0x43, 0x45, 0x45] # SCEE
  of 0x13:
    debug "GetTN"
    respond 3, [stat(), 1, cd.trackCount.uint8.bcd]
  of 0x14:
    debug "GetTD"
    var param: uint8
    if not parameters.readFIFO(param): return
    let track = param.unBCD.int
    if track == 0:
      let time = cd.sectors.toTime
      respond 3, [stat(), time.min.uint8.bcd, time.sec.uint8.bcd]
    elif track >= 1 and track <= cd.trackCount:
      let time = cd.trackStart(track).toTime
      respond 3, [stat(), time.min.uint8.bcd, time.sec.uint8.bcd]
    else:
      warn fmt "Unknown track number {track}"
      respond 5, [0x10u8]
  of 0xe:
    debug "Setmode"
    if not parameters.readFIFO(mode): return
    debug fmt"Mode is {mode:x}"
    if (mode and 0x1f) != 0: warn fmt "Unknown mode {mode:x}"
    events.after(400000*cpuClock, "CDROM delay") do(): respond 3, [stat()]
  of 0x6:
    debug "ReadN"
    respond 3, [stat()]
    reading=true
    scheduleRead()
  of 0x1b:
    debug "ReadS"
    respond 3, [stat()]
    reading=true
    scheduleRead()
  of 0x1e:
    debug "ReadTOC"
    respond 3, [stat()]
    events.after(4000000*cpuClock, "CDROM delay") do():
      respond 2, [stat()]
  of 0x9:
    debug "Pause"
    respond 3, [stat()]
    reading = false
    events.after(400000*cpuClock, "CDROM delay") do():
      respond 2, [stat()]
  of 0xa:
    debug "Init"
    respond 3, [stat()]
    events.after(400000*cpuClock, "CDROM delay") do():
      mode = 0x20
      reading = false
      seekPos = 0
      data.clear
      response.clear
      interrupts.clear
      parameters.clear
      commandStart = false
      respond 2, [stat()]
  else:
    warn fmt"Unknown command {value:02x}"
    respond 5, []

proc readStatus*: uint8 =
  result =
    index.uint8 or
    # TODO: XA-ADPCM FIFO empty
    (uint8(parameters.len == 0) shl 3) or
    (1 shl 4) or # Parameter FIFO not full
    (uint8(response.len != 0) shl 5) or
    (uint8(data.len != 0) shl 6) or
    (busy.uint8 shl 7)
  trace fmt "read status {result:08x}"

proc writeStatus*(value: uint8) =
  index = value and 3

proc readRegister*(address: 1..3): uint8 =
  case address
  of 1:
    # Response FIFO
    trace fmt"read response {response}"
    discard readFIFO(response, result)
  of 2:
    # Data FIFO
    discard readFIFO(data, result)
  of 3:
    case index
    of 0, 2:
      # Interrupt enable register
      return enabledInterrupts or 0xe0
    of 1, 3:
      # Interrupt flag register
      if interrupts.len > 0:
        result = interrupts[0].uint8
      if commandStart:
        result = result or 0x10
      result = result or 0xe0
  trace fmt"Reading from index {index}, address {address} => {result:02x}"

proc writeRegister*(address: 1..3, value: uint8) =
  trace fmt"Writing {value:02x} to index {index}, address {address}"
  # The CD controller has a *lot* of registers!
  case index
  of 0:
    case address
    of 1:
      # Command register
      if smen: commandStart = true
      busy = true
      checkInterrupts()
      events.after(0x1000 * cpuClock, "CDROM delay") do ():
        command(value)
    of 2:
      # Parameter FIFO
      parameters.addLast value
    of 3:
      # Request register
      smen = smen or value.testBit 5
      bfrd = value.testBit 7
      if bfrd:
        if len(buffer) > 0:
          data = buffer.popFirst().toDeque()
        else:
          warn "Setting BFRD when sector buffer empty"
      else:
        data.clear
  of 1:
    case address
    of 1:
      # Unknown
      warn "Write {value:02x} to unknown address/index {address}/{index}"
    of 2:
      # Interrupt enable register
      enabledInterrupts = value and 0x1f
      checkInterrupts()
    of 3:
      # Interrupt flag register
      if (value and 0x7) != 0:
        if interrupts.len > 0:
          debug fmt"ACK interrupt {interrupts[0]}"
          discard interrupts.popFirst
      if value.testBit 4: commandStart = false
      if value.testBit 6: parameters.clear
      checkInterrupts()
  of 2:
    case address
    of 1:
      # Unknown
      warn "Write {value:02x} to unknown address/index {address}/{index}"
    of 2:
      # Audio volume left->left
      volume[Left][Left] = value
    of 3:
      # Audio volume left->right
      volume[Left][Right] = value
  of 3:
    case address
    of 1:
      # Audio volume right->left
      volume[Right][Left] = value
    of 2:
      # Audio volume right->right
      volume[Right][Right] = value
    of 3:
      # Audio volume apply changes
      discard
