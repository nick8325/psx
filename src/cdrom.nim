## The CD-ROM controller.

import utils, irq, eventqueue, basics
import std/[bitops, strformat, deques, options]

const loggerComponent = logCDROM
logCDROM.level = lvlDebug

type
  Channel {.pure.} = enum Left, Right

var
  # This selects which register gets read/written
  index: 0..3

  # Various FIFOs
  parameters = initDeque[uint8]()
  data = initDeque[uint8]()
  response = initDeque[uint8]()

  # First argument: CD out Left/Right, second argument: SPU in Left/Right
  volume: array[Channel, array[Channel, uint8]]

  smen: bool # Command Start Interrupt on next command
  bfrd: bool # Load data into FIFO

  # Queued interrupts
  interrupts = initDeque[range[0..5]]()
  commandStart: bool

  # Interrupt enable register
  enabledInterrupts: uint8

  # "Setmode" mode register
  mode: uint8

proc interruptPending: bool =
  if interrupts.len > 0 and (enabledInterrupts and 7) != 0:
    return true
  if commandStart and enabledInterrupts.testBit(4):
    return true
  return false

proc checkInterrupts =
  trace fmt"{interrupts} {commandStart} {enabledInterrupts:08x} {smen}"
  events.after(20000*cpuClock, "CDROM delay") do(): irqs.set(2, interruptPending())

proc queueInterrupt(interrupt: range[0..5]) =
  if not (interrupt in interrupts):
    interrupts.addLast interrupt
  checkInterrupts()

proc readFIFO(fifo: var Deque[uint8], res: var uint8): bool =
  if fifo.len >= 1:
    res = fifo.popFirst()
    return true
  else:
    warn "empty FIFO"
    queueInterrupt 5
    return false

proc readData16*: uint16 =
  if data.len >= 2:
    let x = data.popFirst()
    let y = data.popFirst()
    return x.uint16 + y.uint16 shl 8
  else:
    warn "empty FIFO"

proc respond*(interrupt: 0..5, values: openarray[int]) =
  parameters.clear
  for x in values: response.addLast x.uint8
  queueInterrupt interrupt

proc command*(value: uint8) =
  # TODO: start of command interrupt

  debug fmt"Command {value:02x}"
  if smen: commandStart = true
  case value
  of 0x1:
    # Stat
    respond 3, [0x18]
  of 0x2:
    # Setloc
    respond 3, [0x18]
  of 0x19:
    # Test
    var param: uint8
    if not parameters.readFIFO(param): return
    case param
    of 0x20:
      respond 3, [0x97, 0x08, 0x14, 0xc2]
    else:
      warn fmt"Unknown test command {param:02x}"
      respond 5, []
  of 0x1a:
    # GetID
    respond 3, [0x18]
    events.after(2000, "CDROM delay") do(): respond 2, [0x02, 0x00, 0x20, 0x00, 0x53, 0x43, 0x45, 0x41] # SCEA
  of 0x13:
    # GetTN
    respond 3, [0x18, 0, 0]
  of 0x14:
    # GetTD
    var param: uint8
    if not parameters.readFIFO(param): return
    case param
    of 0: respond 3, [2, 1, 0]
    of 1: respond 3, [2, 0, 0]
    else:
      warn fmt "Unknown track number {param}"
      respond 5, [0x18]
  of 0xe:
    # Setmode
    if not parameters.readFIFO(mode): return
    debug fmt"Mode is {mode:x}"
    respond 3, [0x18]
  of 0x9:
    # Pause
    respond 3, [0x18]
    respond 2, [0x18]
  of 0xa:
    # Init
    mode = 0
    data.clear
    respond 3, [0x18]
    respond 2, [0x18]
  else:
    warn fmt"Unknown command {value:02x}"
    queueInterrupt 5

proc readStatus*: uint8 =
  result =
    index.uint8 or
    # TODO: XA-ADPCM FIFO empty
    (uint8(parameters.len == 0) shl 3) or
    (1 shl 4) or # Parameter FIFO not full
    (uint8(response.len != 0) shl 5) or
    (uint8(data.len != 0) shl 6)
    # TODO: need to set busy bit?
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
    if bfrd:
      discard readFIFO(data, result)
    else:
      warn "Reading data FIFO when BFRD=0"
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
      command(value)
    of 2:
      # Parameter FIFO
      parameters.addLast value
    of 3:
      # Request register
      smen = smen or value.testBit 5
      bfrd = value.testBit 7
      # TODO is this right?
      if not bfrd: data.clear
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
