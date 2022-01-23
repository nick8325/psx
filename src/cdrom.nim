## The CD-ROM controller.

import basics, utils, irq, eventqueue
import std/[bitops, strformat, deques, options, logging]

var logger = newLogger("CD-ROM", minLevel = lvlDebug)

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

proc interruptPending: bool =
  if interrupts.len > 0 and (enabledInterrupts and 7) != 0:
    return true
  if commandStart and enabledInterrupts.testBit(4):
    return true
  return false

proc checkInterrupts =
  echo fmt"{interrupts} {commandStart} {enabledInterrupts:08x} {smen}"
  irqs.set(2, interruptPending())

proc queueInterrupt(interrupt: range[0..5]) =
  if not (interrupt in interrupts):
    interrupts.addLast interrupt
  checkInterrupts()

proc readFIFO(fifo: var Deque[uint8], res: var uint8): bool =
  if fifo.len >= 1:
    res = fifo.popFirst()
    return true
  else:
    logger.warn "empty FIFO"
    queueInterrupt 5
    return false

proc readData16*: uint16 =
  if data.len >= 2:
    let x = data.popFirst()
    let y = data.popFirst()
    return x.uint16 + y.uint16 shl 8
  else:
    logger.warn "empty FIFO"

proc respond*(interrupt: 0..5, values: openarray[int]) =
  for x in values: response.addLast x.uint8
  queueInterrupt interrupt

proc command*(value: uint8) =
  # TODO: start of command interrupt

  logger.debug fmt"Command {value:02x}"
  case value
  of 0x1:
    # Stat
    respond 3, [0x10]
  of 0x19:
    # Test
    var param: uint8
    if not parameters.readFIFO(param): return
    case param
    of 0x20:
      respond 3, [0x97, 0x08, 0x14, 0xc2]
    else:
      logger.debug fmt"Unknown test command {param:02x}"
      respond 5, []
  of 0x1a:
    # GetID
    respond 5, [0x11, 0x80]
    #respond 5, [0x08, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] # No disc
  else:
    logger.warn fmt"Unknown command {value:02x}"
    queueInterrupt 5

proc readStatus*: uint8 =
  index.uint8 or
  # TODO: XA-ADPCM FIFO empty
  (uint8(parameters.len == 0) shl 3) or
  (uint8(response.len != 0) shl 5) or
  (uint8(data.len != 0) shl 6)
  # TODO: need to set busy bit?

proc writeStatus*(value: uint8) =
  index = value and 3

proc readRegister*(address: 1..3): uint8 =
  case address
  of 1:
    # Response FIFO
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
  # logger.debug fmt"Reading from index {index}, address {address} => {result:02x}"

proc writeRegister*(address: 1..3, value: uint8) =
  # logger.debug fmt"Writing {value:02x} to index {index}, address {address}"
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
      logger.warn "Write {value:02x} to unknown address/index {address}/{index}"
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
      logger.warn "Write {value:02x} to unknown address/index {address}/{index}"
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
