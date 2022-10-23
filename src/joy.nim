## Joypad/memory card bus.

import basics, utils, irq, eventqueue, savestates
import std/[strformat, math, options, deques, sugar]
import sdl2

const loggerComponent = logJoy

type
  ## A device connected to the bus, i.e. a joypad or memory card.
  Joypad* = proc(val: uint8): tuple[done: bool, reply: seq[uint8]]

var
  pads* {.saved.}: array[2, array[256, Joypad]]

type
  Controller = object
    pos: range[0..3]

var
  controller {.saved.}: Controller

proc send(controller: var Controller, val: uint8): tuple[done: bool, reply: seq[uint8]] =
  case controller.pos
  of 0:
    controller.pos.inc
    if val != 0x42:
      warn fmt "Unknown command {val:x}"
    return (done: false, reply: @[0x41u8])
  of 1:
    controller.pos.inc
    return (done: false, reply: @[0x5au8])
  of 2:
    controller.pos.inc
    let keys = getKeyboardState()
    var keys1: uint8 = 0xff
    if keys[SDL_SCANCODE_RSHIFT.int] == 1: keys1 = keys1 and not 0x1u8
    if keys[SDL_SCANCODE_RETURN.int] == 1: keys1 = keys1 and not 0x8u8
    if keys[SDL_SCANCODE_UP.int] == 1: keys1 = keys1 and not 0x10u8
    if keys[SDL_SCANCODE_RIGHT.int] == 1: keys1 = keys1 and not 0x20u8
    if keys[SDL_SCANCODE_DOWN.int] == 1: keys1 = keys1 and not 0x40u8
    if keys[SDL_SCANCODE_LEFT.int] == 1: keys1 = keys1 and not 0x80u8
    return (done: false, reply: @[keys1])
  of 3:
    controller.pos = 0
    let keys = getKeyboardState()
    var keys2: uint8 = 0xff
    if keys[SDL_SCANCODE_F.int] == 1: keys2 = keys2 and not 0x1u8
    if keys[SDL_SCANCODE_V.int] == 1: keys2 = keys2 and not 0x2u8
    if keys[SDL_SCANCODE_D.int] == 1: keys2 = keys2 and not 0x4u8
    if keys[SDL_SCANCODE_C.int] == 1: keys2 = keys2 and not 0x8u8
    if keys[SDL_SCANCODE_S.int] == 1: keys2 = keys2 and not 0x10u8
    if keys[SDL_SCANCODE_Z.int] == 1: keys2 = keys2 and not 0x20u8
    if keys[SDL_SCANCODE_X.int] == 1: keys2 = keys2 and not 0x40u8
    if keys[SDL_SCANCODE_A.int] == 1: keys2 = keys2 and not 0x80u8
    return (done: true, reply: @[keys2])

type
  MemoryCardState {.pure.} = enum
    Initial, ReceiveID1, ReceiveID2, SendAddressMSB, SendAddressLSB,
    ReceiveCommandAck1, ReceiveCommandAck2, ReceiveAddressMSB,
    ReceiveAddressLSB, ReceiveData, ReceiveChecksum, End
  MemoryCard = object
    state: MemoryCardState
    msb, lsb: uint8
    checksum: uint8
    data: Deque[uint8]
    writing: bool
    written: bool

var
  memoryCard: MemoryCard

const memoryCardData = staticRead "../test.mcr"

proc send(card: var MemoryCard, val: byte): tuple[done: bool, reply: seq[uint8]] =
  let oldState = card.state
  case card.state
  of Initial:
    case val
    of 0x52:
      card.state.inc
      card.writing = false
      result = (done: false, reply: @[if card.written: 0 else: 8])
    of 0x57:
      card.state.inc
      card.writing = true
      card.written = true
      result = (done: false, reply: @[if card.written: 0 else: 8])
    else:
      warn fmt"Unknown memory card command: {val:02x}"
      card.state = Initial
      result = (done: true, reply: @[0xff])
  of ReceiveID1:
    card.state.inc
    result = (done: false, reply: @[0x5a])
  of ReceiveID2:
    card.state.inc
    result = (done: false, reply: @[0x5d])
  of SendAddressMSB:
    card.msb = val and 3
    card.state.inc
    result = (done: false, reply: @[0])
  of SendAddressLSB:
    card.lsb = val
    card.checksum = card.msb xor card.lsb

    let sector = int(card.lsb) + int(card.msb) shl 8
    card.data.clear
    for i in 0..<128:
      let data = memoryCardData[sector*128+i].uint8
      card.data.addLast data
      card.checksum = card.checksum xor data

    if card.writing:
      card.state = ReceiveData
    else:
      card.state = ReceiveCommandAck1
    result = (done: false, reply: @[0])
  of ReceiveCommandAck1:
    card.state.inc
    result = (done: false, reply: @[0x5c])
  of ReceiveCommandAck2:
    if card.writing:
      card.state = End
    else:
      card.state = ReceiveAddressMSB
    result = (done: false, reply: @[0x5d])
  of ReceiveAddressMSB:
    card.state.inc
    result = (done: false, reply: @[card.msb])
  of ReceiveAddressLSB:
    card.state.inc
    result = (done: false, reply: @[card.lsb])
  of ReceiveData:
    let data = card.data.popFirst
    if len(card.data) == 0:
      card.state.inc
    result = (done: false, reply: @[data])
  of ReceiveChecksum:
    if card.writing:
      card.state = ReceiveCommandAck1
    else:
      card.state = End
    result = (done: false, reply: @[card.checksum])
  of End:
    card.state = Initial
    result = (done: true, reply: @[0x47])

  info fmt"MC: in={val:2x} out={result.reply[0]:2x} done={result.done} state={oldState}->{card.state}"

pads[0][0x01] = (val: byte) => controller.send(val)
pads[0][0x81] = (val: byte) => memoryCard.send(val)

type
  Stat = distinct word
  Control = distinct uint16

Stat.bitfield txReadyStarted, bool, 0, 1
Stat.bitfield rxFIFONotEmpty, bool, 1, 1
Stat.bitfield txReadyFinished, bool, 2, 1
Stat.bitfield rxParityError, bool, 3, 1
Stat.bitfield padAck, bool, 7, 1
Stat.bitfield irq, bool, 9, 1
Stat.bitfield timer, int, 11, 21

Control.bitfield txEnable, bool, 0, 1
Control.bitfield joyEnable, bool, 1, 1
Control.bitfield rxForceEnable, bool, 2, 1
Control.bitfield ackIRQ, bool, 4, 1
Control.bitfield reset, bool, 6, 1
Control.bitfield rxInterruptMode, range[0..3], 8, 2
Control.bitfield txInterruptEnabled, bool, 10, 1
Control.bitfield rxInterruptEnabled, bool, 11, 1
Control.bitfield ackInterruptEnabled, bool, 12, 1
Control.bitfield slot, range[0..1], 13, 1

## Hardware registers.
var
  stat {.saved.}: Stat
  mode {.saved.}: uint16 # Ignore mode settings for now
  control {.saved.}: Control
  rxFIFO {.saved.}: Deque[uint8]
  selected {.saved.}: array[2, Option[uint8]]

const
  # Don't support changing baud rate for now
  baudReload = 0x88
  tickRate = 2 * baudReload * cpuClock

proc reset =
  word(stat) = 0
  stat.txReadyStarted = true
  stat.txReadyFinished = true
  mode = 0xd # 8-bit words
  uint16(control) = 0
  rxFIFO.clear
  selected[0] = none(uint8)
  selected[1] = none(uint8)

reset()

proc updateIRQ =
  let
    txReady = stat.txReadyStarted or stat.txReadyFinished
    rxFull =
      case control.rxInterruptMode
      of 0: rxFIFO.len >= 1
      of 1: rxFIFO.len >= 2
      of 2: rxFIFO.len >= 4
      of 3: rxFIFO.len >= 8

    newIRQ =
      (control.txInterruptEnabled and txReady) or
      (control.rxInterruptEnabled and rxFull) or
      (control.ackInterruptEnabled and stat.padAck)

  if control.ackIRQ:
    stat.irq = newIRQ
    control.ackIRQ = false
  else:
    stat.irq = stat.irq or newIRQ

  irqs.set(7, stat.irq)

proc joyTransmit*(val32: word) =
  let val = cast[uint8](val32)
  debug fmt "Transmitting {val:x} on slot {control.slot}"

  if not control.txEnable:
    warn "Transmit when TX disabled"
    return

  if not control.joyEnable:
    warn "Transmit when /JOYn high"

    if control.rxForceEnable:
      rxFIFO.addLast(0xffu8)
      stat.rxFIFONotEmpty = (rxFIFO.len > 0)

    control.rxForceEnable = false
    return

  control.rxForceEnable = false

  var reply = @[0xffu8]
  var ack = false

  if selected[control.slot].isNone():
    # First byte transmitted - select controller
    if pads[control.slot][val] != nil:
      selected[control.slot] = some(val)
      ack = true
      debug fmt "Selected controller {val:x} in slot {control.slot}"
    else:
      debug fmt "No controller {val:x} in slot {control.slot}"
  else:
    # Later byte
    let pad = pads[control.slot][selected[control.slot].get()]
    if pad != nil:
      let result = pad(val)
      reply = result.reply
      ack = not result.done

    if not ack:
      debug fmt "Finished with controller {selected[control.slot]} in slot {control.slot}"
      selected[control.slot] = none(uint8)

  events.after(tickRate*2, "Joypad reply") do():
    debug fmt "Replying with {reply}, ACK = {ack}"

    stat.padAck = ack

    for x in reply: rxFIFO.addLast(x)
    stat.rxFIFONotEmpty = (rxFIFO.len > 0)

    updateIRQ()

    if ack:
      events.after(100*cpuClock, "Joypad ACK end") do():
        trace fmt "Switching off ACK"
        stat.padAck = false
        updateIRQ()

proc joyReceive*: word =
  var bytes = [0xffu8, 0xffu8, 0xffu8, 0xffu8]
  if rxFIFO.len > 0:
    bytes[0] = rxFIFO.popFirst()
    if rxFIFO.len >= 1: bytes[1] = rxFIFO[0]
    if rxFIFO.len >= 2: bytes[2] = rxFIFO[1]
    if rxFIFO.len >= 3: bytes[3] = rxFIFO[2]
  else:
    debug "Receive on empty FIFO"

  result = bytes[0].word + (bytes[1].word shl 8) +
    (bytes[2].word shl 16) + (bytes[3].word shl 24)

  stat.rxFIFONotEmpty = (rxFIFO.len > 0)

  debug fmt "Received {result:x}"

  updateIRQ()

proc joyStat*: word =
  stat.timer = int(baudReload - ((events.now() div cpuClock) mod baudReload))
  trace fmt"Read status as {stat.word:x} at {events.now()}"
  stat.word

proc joyMode*: uint16 =
  trace fmt"Read mode as {mode:x}"

  mode

proc setJoyMode*(val: uint16) =
  trace fmt"Set mode to {mode:x}"

  mode = val

proc joyControl*: uint16 =
  trace fmt"Read control as {control.uint16:x}"

  control.uint16

proc setJoyControl*(val: uint16) =
  trace fmt"Set control to {val:x}"

  uint16(control) = val and 0x3f7f
  if control.reset: reset()
  if not control.txEnable:
    selected[0] = none(uint8)
    selected[1] = none(uint8)
    controller.pos = 0
    memoryCard = MemoryCard(written: true)
    rxFIFO.clear
  updateIRQ()

proc joyBaud*: uint16 =
  baudReload
