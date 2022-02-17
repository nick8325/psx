## Joypad/memory card bus.

import basics, utils, irq, eventqueue
import std/[strformat, math, options, deques]

const loggerComponent = logJoy

type
  ## A device connected to the bus, i.e. a joypad or memory card.
  Joypad* = proc(val: uint8): tuple[done: bool, reply: uint8]

var
  pos: range[0..3] = 0

var
  pads*: array[2, array[256, Joypad]]

proc controller(val: uint8): tuple[done: bool, reply: uint8] =
  case pos
  of 0:
    pos.inc
    if val != 0x42:
      warn fmt "Unknown command {val:x}"
    return (done: false, reply: 0x41u8)
  of 1:
    pos.inc
    return (done: false, reply: 0x5au8)
  of 2:
    pos.inc
    return (done: false, reply: 0x40u8)
  of 3:
    pos = 0
    return (done: true, reply: 0u8)

pads[0][0x01] = controller

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
  stat: Stat
  mode: uint16 # Ignore mode settings for now
  control: Control
  rxFIFO: Deque[uint8]
  selected: array[2, Option[uint8]]

const
  # Don't support changing baud rate for now
  baudReload = 0x44
  tickRate = baudReload * cpuClock

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

proc joyTransmit*(val: byte) =
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

  var reply = 0xffu8
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

  events.after(tickRate*16, "Joypad reply") do():
    debug fmt "Replying with {reply}, ACK = {ack}"

    stat.padAck = ack

    rxFIFO.addLast(reply)
    stat.rxFIFONotEmpty = (rxFIFO.len > 0)

    updateIRQ()

    if ack:
      events.after(tickRate*8, "Joypad ACK end") do():
        trace fmt "Switching off ACK"
        stat.padAck = false
        updateIRQ()

proc joyReceive*: word =
  var bytes = [0xffu8, 0xffu8, 0xffu8, 0xffu8]
  if rxFIFO.len > 0:
    bytes[0] = rxFIFO.popFirst()
    if rxFIFO.len > 1: bytes[1] = rxFIFO[1]
    if rxFIFO.len > 2: bytes[2] = rxFIFO[2]
    if rxFIFO.len > 3: bytes[3] = rxFIFO[3]
  else:
    trace "Receive on empty FIFO"

  result = bytes[0].word + (bytes[1].word shl 8) +
    (bytes[2].word shl 16) + (bytes[3].word shl 24)

  stat.rxFIFONotEmpty = (rxFIFO.len > 0)

  trace fmt "Received {result:x}"

  updateIRQ()

proc joyStat*: word =
  stat.timer = int(baudReload - ((events.now() div tickRate) mod baudReload))
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
  updateIRQ()

proc joyBaud*: uint16 =
  baudReload
