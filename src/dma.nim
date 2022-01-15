## The DMA controller.

import machine, utils, irq
import std/[bitops, strformat]

type
  ChannelNumber = range[0..6]
  BlockControl = distinct word
  ChannelControl = distinct word
  Control = distinct word
  Interrupt = distinct word

  SyncMode {.pure.} = enum
    Immediate = 0,
    Blocks = 1,
    LinkedList = 2,
    Reserved = 3

  Direction {.pure.} = enum
    ToRAM = 0,
    FromRAM = 1

  Step {.pure.} = enum
    Forwards = 0,
    Backwards = 1

  Channel* = object
    baseAddress: word
    blockControl: BlockControl
    channelControl: ChannelControl
    read*: proc: word
    write*: proc(value: word)

proc initChannel(n: ChannelNumber): Channel =
  result.read = proc(): word =
    echo fmt"Read from unknown DMA channel {n}"
  result.write = proc(value: word) =
    echo fmt"Write {value:x} to unknown DMA channel {n}"

var
  channels*: array[ChannelNumber, Channel]
  control = Control(0x07654321)
  interrupt: Interrupt

# BlockControl's fields.
const
  size: BitSlice[word, BlockControl] = (pos: 0, width: 16)
  blocks: BitSlice[word, BlockControl] = (pos: 16, width: 16)

# ChannelControl's fields.
const
  direction: BitSlice[Direction, ChannelControl] = (pos: 0, width: 1)
  step: BitSlice[Step, ChannelControl] = (pos: 1, width: 1)
  chopping {.used.}: BitSlice[bool, ChannelControl] = (pos: 8, width: 1)
  syncMode: BitSlice[SyncMode, ChannelControl] = (pos: 9, width: 2)
  dmaWindowSize {.used.}: BitSlice[int, ChannelControl] = (pos: 16, width: 3)
  cpuWindowSize {.used.}: BitSlice[int, ChannelControl] = (pos: 20, width: 3)
  startBusy: BitSlice[bool, ChannelControl] = (pos: 24, width: 1)
  startTrigger: BitSlice[bool, ChannelControl] = (pos: 28, width: 1)

# Interrupt's fields.
const
  force: BitSlice[bool, Interrupt] = (pos: 15, width: 1)
  enableIRQs: BitSlice[word, Interrupt] = (pos: 16, width: 7)
  enableIRQ {.used.}: array[ChannelNumber, auto] = enableIRQs.bits
  masterEnable: BitSlice[bool, Interrupt] = (pos: 23, width: 1)
  flagsIRQs: BitSlice[word, Interrupt] = (pos: 24, width: 7)
  flagsIRQ: array[ChannelNumber, auto] = flagsIRQs.bits
  master: BitSlice[bool, Interrupt] = (pos: 31, width: 1)

# Control's fields.
const
  enableChannel: array[ChannelNumber, auto] =
    bits[bool, Control](pos=3, width=7, stride=4)

const
  # Which bits of the channel control mask are writable?
  channelControlMask: array[ChannelNumber, word] = block:
    var result: array[ChannelNumber, word]
    for x in result.mitems: x = 0x71770703
    # Nocash PSX claims many bits are not writable for OTC
    result[6] = 0x51000000
    result

  initialDirections: array[ChannelNumber, Direction] =
    [FromRAM, ToRAM, FromRAM, ToRAM, ToRAM, ToRAM, ToRAM]

# Initialise channels
for i in ChannelNumber.low..ChannelNumber.high:
  channels[i] = initChannel(i)
  channels[i].channelControl[direction] = initialDirections[i]
channels[6].channelControl[step] = Backwards

proc checkInterrupt =
  ## Set bit 31 (irqMaster) if an interrupt is ready.
  ## If bit 31 is freshly triggered, signal IRQ 3.
  let
    oldInterrupt = interrupt[master]
    newInterrupt =
      interrupt[force] or
      (interrupt[masterEnable] and
       ((interrupt[enableIRQs] and interrupt[flagsIRQs]) != 0))
  interrupt[master] = newInterrupt
  if newInterrupt and not oldInterrupt:
    irqs.signal(3)

proc transfer(chan: Channel, startingAddress: word, size: word): word =
  ## Do a basic DMA transfer.

  var address = startingAddress
  for i in 0..<size:
    case chan.channelControl[direction]
    of ToRAM: addressSpace.rawWrite[:word](address, chan.read())
    of FromRAM: chan.write(addressSpace.rawRead[:word](address))

    case chan.channelControl[step]
    of Forwards: address += 4
    of Backwards: address -= 4

  return address # TODO: is it this value or one step backwards that's written to MADR?

proc checkChannel(n: ChannelNumber, chan: var Channel) =
  ## Check if the given channel should do a DMA right now.

  # TODO run at a realistic clock rate
  if control[enableChannel[n]] and
     (chan.channelControl[startBusy] or chan.channelControl[startTrigger]):
    echo fmt"Starting DMA transfer on channel {n}"
    chan.channelControl[startTrigger] = false
    var address = chan.baseAddress and not 3u32

    if n == 6: # OT clear
      let words = chan.blockControl[size]
      echo fmt"Clearing {words:x} words ending at {address:x}"
      address -= (words-1)*4 # TODO: words or words-1?
      for i in 0..<int(words):
        let value = if i == 0: 0x00ffffffu32 else: address-4
        addressSpace.rawWrite[:word](address, value)
        address += 4
    else:
      case chan.channelControl[syncMode]
      of LinkedList:
        if chan.channelControl[direction] == FromRAM:
          while address != 0x00ff_ffff:
            let
              header = addressSpace.rawRead[:word](address)
              size = header shr 24
              next = header and 0x00ff_ffff

            for i in 1..size:
              chan.write(addressSpace.rawRead[:word](address + i*4))

            address = next

          chan.baseAddress = 0x00ff_ffff
        else:
          echo "Linked list to RAM not supported (except for DMA channel 6)"
      of Immediate:
        let endAddress = transfer(chan, address, chan.blockControl[size])
        if chan.channelControl[chopping]:
          chan.baseAddress = endAddress
          chan.blockControl[size] = 0
      of Blocks:
        let endAddress = transfer(chan, address, chan.blockControl[size] * chan.blockControl[blocks])
        chan.baseAddress = endAddress
        chan.blockControl[blocks] = 0
      else:
        echo fmt"Sync mode {chan.channelControl[syncMode]} not supported"

    chan.channelControl[startBusy] = false
    interrupt[flagsIRQ[n]] = true
    checkInterrupt()

proc handleDMABaseAddress*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = channels[n].baseAddress
  of Write: channels[n].baseAddress = value and 0x00ff_ffff

proc handleDMABlockControl*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = word(channels[n].blockControl)
  of Write: word(channels[n].blockControl) = value

proc handleDMAChannelControl*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = word(channels[n].channelControl)
  of Write:
    word(channels[n].channelControl) = value and channelControlMask[n]
    checkChannel n, channels[n]

proc handleDMAControl*(value: var word, kind: IOKind) =
  case kind
  of Read: value = word(control)
  of Write:
    word(control) = value
    for n, chan in channels.mpairs:
      checkChannel n, chan

proc handleDMAInterrupt*(value: var word, kind: IOKind) =
  case kind
  of Read: value = word(interrupt)
  of Write:
    # Update R/W bits
    let writable = 0x00ff803fu32
    word(interrupt).clearMask writable
    word(interrupt).setMask (writable and value)

    # Bits 24-30 are reset to 0 by writing a 1 there
    let ack = 0x7f000000u32
    word(interrupt).clearMask (ack and value)

    checkInterrupt()