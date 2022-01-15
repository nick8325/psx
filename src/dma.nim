## The DMA controller.

import machine, utils, irq
import std/[bitops, strformat]

type
  ChannelNumber = range[0..6]
  BlockControl = distinct word
  ChannelControl = distinct word

  Channel* = object
    baseAddress: word
    blockControl: BlockControl
    channelControl: ChannelControl
    read*: proc: word
    write*: proc(value: word)

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

BlockControl.bitfield size, uint16, 0, 16
BlockControl.bitfield blocks, uint16, 16, 16
ChannelControl.bitfield direction, Direction, 0, 1
ChannelControl.bitfield step, Step, 0, 1
ChannelControl.bitfield chopping, bool, 8, 1
ChannelControl.bitfield syncMode, SyncMode, 9, 2
ChannelControl.bitfield dmaWindowSize, int, 16, 3
ChannelControl.bitfield cpuWindowSize, int, 20, 3
ChannelControl.bitField startBusy, bool, 24, 1
ChannelControl.bitField startTrigger, bool, 28, 1

proc initChannel(n: ChannelNumber): Channel =
  result.read = proc(): word =
    echo fmt"Read from unknown DMA channel {n}"
  result.write = proc(value: word) =
    echo fmt"Write {value:x} to unknown DMA channel {n}"

var
  channels*: array[ChannelNumber, Channel]
  control: word = 0x07654321u32
  interrupt: word

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
  channels[i].channelControl.direction = initialDirections[i]
channels[6].channelControl.step = Backwards

const
  irqForce: BitSlice[bool, word] = bit 15
  irqEnableAll: BitSlice[word, word] = (pos: 16, width: 7)
  irqMasterEnable: BitSlice[bool, word] = bit 23
  irqFlags: array[ChannelNumber, BitSlice[bool, word]] =
    [bit 24, bit 25, bit 26, bit 27, bit 28, bit 29, bit 30]
  irqFlagsAll: BitSlice[word, word] = (pos: 24, width: 7)
  irqMaster: BitSlice[bool, word] = bit 31

proc checkInterrupt =
  ## Set bit 31 (irqMaster) if an interrupt is ready.
  ## If bit 31 is freshly triggered, signal IRQ 3.
  let
    oldInterrupt = interrupt[irqMaster]
    newInterrupt =
      interrupt[irqForce] or
      (interrupt[irqMasterEnable] and
       ((interrupt[irqEnableAll] and interrupt[irqFlagsAll]) != 0))
  interrupt[irqMaster] = newInterrupt
  if newInterrupt and not oldInterrupt:
    irqs.signal(3)

proc checkChannel(n: ChannelNumber, chan: var Channel) =
  ## Check if the given channel should do a DMA right now.

  if chan.channelControl.startBusy or
     chan.channelControl.startTrigger:
    echo fmt"Starting DMA transfer on channel {n}"
    chan.channelControl.startTrigger = false
    var address = chan.baseAddress and not 3u32

    if n == 6: # OT clear
      let words = chan.blockControl.size
      echo fmt"Clearing {words:x} words ending at {address:x}"
      address -= (words-1)*4 # TODO: words or words-1?
      for i in 0..<int(words):
        let value = if i == 0: 0x00ffffffu32 else: address-4
        addressSpace.rawWrite[:word](address, value)
        address += 4
    else:
      case chan.channelControl.syncMode
      of LinkedList:
        case chan.channelControl.direction
        of FromRAM:
          while address != 0x00ffffffu32:
            let
              header = addressSpace.rawRead[:word](address)
              size = header shr 24
              next = header and 0x00ffffffu32

            for i in 1..size:
              chan.write(addressSpace.rawRead[:word](address + i*4))

            address = next
        of ToRAM:
          echo "Linked list to RAM not supported (except for DMA channel 6)"
      else:
        echo fmt"Sync mode {chan.channelControl.syncMode} not supported"

    chan.channelControl.startBusy = false
    interrupt[irqFlags[n]] = true
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
    echo fmt"channel {n} control {value:x}"
    checkChannel n, channels[n]

proc handleDMAControl*(value: var word, kind: IOKind) =
  case kind
  of Read: value = control
  of Write:
    control = value
    for n, chan in channels.mpairs:
      checkChannel n, chan

proc handleDMAInterrupt*(value: var word, kind: IOKind) =
  case kind
  of Read: value = interrupt
  of Write:
    # Update R/W bits
    let writable = 0x00ff803fu32
    interrupt.clearMask writable
    interrupt.setMask (writable and value)

    # Bits 24-30 are reset to 0 by writing a 1 there
    let ack = 0x7f000000u32
    interrupt.clearMask (ack and value)

    checkInterrupt()
