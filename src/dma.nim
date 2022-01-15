## The DMA controller.

import machine, utils, irq
import std/[bitops, strformat]

type
  ChannelNumber = range[0..6]

  Channel* = object
    baseAddress: Masked[word]
    blockControl: word
    channelControl: Masked[word]
    read*: proc: word
    write*: proc(value: word)

  SyncMode = enum
    Immediate = 0,
    Blocks = 1,
    LinkedList = 2,
    Reserved = 3

const
  # blockControl fields
  blockSize: BitSlice[uint16, word] = (pos: 0, width: 16)
  blockCount: BitSlice[uint16, word] = (pos: 0, width: 16)

  # channelControl fields
  controlDirection: BitSlice[bool, word] = bit 0 # false=to ram, true=from ram
  controlStep: BitSlice[bool, word] = bit 1 # false=forwards, back=forwards
  controlChopping: BitSlice[bool, word] = bit 8
  controlSyncMode: BitSlice[SyncMode, word] = (pos: 9, width: 2)
  controlDMAWindowSize: BitSlice[int, word] = (pos: 16, width: 3)
  controlCPUWindowSize: BitSlice[int, word] = (pos: 20, width: 3)
  controlStartBusy: BitSlice[bool, word] = bit 24
  controlStartTrigger: BitSlice[bool, word] = bit 28

proc initChannel(n: ChannelNumber): Channel =
  result.baseAddress.mask = 0xfffffff
  result.channelControl.mask = 0x71770703
  result.read = proc(): word =
    echo fmt"Read from unknown DMA channel {n}"
  result.write = proc(value: word) =
    echo fmt"Write {value:x} to unknown DMA channel {n}"

var
  channels*: array[ChannelNumber, Channel]
  control: word = 0x07654321u32
  interrupt: word

for i in ChannelNumber.low..ChannelNumber.high:
  channels[i] = initChannel(i)
# Nocash PSX claims many bits are not writable for OTC
channels[6].channelControl.mask = 0x51000000
channels[6].channelControl.value[controlStep] = true

# Initialise direction just for luck
const
  directions: array[ChannelNumber, bool] = [true, false, true, false, false, false, false]
for i in ChannelNumber.low..ChannelNumber.high:
  channels[i].channelControl.value[controlDirection] = directions[i]

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

  if chan.channelControl[controlStartBusy] or
     chan.channelControl[controlStartTrigger]:
    echo fmt"Starting DMA transfer on channel {n}"
    chan.channelControl.value[controlStartTrigger] = false
    var address = chan.baseAddress and not 3u32

    if n == 6: # OT clear
      let words = chan.blockControl[blockSize]
      echo fmt"Clearing {words:x} words ending at {address:x}"
      address -= words*4 # TODO: words or words-1?
      for i in 0..<int(words):
        let value = if i == 0: 0x00ffffffu32 else: address-4
        addressSpace.rawWrite[:word](address, value)
        address += 4
    else:
      case chan.channelControl[controlSyncMode]
      of LinkedList:
        if chan.channelControl[controlDirection]:
          while address != 0x00ffffffu32:
            let
              header = addressSpace.rawRead[:word](address)
              size = header shr 24
              next = header and 0x00ffffffu32

            for i in 1..<size:
              chan.write(addressSpace.rawRead[:word](address + i*4))

            address = next
        else:
          echo "Linked list to RAM not supported"
      else:
        echo fmt"Sync mode {chan.channelControl[controlSyncMode]} not supported"

    chan.channelControl.value[controlStartBusy] = false
    interrupt[irqFlags[n]] = true
    checkInterrupt()

proc handleDMABaseAddress*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = channels[n].baseAddress
  of Write: channels[n].baseAddress.update value

proc handleDMABlockControl*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = channels[n].blockControl
  of Write: channels[n].blockControl = value

proc handleDMAChannelControl*(n: ChannelNumber, value: var word, kind: IOKind) =
  case kind
  of Read: value = channels[n].channelControl
  of Write:
    channels[n].channelControl.update value
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
