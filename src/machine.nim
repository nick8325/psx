## Hooking up the PSX itself.

import basics, memory, eventqueue, irq, dma, gpu, cpu, timer, utils
from cdrom import nil
import std/strformat

const loggerComponent = logMachine

var
  bios {.align: 4096.}: array[0x80000, byte]
  ram {.align: 4096.}: array[0x200000, byte]
  scratchpad {.align: 4096.}: array[0x1000, byte]
  expansion {.align: 4096.}: array[0x1000, byte]
  ioPorts {.align: 4096.}: array[0x1000, byte]
  cacheControl {.align: 4096.}: array[0x1000, byte]

# Initialise expansion to -1, and read in BIOS
for x in expansion.mitems: x = 0xff
bios[0 ..< 0x80000] = toOpenArrayByte(static (staticRead "../roms/scph1001.bin"), 0, 0x7ffff)

# Map in all the memory
#                      region        address        writable  io
addressSpace.mapRegion(ram,          0x00000000u32, true,     false)
addressSpace.mapRegion(ram,          0x00200000u32, true,     false)
addressSpace.mapRegion(ram,          0x00400000u32, true,     false)
addressSpace.mapRegion(ram,          0x00600000u32, true,     false)
addressSpace.mapRegion(expansion,    0x1f000000u32, true,     true)
addressSpace.mapRegion(scratchpad,   0x1f800000u32, true,     false)
addressSpace.mapRegion(ioPorts,      0x1f801000u32, true,     true)
addressSpace.mapRegion(expansion,    0x1f802000u32, true,     true)
addressSpace.mapRegion(bios,         0x1fc00000u32, false,    false)
# TODO: allow enabling/disabling scratchpad
addressSpace.mapRegion(cacheControl, 0xfffe0000u32, true,     false)

# Set up I/O space
addressSpace.rawWrite[:word](0x1f801000u32, 0x1f000000u32)
addressSpace.rawWrite[:word](0x1f801004u32, 0x1f802000u32)
addressSpace.rawWrite[:word](0x1f801008u32, 0x0013243fu32)
addressSpace.rawWrite[:word](0x1f80100cu32, 0x00003022u32)
addressSpace.rawWrite[:word](0x1f801010u32, 0x0013243fu32)
addressSpace.rawWrite[:word](0x1f801014u32, 0x200931e1u32)
addressSpace.rawWrite[:word](0x1f801018u32, 0x00020843u32)
addressSpace.rawWrite[:word](0x1f80101cu32, 0x00070777u32)
addressSpace.rawWrite[:word](0x1f801020u32, 0x00031125u32)
addressSpace.rawWrite[:word](0x1f801060u32, 0x00000b88u32)
addressSpace.rawWrite[:word](0xfffe0130u32, 0x0001e988u32)

# Patch the BIOS to enable TTY output (taken from DuckStation).
addressSpace.forcedRawWrite[:word](0x1FC06F0Cu32, 0x24010001u32)
addressSpace.forcedRawWrite[:word](0x1FC06F14u32, 0xAF81A9C0u32)

# I/O handlers
proc handleIO8(address: word, value: var uint8, kind: IOKind): bool =
  case address
  of 0x1f801800:
    # CD-ROM
    case kind
    of Read: value = cdrom.readStatus()
    of Write: cdrom.writeStatus(value)
    return true
  of 0x1f801801..0x1f801803:
    # CD-ROM
    case kind
    of Read: value = cdrom.readRegister(address mod 4)
    of Write: cdrom.writeRegister(address mod 4, value)
    return true
  of 0x1f802041:
    # 7-segment display
    if kind == Write:
      debug fmt "POST {value}"
      return true
    else:
      return false
  of 0x1f802080:
    # PCSX-Redux MIPS API
    if kind == Write:
      stdout.write(value.char)
      stdout.flushFile
      return true
    else:
      return false
  of 0x1f802081:
    # PCSX-Redux MIPS API
    if kind == Write:
      warn "Debug break"
      return true
    else:
      return false
  of 0x1f802020..0x1f80202f:
    if address == 0x1f802021 and kind == Read:
      # UART status
      value = 4
      return true
    elif address == 0x1f802023 and kind == Write:
      # UART TX
      stdout.write(value.char)
      stdout.flushFile
      return true
    elif kind == Write:
      # Unknown UART control - ignore
      return true
    # Unknown reads might be important
    return false
  else:
    return false

proc handleIO16(address: word, value: var uint16, kind: IOKind): bool =
  if address == 0x1f801802 and kind == Read:
    # CD-ROM DATA FIFO
    value = cdrom.readData16()
    return true
  elif address == 0x1f802082 and kind == Write:
    # PCSX-Redux MIPS API
    fatal fmt"Exit requested with code {value}"
    quit 1

  return false

proc handleIO32(address: word, value: var uint32, kind: IOKind): bool =
  case address
  of 0x1f801000u32 .. 0x1f801024u32, 0x1f801060:
    # Memory control (TODO)
    return true
  of 0x1f801c00u32 .. 0x1f801ffcu32:
    # SPU (TODO)
    return true
  of 0x1f801100..0x1f801128:
    # Timers
    let n = ((address div 16) mod 16).int
    if n < timers.low or n > timers.high: return false
    case address mod 16
    of 0:
      case kind
      of Read: value = timers[n].counter()
      of Write: timers[n].setCounter(value)
    of 4:
      case kind
      of Read: value = timers[n].mode()
      of Write: timers[n].setMode(value)
    of 8:
      case kind
      of Read: value = timers[n].target()
      of Write: timers[n].setTarget(value)
    else:
      return false

    return true
  of 0x1f801070u32:
    # Interrupt status
    irqs.handleStatus value, kind
    return true
  of 0x1f801074u32:
    # Interrupt mask
    irqs.handleMask value, kind
    return true
  of 0x1f801080u32..0x1f8010e8u32:
    # DMA channel
    let channel = (address-0x1f801080u32) div 16
    case address mod 16
    of 0:
      handleDMABaseAddress channel, value, kind
      return true
    of 4:
      handleDMABlockControl channel, value, kind
      return true
    of 8:
      handleDMAChannelControl channel, value, kind
      return true
    else: return false
  of 0x1f8010f0u32:
    # DMA control register
    handleDMAControl value, kind
    return true
  of 0x1f8010f4u32:
    # DMA interrupt register
    handleDMAInterrupt value, kind
    return true
  of 0x1f801810u32:
    # GPU
    case kind
    of Read: value = gpuread()
    of Write: gp0(value)
    return true
  of 0x1f801814u32:
    # GPU
    case kind
    of Read: value = gpustat()
    of Write: gp1(value)
    return true
  of 0x1f802080:
    # PCSX-Redux MIPS API
    if kind == Read:
      value = 0x58534350
      return true
    else:
      return false
  of 0x1f802084:
    # PCSX-Redux MIPS API
    if kind == Write:
      var msg: string
      var address = value
      var c: uint8 = addressSpace.rawRead[:uint8](address)
      while c != 0:
        msg &= c.char
        address += 1
        c = addressSpace.rawRead[:uint8](address)
      return true
    else:
      return false
  of 0x1F801040:
    # JOY_RX_DATA
    if kind == Read:
      value = 0xffffffffu32
      return true
  of 0x1F801044:
    # JOY_STAT
    if kind == Read:
      value = 3
      return true
  else:
    return false

addressSpace.ioHandler8 = handleIO8
addressSpace.ioHandler16 = handleIO16
addressSpace.ioHandler32 = handleIO32

# Set up DMA handlers.
dma.channels[2].read = gpuReadDMA
dma.channels[2].write = gpuWriteDMA

proc runSystem*(clocks: int64) =
  ## Run the system.

  let stop = events.now + clocks
  while events.now < stop:
    cpu.cpu.step
    events.fastForward(cpuClock)

proc dumpRAM*(filename: string) =
  ## Dump the contents of RAM to a file.

  writeFile(filename, ram)
