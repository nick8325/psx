## Hooking up the PSX itself.

import basics, memory, eventqueue, irq, dma, gpu, cpu, timer, joy, utils
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
bios[0 ..< 0x80000] = toOpenArrayByte(static (staticRead "../roms/scph5502.bin"), 0, 0x7ffff)

# Map in all the memory
#                      region        address        writable  io     region
addressSpace.mapRegion(ram,          0x00000000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00200000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00400000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00600000u32, true,     false, RAM)
addressSpace.mapRegion(expansion,    0x1f000000u32, true,     true,  Expansion1)
addressSpace.mapRegion(scratchpad,   0x1f800000u32, true,     false, Scratchpad)
addressSpace.mapRegion(ioPorts,      0x1f801000u32, true,     true,  GPU) # WRONG!
addressSpace.mapRegion(expansion,    0x1f802000u32, true,     true,  Expansion2)
addressSpace.mapRegion(bios,         0x1fc00000u32, false,    false, BIOS)
# TODO: allow enabling/disabling scratchpad
addressSpace.mapRegion(cacheControl, 0xfffe0000u32, true,     false, ScratchPad)

# Expansion 1/2 base address
addressSpace.rawWrite[:word](0x1f801000u32, 0x1f000000u32)
addressSpace.rawWrite[:word](0x1f801004u32, 0x1f802000u32)

# Patch the BIOS to enable TTY output (taken from DuckStation).
addressSpace.forcedRawWrite[:word](0x1FC06F0Cu32, 0x24010001u32)
addressSpace.forcedRawWrite[:word](0x1FC06F14u32, 0xAF81A9C0u32)

# RAM timings
type
  RegionDelay = distinct word
  CommonDelay = distinct word

RegionDelay.bitfield accessTime, int, 4, 4
RegionDelay.bitfield useCOM0, bool, 8, 1
RegionDelay.bitfield useCOM1, bool, 9, 1
RegionDelay.bitfield useCOM2, bool, 10, 1
RegionDelay.bitfield useCOM3, bool, 11, 1
RegionDelay.bitfield sixteenBits, bool, 12, 1
RegionDelay.bitfield windowSize, int, 16, 5

CommonDelay.bitfield com0, int, 0, 4
CommonDelay.bitfield com1, int, 4, 4
CommonDelay.bitfield com2, int, 8, 4
CommonDelay.bitfield com3, int, 12, 4

proc delay(region: RegionDelay, common: CommonDelay, size: int): int =
  ## Calculate how many clock cycles it takes to read a given region.

  # Taken from Nocash PSX, "Memory control".
  let
    com0 = if region.useCOM0: common.com0 - 1 else: 0
    com2 = if region.useCOM2: common.com2 else: 0
    min = if region.useCOM3: common.com3 else: 0

    shared = com0 + com2 + region.accessTime + 2
    first = max(min+6, shared + int(com0 + com2 < 6))
    seq = max(min+2, shared)

    accesses = divRoundUp(size, if region.sixteenBits: 2 else: 1)

  cpuClock * (first + (accesses-1) * seq)

var
  regionDelays: array[6, RegionDelay]
  regions: array[6, MemoryRegion] =
    [Expansion1, Expansion3, BIOS, SPU, CDROM, Expansion2]
  commonDelay: CommonDelay

proc updateDelays =
  ## Update the memory delay information.
  for i, regionDelay in regionDelays:
    let region = regions[i]
    memoryDelay8[region] = delay(regionDelay, commonDelay, 1)
    memoryDelay16[region] = delay(regionDelay, commonDelay, 2)
    memoryDelay32[region] = delay(regionDelay, commonDelay, 4)

# I/O handlers
proc handleIO8(address: word, value: var uint8, kind: IOKind, region: var MemoryRegion): bool =
  case address
  of 0x1f801800:
    # CD-ROM
    region = CDROM
    case kind
    of Read: value = cdrom.readStatus()
    of Write: cdrom.writeStatus(value)
    return true
  of 0x1f801801..0x1f801803:
    # CD-ROM
    region = CDROM
    case kind
    of Read: value = cdrom.readRegister(address mod 4)
    of Write: cdrom.writeRegister(address mod 4, value)
    return true
  of 0x1f802041:
    # 7-segment display
    region = Expansion2
    if kind == Write:
      debug fmt "POST {value}"
      return true
    else:
      return false
  of 0x1f802080:
    # PCSX-Redux MIPS API
    region = BuiltIn
    if kind == Write:
      stdout.write(value.char)
      stdout.flushFile
      return true
    else:
      return false
  of 0x1f802081:
    # PCSX-Redux MIPS API
    region = BuiltIn
    if kind == Write:
      warn "Debug break"
      return true
    else:
      return false
  of 0x1f802020..0x1f80202f:
    region = Serial
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
  of 0x1f802066:
    region = BuiltIn
    if kind == Read:
      # Nocash halt until interrupt
      return true
    return false
  else:
    return false

proc handleIO16(address: word, value: var uint16, kind: IOKind, region: var MemoryRegion): bool =
  case address
  of 0x1f801802:
    if kind == Read:
      # CD-ROM DATA FIFO
      region = CDROM
      value = cdrom.readData16()
      return true
  of 0x1f802082:
    if kind == Write:
      # PCSX-Redux MIPS API
      region = BuiltIn
      fatal fmt"Exit requested with code {value}"
      quit 1
  of 0x1f801048:
    # JOY_MODE
    region = Serial
    case kind
    of Read: value = joyMode()
    of Write: setJoyMode(value)
    return true
  of 0x1f80104a:
    # JOY_CTRL
    region = Serial
    case kind
    of Read: value = joyControl()
    of Write: setJoyControl(value)
    return true
  of 0x1f80104e:
    # JOY_BAUD
    region = Serial
    if kind == Read:
      value = joyBaud()
      return true
  else:
    return false

proc handleIO32(address: word, value: var uint32, kind: IOKind, region: var MemoryRegion): bool =
  case address
  of 0x1f801000u32..0x1f801004u32:
    # Expansion base address
    region = BuiltIn
    case kind
    of Read: return true
    of Write: return false
  of 0x1f801060u32:
    # RAM Size
    region = BuiltIn
    return true
  of 0x1f801008u32..0x1f80101cu32:
    # Memory region delay
    region = BuiltIn
    let i = (address - 0x1f801008) div 4
    case kind
    of Read: value = regionDelays[i].word
    of Write:
      word(regionDelays[i]) = value
      updateDelays()
    return true
  of 0x1f801020u32:
    # Common delay
    region = BuiltIn
    case kind
    of Read: value = commonDelay.word
    of Write:
      word(commonDelay) = value
      updateDelays()
  of 0x1f801c00u32 .. 0x1f801ffcu32:
    # SPU (TODO)
    region = SPU
    return true
  of 0x1f801100..0x1f801128:
    # Timers
    region = BuiltIn
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
    region = BuiltIn
    irqs.handleStatus value, kind
    return true
  of 0x1f801074u32:
    # Interrupt mask
    region = BuiltIn
    irqs.handleMask value, kind
    return true
  of 0x1f801080u32..0x1f8010e8u32:
    # DMA channel
    region = BuiltIn
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
    region = BuiltIn
    handleDMAControl value, kind
    return true
  of 0x1f8010f4u32:
    # DMA interrupt register
    region = BuiltIn
    handleDMAInterrupt value, kind
    return true
  of 0x1f801810u32:
    # GPU
    region = GPU
    case kind
    of Read: value = gpuread()
    of Write: gp0(value)
    return true
  of 0x1f801814u32:
    # GPU
    region = GPU
    case kind
    of Read: value = gpustat()
    of Write: gp1(value)
    return true
  of 0x1f802080:
    # PCSX-Redux MIPS API
    region = BuiltIn
    if kind == Read:
      value = 0x58534350
      return true
    else:
      return false
  of 0x1f802084:
    # PCSX-Redux MIPS API
    region = BuiltIn
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
    # JOY_DATA
    region = Serial
    case kind
    of Read: value = joyReceive()
    of Write: joyTransmit(uint8(value and 0xff))
    return true
  of 0x1F801044:
    # JOY_STAT
    region = Serial
    if kind == Read:
      value = joyStat()
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
    let now = events.now
    var time = now
    cpu.cpu.step(time)
    events.fastForward(time - now)

proc dumpRAM*(filename: string) =
  ## Dump the contents of RAM to a file.

  writeFile(filename, ram)
