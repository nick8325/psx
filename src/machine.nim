## Hooking up the PSX itself.

import basics, memory, eventqueue, irq, dma, gpu, cpu

var
  bios {.align: 4096.}: array[0x80000, byte]
  ram {.align: 4096.}: array[0x200000, byte]
  scratchpad {.align: 4096.}: array[0x1000, byte]
  expansion {.align: 4096.}: array[0x1000, byte]
  ioPorts {.align: 4096.}: array[0x1000, byte]
  cacheControl {.align: 4096.}: array[0x1000, byte]

# Initialise expansion to -1, and read in BIOS
for x in expansion.mitems: x = 0xff
bios[0 ..< 0x80000] = toOpenArrayByte(static (staticRead "../SCPH1002.bin"), 0, 0x7ffff)

# Map in all the memory
#                      region        address        writable  io
addressSpace.mapRegion(ram,          0x00000000u32, true,     false)
addressSpace.mapRegion(ram,          0x00200000u32, true,     false)
addressSpace.mapRegion(ram,          0x00400000u32, true,     false)
addressSpace.mapRegion(ram,          0x00600000u32, true,     false)
addressSpace.mapRegion(expansion,    0x1f000000u32, false,    false)
addressSpace.mapRegion(scratchpad,   0x1f800000u32, true,     false)
addressSpace.mapRegion(ioPorts,      0x1f801000u32, true,     true)
addressSpace.mapRegion(expansion,    0x1f802000u32, false,    false)
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

# I/O handlers
proc handleIO8(address: word, value: var uint8, kind: IOKind): bool =
  return false

proc handleIO16(address: word, value: var uint16, kind: IOKind): bool =
  return false

proc handleIO32(address: word, value: var uint32, kind: IOKind): bool =
  case address
  of 0x1f801000u32 .. 0x1f801024u32, 0x1f801060:
    # Memory control (TODO)
    return true
  of 0x1f801c00u32 .. 0x1f801ffcu32:
    # SPU (TODO)
    return true
  of 0x1f801100u32 .. 0x1f801128u32:
    # Timers (TODO)
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
    case kind
    of Read: value = gpuread()
    of Write: gp0(value)
    return true
  of 0x1f801814u32:
    case kind
    of Read: value = gpustat()
    of Write: gp1(value)
    return true
  else:
    return false

addressSpace.ioHandler8 = handleIO8
addressSpace.ioHandler16 = handleIO16
addressSpace.ioHandler32 = handleIO32

# Set up DMA handlers.
dma.channels[2].read = gpuReadDMA
dma.channels[2].write = gpuWriteDMA

proc runSystem*(hz: int) =
  ## Run the system.

  let stop = events.time + clockRate div hz.uint64
  while true:
    while events.nextTime >= cpuClock:
      cpu.cpu.step
      events.fastForward(cpuClock)
    if not events.runNext: break
    if events.time >= stop: break
