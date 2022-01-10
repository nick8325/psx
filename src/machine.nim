## The PSX itself.

import basics, utils, memory, eventqueue
import std/bitops

var
  events*: EventQueue = initEventQueue() ## The queue of events to happen.
  addressSpace*: Memory ## The PSX address space.

import cpu as r3000

var
  cpu*: CPU = initCPU ## The main processor

# Interrupt mask
var
  irqs* =
    (stat: Masked[word](mask: 0x7ff),
     mask: Masked[word](mask: 0x7ff))

proc setCPUIRQ() =
  ## Update the processor IRQ flags from irq.stat.
  cpu.setIRQ((irqs.stat and irqs.mask) != 0)

proc signalIRQ*(irq: range[0..10]) =
  ## Activate a given IRQ.

  setBit(irqs.stat.value, int(irq))
  setCPUIRQ()

proc clearIRQ*(irq: range[0..10]) =
  ## Clear a given IRQ.

  clearBit(irqs.stat.value, int(irq))
  setCPUIRQ()

# VBLANK IRQ
events.every(clockRate div refreshRate) do: signalIRQ(0)

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
# TODO this is the initial value of GPUSTAT
addressSpace.rawWrite[:word](0x1f801814u32, 0x14802000u32)

# I/O handlers
proc ioHandler8(address: word, value: var uint8, kind: IOKind): bool =
  return false

proc ioHandler16(address: word, value: var uint16, kind: IOKind): bool =
  return false

proc ioHandler32(address: word, value: var uint32, kind: IOKind): bool =
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
    case kind
    of Read: value = irqs.stat
    of Write: irqs.stat.update (irqs.stat and value)
    return true
  of 0x1f801074u32:
    # Interrupt mask
    case kind
    of Read: value = irqs.mask
    of Write: irqs.mask.update value
    return true
  else:
    return false

addressSpace.ioHandler8 = ioHandler8
addressSpace.ioHandler16 = ioHandler16
addressSpace.ioHandler32 = ioHandler32

proc runSystem*() =
  ## Run the system.

  while true:
    while events.nextTime >= cpuClock:
      cpu.step
      events.fastForward(cpuClock)
    if not events.runNext: break

runSystem()
