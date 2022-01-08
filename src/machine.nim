## The PSX itself.

import common
import memory

var
  addressSpace*: Memory ## The PSX address space.
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
var io: bool
addressSpace.rawWrite[:word](0x1f801000u32, 0x1f000000u32, io)
addressSpace.rawWrite[:word](0x1f801004u32, 0x1f802000u32, io)
addressSpace.rawWrite[:word](0x1f801008u32, 0x0013243fu32, io)
addressSpace.rawWrite[:word](0x1f80100cu32, 0x00003022u32, io)
addressSpace.rawWrite[:word](0x1f801010u32, 0x0013243fu32, io)
addressSpace.rawWrite[:word](0x1f801014u32, 0x200931e1u32, io)
addressSpace.rawWrite[:word](0x1f801018u32, 0x00020843u32, io)
addressSpace.rawWrite[:word](0x1f80101cu32, 0x00070777u32, io)
addressSpace.rawWrite[:word](0x1f801020u32, 0x00031125u32, io)
addressSpace.rawWrite[:word](0x1f801060u32, 0x00000b88u32, io)
addressSpace.rawWrite[:word](0xfffe0130u32, 0x0001e988u32, io)
