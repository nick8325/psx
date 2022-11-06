## Hooking up the PSX itself.

import basics, memory, eventqueue, irq, dma, gpu, cpu, timer, joy, utils, savestates, cdrom
import std/[strformat, sugar, bitops]

const loggerComponent = logMachine
logMachine.level = lvlDebug

var
  bios {.align: 4096, saved.}: array[0x80000, byte]
  ram {.align: 4096, saved.}: array[0x200000, byte]
  scratchpad {.align: 4096, saved.}: array[0x1000, byte]
  expansion {.align: 4096, saved.}: array[0x1000, byte]
  ioPorts {.align: 4096, saved.}: array[0x1000, byte]
  cacheControl {.align: 4096, saved.}: array[0x1000, byte]

# Initialise expansion to -1, and read in BIOS
for x in expansion.mitems: x = 0xff
bios[0 ..< 0x80000] = toOpenArrayByte(readFile "rom.bin", 0, 0x7ffff)

# Map in all the memory
#                      region        address        writable  io     region
addressSpace.mapRegion(ram,          0x00000000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00200000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00400000u32, true,     false, RAM)
addressSpace.mapRegion(ram,          0x00600000u32, true,     false, RAM)
addressSpace.mapRegion(expansion,    0x1f000000u32, true,     true,  Expansion1)
addressSpace.mapRegion(scratchpad,   0x1f800000u32, true,     false, Scratchpad)
addressSpace.mapRegion(ioPorts,      0x1f801000u32, true,     true,  BuiltIn) # will be overridden in individual handlers
addressSpace.mapRegion(expansion,    0x1f802000u32, true,     true,  Expansion2)
addressSpace.mapRegion(bios,         0x1fc00000u32, false,    false, BIOS)
# TODO: allow enabling/disabling scratchpad
addressSpace.mapRegion(cacheControl, 0xfffe0000u32, true,     false, BuiltIn)

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
  regionDelays {.saved.}: array[6, RegionDelay]
  commonDelay {.saved.}: CommonDelay
const
  regions: array[6, MemoryRegion] =
    [Expansion1, Expansion3, BIOS, SPU, CDROM, Expansion2]

proc updateDelays =
  ## Update the memory delay information.
  for i, regionDelay in regionDelays:
    let region = regions[i]
    memoryDelay8[region] = delay(regionDelay, commonDelay, 1)
    memoryDelay16[region] = delay(regionDelay, commonDelay, 2)
    memoryDelay32[region] = delay(regionDelay, commonDelay, 4)

# I/O handlers

# CD-ROM
addressSpace.io8 0x1f801800, CDROM, cdrom.readStatus, cdrom.writeStatus

for i in 1..3:
  capture i:
    addressSpace.io8 (0x1f801800 + i.word), CDROM,
      () => cdrom.readRegister(i),
      (val: uint8) => cdrom.writeRegister(i, val)

# 7-segment display
addressSpace.io8 0x1f802041, Expansion2, nil,
  (val: uint8) => debug fmt "POST {val}"

# PCSX-redux MIPS API
addressSpace.io8 0x1f802080, BuiltIn, nil,
  proc(val: uint8) =
    stdout.write(val.char)
    stdout.flushFile
addressSpace.io8 0x1f802081, BuiltIn, nil,
  (val: uint8) => warn "Debug break"

# UART status
addressSpace.io8 0x1f802021, Serial, () => 4, nil
# UART TX
addressSpace.io8 0x1f802023, Serial, nil,
  proc(val: uint8) =
    stdout.write(val.char)
    stdout.flushFile
addressSpace.ignore8 0x1f802020, Serial
addressSpace.ignore8 0x1f802022, Serial
for address in 0x1f802024u32..0x1f80202fu32:
  addressSpace.ignore8 address, Serial

# Nocash halt until interrupt
addressSpace.io8 0x1f802066, BuiltIn, () => 0, nil

# CD-ROM DATA FIFO
addressSpace.io16 0x1f801802, CDROM, cdrom.readData16, nil

# PCSX-Redux MIPS API
addressSpace.io16 0x1f802082, BuiltIn, nil,
  proc(val: uint16) =
    fatal fmt"Exit requested with code {val}"
    quit 1

# JOY_MODE
addressSpace.io16 0x1f801048, Serial, joyMode, setJoyMode
# JOY_CTRL
addressSpace.io16 0x1f80104a, Serial, joyControl, setJoyControl
# JOY_BAUD
addressSpace.io16 0x1f80104e, Serial, joyBaud, nil

# Memory region delay
for i in regionDelays.low .. regionDelays.high:
  capture i:
    addressSpace.io32 (0x1f801008 + i.word*4), BuiltIn,
      () => regionDelays[i].word,
      proc(val: uint32) =
        word(regionDelays[i]) = val
        updateDelays()
# Common delay
addressSpace.io32 0x1f801020, BuiltIn,
  () => commonDelay.word,
  proc(val: uint32) =
      word(commonDelay) = val
      updateDelays()

# Timers
for i in 0 .. 2:
  capture i:
    addressSpace.io32 (0x1f801100 + i.word*16), BuiltIn,
      () => timers[i].counter,
      (val: word) => timers[i].setCounter(val)
    addressSpace.io32 (0x1f801104 + i.word*16), BuiltIn,
      () => timers[i].mode,
      (val: word) => timers[i].setMode(val)
    addressSpace.io32 (0x1f801108 + i.word*16), BuiltIn,
      () => timers[i].target,
      (val: word) => timers[i].setTarget(val)

# Interrupt status
addressSpace.io32 0x1f801070, BuiltIn,
    () => irqs.status,
    (val: word) => irqs.setStatus(val)
# Interrupt mask
addressSpace.io32 0x1f801074, BuiltIn,
    () => irqs.mask,
    (val: word) => irqs.setMask(val)

# DMA channels
for chan in 0..6:
  capture chan:
    addressSpace.io32 (0x1f801080 + chan.word*16), BuiltIn,
      () => dmaBaseAddress(chan),
      (val: word) => setDMABaseAddress(chan, val)
    addressSpace.io32 (0x1f801084 + chan.word*16), BuiltIn,
      () => dmaBlockControl(chan),
      (val: word) => setDMABlockControl(chan, val)
    addressSpace.io32 (0x1f801088 + chan.word*16), BuiltIn,
      () => dmaChannelControl(chan),
      (val: word) => setDMAChannelControl(chan, val)

# DMA control register
addressSpace.io32 0x1f8010f0, BuiltIn, dmaControl, setDMAControl
# DMA interrupt register
addressSpace.io32 0x1f8010f4, BuiltIn, dmaInterrupt, setDMAInterrupt

# GP0/GPUREAD
addressSpace.io32 0x1f801810, GPU, gpuread, gp0

# GP1/GPUSTAT
addressSpace.io32 0x1f801814, GPU, gpustat, gp1

# PCSX-Redux MIPS API
addressSpace.io32 0x1f802080, BuiltIn, () => 0x58534350, nil
addressSpace.io32 0x1f802084, BuiltIn, nil,
  proc(val: uint32) =
    var msg: string
    var address = val
    var c: uint8 = addressSpace.rawRead[:uint8](address)
    while c != 0:
      msg &= c.char
      address += 1
      c = addressSpace.rawRead[:uint8](address)

# JOY_DATA
addressSpace.io32 0x1F801040, Serial, joyReceive, joyTransmit
# JOY_STAT
addressSpace.io32 0x1F801044, Serial, joyStat, nil

# Ignored (for now) values
addressSpace.cell32 0x1f801000, BuiltIn
addressSpace.cell32 0x1f801004, BuiltIn
addressSpace.cell32 0x1f801060, BuiltIn

# SIO
for address in countup(0x1f801050u32, 0x1f80105cu32, 4):
  addressSpace.ignore32 address, BuiltIn

# SPU
for address in countup(0x1f801c00u32, 0x1f801ffeu32, 2):
  if address != 0x1F801DAEu32 and address != 0x1f801daau32:
    addressSpace.cell16 address, SPU

var spucnt {.saved.}: uint16 = 0
addressSpace.io16 0x1f801daau32, SPU,
  () => spucnt,
  proc(val: uint16) = spucnt = val

addressSpace.io16 0x1f801daeu32, SPU,
  () => (spucnt and 0x3f) or (if spucnt.testBit 5: 1 shl 7 else: 0),
  nil

# Set up DMA handlers.
dma.channels[2].read = gpuReadDMA
dma.channels[2].write = gpuWriteDMA
dma.channels[3].read = cdromReadDMA
# SPU DMA is noisy so ignore it
dma.channels[4].write = proc (val: word) = discard

type EXE {.packed.} = object
  id: array[8, char]
  pad: array[8, char]
  initialPC: word
  initialGP: word
  loadAddress: word
  fileSize: word
  unk0, unk1: word
  memFillStart, memFillSize: word
  initialSPBase, initialSPOffset: word

const wantedID: array[8, char] = ['P', 'S', '-', 'X', ' ', 'E', 'X', 'E']
var header {.saved.}: EXE

proc loadEXE*(exe: string) =
  if exe.len > EXE.sizeof:
    header = (cast[ptr EXE](addr(exe[0])))[]

    if header.id != wantedID:
      error "EXE file has wrong header"

    for i in header.memFillStart ..< header.memFillStart + header.memFillSize:
      addressSpace.rawWrite[:uint8](i, 0)

    for i in 0x800 ..< exe.len:
      addressSpace.rawWrite[:uint8](header.loadAddress + i.word - 0x800, exe[i].uint8)

  else:
    error "EXE file too small"

proc runSystem*(clocks: int64) =
  ## Run the system.

  let stop = events.now + clocks
  while events.now < stop:
    let now = events.now
    var time = now

    if cpu.cpu.pc == 0xbfc06ff0u32 or cpu.cpu.pc == 0x1fc06ff0u32:
      info "Kernel done"
      if header.id == wantedID:
        info "Switching to loaded EXE"
        cpu.cpu.jump(header.initialPC)
        cpu.cpu[Register(4)] = 1
        cpu.cpu[Register(5)] = 0
        cpu.cpu[Register(28)] = header.initialGP
        var base = header.initialSPBase
        if base == 0: base = cpu.cpu[Register(29)]
        cpu.cpu[Register(29)] = base + header.initialSPOffset
        cpu.cpu[Register(30)] = base + header.initialSPOffset

    cpu.cpu.step(time)
    events.fastForward(time - now)

proc dumpRAM*(filename: string) =
  ## Dump the contents of RAM to a file.

  writeFile(filename, ram)
