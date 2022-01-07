import common, utils

type
  Page = distinct ByteAddress

const
  pageSize = 0x1000

# Page tables
const
  invalidPage {.used.}: Page = Page(0)

func initPage(page: ptr array[pageSize, byte], writable: bool, io: bool): Page =
  let address = cast[ByteAddress](page)
  assert address mod pageSize == 0
  Page(address or (if writable: 1 else: 0) or (if io: 2 else: 0))

func pointer(page: Page): ptr array[pageSize, byte] {.inline.} =
  # This gives null if the page is invalid
  cast[ptr array[pageSize, byte]](ByteAddress(page) and not 3)

func writable(page: Page): bool {.inline.} =
  (ByteAddress(page) and 1) != 0

func IO(page: Page): bool {.inline.} =
  (ByteAddress(page) and 2) != 0

type
  PageTable = object
    table {.align: 4096.}: array[0x100000, Page]
    # Keep references to the underlying arrays, to prevent them
    # getting garbage collected while 'table' is still alive
    regions: seq[ref seq[byte]]

var
  pageTable: PageTable
  bios {.align: 4096.}: array[0x80000, byte]
  ram {.align: 4096.}: array[0x200000, byte]
  scratchpad {.align: 4096.}: array[0x1000, byte]
  expansion {.align: 4096.}: array[0x1000, byte]
  ioPorts {.align: 4096.}: array[0x1000, byte]
  cacheControl {.align: 4096.}: array[0x1000, byte]

# Initialise expansion to -1, and read in BIOS
for x in expansion.mitems: x = 0xff
bios[0 ..< 0x80000] = toOpenArrayByte(static (staticRead "../SCPH1002.bin"), 0, 0x7ffff)

type
  ResolvedAddress[T] = tuple[pointer: ptr T, writable: bool, io: bool]

proc resolve[T](table: var PageTable, address: word): ResolvedAddress[T] {.inline.} =
  if address mod cast[word](sizeof(T)) != 0: raise new UnalignedAccessError

  let
    page = address shr 12
    offset = address and 0xfff
    entry = table.table[page]

  if entry.pointer.isNil: raise new InvalidAddressError
  let pointer = cast[ptr T](cast[ByteAddress](entry.pointer) +% cast[ByteAddress](offset))

  return (pointer: pointer, writable: entry.writable, io: entry.IO())

proc fetch*(address: word): word {.inline.} =
  pageTable.resolve[:word](address).pointer[]

proc mapRegion(table: var PageTable, arr: var openArray[byte], address: word, writable: bool, io: bool) =
  assert arr.len mod pageSize == 0
  assert address mod pageSize == 0

  table.regions.add(cast[ref seq[byte]](arr))
  let startingPage = address div pageSize
  for i in 0 ..< arr.len div pageSize:
    let page = sliceArray[pageSize, byte](arr, i * pageSize)
    table.table[startingPage + cast[word](i)] = initPage(page, writable = writable, io = io)

  if address + word(arr.len) <= 0x20000000u32:
    # Add the block to KSEG0 and KSEG1
    mapRegion(table, arr, address + 0x80000000u32, writable, io)
    mapRegion(table, arr, address + 0xa0000000u32, writable, io)

proc read*[T](address: word): T {.inline.} =
  let resolved = pageTable.resolve[:T](address)
  resolved.pointer[]
  # TODO: handle I/O

proc write*[T](address: word, value: T): void {.inline.} =
  let resolved = pageTable.resolve[:T](address)
  if resolved.writable:
    resolved.pointer[] = value
  # TODO: handle I/O

# Map in all the memory
#         region        address        writable  io
pageTable.mapRegion(ram,          0x00000000u32, true,     false)
pageTable.mapRegion(ram,          0x00200000u32, true,     false)
pageTable.mapRegion(ram,          0x00400000u32, true,     false)
pageTable.mapRegion(ram,          0x00600000u32, true,     false)
pageTable.mapRegion(expansion,    0x1f000000u32, false,    false)
pageTable.mapRegion(scratchpad,   0x1f800000u32, true,     false)
pageTable.mapRegion(ioPorts,      0x1f801000u32, true,     true)
pageTable.mapRegion(expansion,    0x1f802000u32, false,    false)
pageTable.mapRegion(bios,         0x1fc00000u32, false,    false)
pageTable.mapRegion(cacheControl, 0xfffe0000u32, true,     true)

# Set up I/O space
write[uint32](0x1f801000u32, 0x1f000000u32)
write[uint32](0x1f801004u32, 0x1f802000u32)
write[uint32](0x1f801008u32, 0x0013243fu32)
write[uint32](0x1f80100cu32, 0x00003022u32)
write[uint32](0x1f801010u32, 0x0013243fu32)
write[uint32](0x1f801014u32, 0x200931e1u32)
write[uint32](0x1f801018u32, 0x00020843u32)
write[uint32](0x1f80101cu32, 0x00070777u32)
write[uint32](0x1f801020u32, 0x00031125u32)
write[uint32](0x1f801060u32, 0x00000b88u32)
