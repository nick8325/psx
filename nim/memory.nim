type
  word = uint32
  Page = distinct ByteAddress

const
  pageSize = 0x1000

# Convert a page-aligned slice of an array into a page
func slicePage(arr: var openArray[byte], offset: word): ptr array[pageSize, byte] =
  assert offset mod pageSize == 0
  assert cast[uint32](arr.len) >= offset + pageSize
  let
    address = addr(arr[offset])
    raw = cast[ptr UncheckedArray[byte]](address)
  cast[ptr array[pageSize, byte]](raw)

# Page tables
#[
const
  invalidPage: Page = Page(0) # unused
]#

func initPage(page: ptr array[pageSize, byte], writable: bool, io: bool): Page =
  let address = cast[ByteAddress](page)
  assert address mod pageSize == 0
  Page(address or (if writable: 1 else: 0) or (if io: 2 else: 0))

func pointer(page: Page): ptr array[pageSize, byte] {. inline .} =
  # This gives null if the page is invalid
  cast[ptr array[pageSize, byte]](ByteAddress(page) and not 3)

func writable(page: Page): bool {. inline .} =
  (ByteAddress(page) and 1) != 0

func IO(page: Page): bool {. inline .} =
  (ByteAddress(page) and 2) != 0

type
  PageTable = object
    table {. align: 4096 .}: array[0x100000, Page]
    # Keep references to the underlying arrays, to prevent them
    # getting garbage collected while 'table' is still alive
    regions: seq[ref seq[byte]]

var
  pageTable: PageTable
  bios {. align: 4096 .}: array[0x80000, byte]
  ram {. align: 4096 .}: array[0x200000, byte]
  scratchpad {. align: 4096 .}: array[0x1000, byte]
  expansion {. align: 4096 .}: array[0x1000, byte]
  ioPorts {. align: 4096 .}: array[0x1000, byte]
  cacheControl {. align: 4096 .}: array[0x1000, byte]

# Initialise expansion to -1, and read in BIOS
for x in expansion.mitems: x = 0xff
bios[0 ..< 0x80000] = toOpenArrayByte(static (staticRead "../SCPH1002.bin"), 0, 0x7ffff)

type
  ResolvedAddress[T] = tuple[pointer: ptr T, writable: bool, io: bool]
  MemoryError = object of CatchableError
  InvalidAddressError = object of MemoryError
  UnalignedAccessError = object of MemoryError

let
  invalidAddressError = new InvalidAddressError
  unalignedAccessError = new UnalignedAccessError

proc resolve[T](table: var PageTable, address: word): ResolvedAddress[T] =
  if address mod cast[word](sizeof(T)) != 0: raise unalignedAccessError

  let
    page = address shr 12
    offset = address and 0xfff
    entry = table.table[page]

  if entry.pointer.isNil: raise invalidAddressError
  let pointer = cast[ptr word](cast[ByteAddress](entry.pointer) +% cast[ByteAddress](offset))

  return (pointer: pointer, writable: entry.writable, io: entry.IO())

proc fetch*(address: word): word =
  pageTable.resolve[:word](address).pointer[]

proc mapRegion(table: var PageTable, arr: var openArray[byte], address: word, writable: bool, io: bool) =
  assert arr.len mod pageSize == 0
  assert address mod pageSize == 0

  table.regions.add(cast[ref seq[byte]](arr))
  let startingPage = address div pageSize
  for i in 0 ..< arr.len div pageSize:
    let page = slicePage(arr, cast[word](i) * pageSize)
    table.table[startingPage + cast[word](i)] = initPage(page, writable = writable, io = io)

proc read*[T](address: word): T =
  let resolved = pageTable.resolve[T](address)
  resolved.pointer[]
  # TODO: handle I/O

proc write*[T](address: word, value: T): void =
  let resolved = pageTable.resolve[T](address)
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
