## The PSX virtual address space.

import common, utils

type
  # We represent the address space as a page table consisting of an array of
  # page descriptors. A page descriptor is a pointer to a page-aligned block
  # of memory, but we also store information about the page in the low bits.
  # An unmapped page is represented as 0.
  Page = distinct ByteAddress ## A page descriptor.

const
  pageSize = 0x1000

const
  invalidPage {.used.}: Page = Page(0)

func initPage(page: ptr array[pageSize, byte], writable: bool, io: bool): Page =
  ## Create a page descriptor from a page-aligned piece of memory.

  let address = cast[ByteAddress](page)
  assert address mod pageSize == 0
  Page(address or (if writable: 1 else: 0) or (if io: 2 else: 0))

func pointer(page: Page): ptr array[pageSize, byte] {.inline.} =
  ## Get the pointer from a page descriptor (or null).

  # This gives null if the page is invalid
  cast[ptr array[pageSize, byte]](ByteAddress(page) and not 3)

func writable(page: Page): bool {.inline.} =
  ## Is a page writable?

  (ByteAddress(page) and 1) != 0

func IO(page: Page): bool {.inline.} =
  ## Does a page represent memory-mapped I/O?

  (ByteAddress(page) and 2) != 0

type
  AddressSpace* = object
    ## A virtual address space.

    table {.align: 4096.}: array[0x100000, Page]
    # Keep references to the underlying arrays, to prevent them
    # getting garbage collected while 'table' is still alive
    regions: seq[ref seq[byte]]

type
  ResolvedAddress[T] = tuple[pointer: ptr T, writable: bool, io: bool] ## \
    ## A virtual address resolved to a pointer on the host.

proc resolve[T](space: AddressSpace, address: word): ResolvedAddress[T] {.inline.} =
  ## Resolve a virtual address to a pointer.
  ## Raises InvalidAddressError if the address is invalid.

  if address mod cast[word](sizeof(T)) != 0: raise new UnalignedAccessError

  let
    page = address shr 12
    offset = address and 0xfff
    entry = space.table[page]

  if entry.pointer.isNil: raise new InvalidAddressError
  let pointer = cast[ptr T](cast[ByteAddress](entry.pointer) +% cast[ByteAddress](offset))

  return (pointer: pointer, writable: entry.writable, io: entry.IO())

proc mapRegion(space: var AddressSpace, arr: var openArray[byte], address: word, writable: bool, io: bool) =
  ## Map a byte array into the virtual address space.
  ## The array must be page-aligned and its size must be a multiple of one page.

  assert arr.len mod pageSize == 0
  assert address mod pageSize == 0

  space.regions.add(cast[ref seq[byte]](arr))
  let startingPage = address div pageSize
  for i in 0 ..< arr.len div pageSize:
    let page = sliceArray[pageSize, byte](arr, i * pageSize)
    space.table[startingPage + cast[word](i)] = initPage(page, writable = writable, io = io)

  if address + word(arr.len) <= 0x20000000u32:
    # Add the block to KSEG0 and KSEG1
    mapRegion(space, arr, address + 0x80000000u32, writable, io)
    mapRegion(space, arr, address + 0xa0000000u32, writable, io)

proc fetch*(space: AddressSpace, address: word): word {.inline.} =
  ## Fetch a word of memory as an instruction.
  ## Raises InvalidAddressError if the address is invalid.

  space.resolve[:word](address).pointer[]

proc read*[T](space: AddressSpace, address: word): T {.inline.} =
  ## Read data from memory.
  ## Raises InvalidAddressError if the address is invalid.

  let resolved = space.resolve[:T](address)
  resolved.pointer[]
  # TODO: handle I/O

proc write*[T](space: AddressSpace, address: word, value: T): void {.inline.} =
  ## Write data to memory.
  ## Raises InvalidAddressError if the address is invalid.

  let resolved = space.resolve[:T](address)
  if resolved.writable:
    resolved.pointer[] = value
  # TODO: handle I/O

# The PSX address space.

var
  addressSpace*: AddressSpace ## The PSX address space.
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
addressSpace.mapRegion(cacheControl, 0xfffe0000u32, true,     true)

# Set up I/O space
addressSpace.write[:word](0x1f801000u32, 0x1f000000u32)
addressSpace.write[:word](0x1f801004u32, 0x1f802000u32)
addressSpace.write[:word](0x1f801008u32, 0x0013243fu32)
addressSpace.write[:word](0x1f80100cu32, 0x00003022u32)
addressSpace.write[:word](0x1f801010u32, 0x0013243fu32)
addressSpace.write[:word](0x1f801014u32, 0x200931e1u32)
addressSpace.write[:word](0x1f801018u32, 0x00020843u32)
addressSpace.write[:word](0x1f80101cu32, 0x00070777u32)
addressSpace.write[:word](0x1f801020u32, 0x00031125u32)
addressSpace.write[:word](0x1f801060u32, 0x00000b88u32)
