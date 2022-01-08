## The PSX virtual address space.

import common, utils
import std/strformat

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
  IOKind* {.pure.} = enum
    ## Which memory type an I/O access is.
    Read, Write

  Memory* = object
    ## The PSX address space.

    table {.align: 4096.}: array[0x100000, Page]
    # Keep references to the underlying arrays, to prevent them
    # getting garbage collected while 'table' is still alive
    regions: seq[ref seq[byte]]

    # I/O handlers.
    # Returns true if the I/O was handled.
    # For each address, only one of ioHandler8/16/32 needs to handle it -
    # the main I/O handler takes care of splitting up requests.
    ioHandler8*: proc(address: word, value: var uint8, kind: IOKind): bool
    ioHandler16*: proc(address: word, value: var uint16, kind: IOKind): bool
    ioHandler32*: proc(address: word, value: var uint32, kind: IOKind): bool

type
  ResolvedAddress[T] = tuple[pointer: ptr T, writable: bool, io: bool] ## \
    ## A virtual address resolved to a pointer on the host.

proc resolve[T](memory: Memory, address: word, kind: AccessKind): ResolvedAddress[T] {.inline.} =
  ## Resolve a virtual address to a pointer.
  ## Raises a MachineError if the address is invalid.

  if address mod cast[word](sizeof(T)) != 0:
    raise MachineError(error: AddressError, address: address, kind: kind)

  let
    page = address shr 12
    offset = address and 0xfff
    entry = memory.table[page]

  if entry.pointer.isNil:
    raise MachineError(error: BusError, address: address, kind: kind)

  let pointer = cast[ptr T](cast[ByteAddress](entry.pointer) +% cast[ByteAddress](offset))

  return (pointer: pointer, writable: entry.writable, io: entry.IO())

proc mapRegion*(memory: var Memory, arr: var openArray[byte], address: word, writable: bool, io: bool) =
  ## Map a byte array into the virtual address space.
  ## The array must be page-aligned and its size must be a multiple of one page.

  assert arr.len mod pageSize == 0
  assert address mod pageSize == 0

  memory.regions.add(cast[ref seq[byte]](arr))
  let startingPage = address div pageSize
  for i in 0 ..< arr.len div pageSize:
    let page = sliceArray[pageSize, byte](arr, i * pageSize)
    memory.table[startingPage + cast[word](i)] = initPage(page, writable = writable, io = io)

  if address + word(arr.len) <= 0x20000000u32:
    # Add the block to KSEG0 and KSEG1
    mapRegion(memory, arr, address + 0x80000000u32, writable, io)
    mapRegion(memory, arr, address + 0xa0000000u32, writable, io)

proc handleIO(memory: Memory, address: word, size: static int, kind: IOKind) =
  ## Execute I/O handlers for a write (which must be to I/O space).

  # Implementation idea: always start by calling 32-bit handler, then
  # split into smaller pieces as needed.
  # In the helper procs handleIO16/32, 'address' doesn't have to be
  # 16/32-bit aligned, but it does have to be aligned wrt 'size'.
  # So all direct calls to ioHandler16/32 mask off the lower bits first.

  proc handleIO8(memory: Memory, address: word, kind: IOKind): bool {.inline.} =
    # Doesn't matter what 'kind' we pass in since the address must resolve
    let pointer = memory.resolve[:uint8](address, Fetch).pointer
    memory.ioHandler8 != nil and memory.ioHandler8(address, pointer[], kind)

  proc handleIO16(memory: Memory, address: word, size: static int, kind: IOKind): bool {.inline.} =
    # Try a 16-bit write first
    if memory.ioHandler16 != nil:
      let pointer = memory.resolve[:uint16](address and not 1u32, Fetch).pointer
      if memory.ioHandler16(address and not 1u32, pointer[], kind): return true

    # Split into one or two 8-bit writes, depending on size
    if size == 2:
      return memory.handleIO8(address, kind) and memory.handleIO8(address xor 1, kind)
    else:
      return memory.handleIO8(address, kind)

  proc handleIO32(memory: Memory, address: word, size: static int, kind: IOKind): bool {.inline.} =
    # Try a 32-bit write first
    if memory.ioHandler32 != nil:
      let pointer = memory.resolve[:uint32](address and not 3u32, Fetch).pointer
      if memory.ioHandler32(address and not 3u32, pointer[], kind): return true

    # Split into one or two 16-bit writes, depending on size
    if size == 4:
      return memory.handleIO16(address, 2, kind) and
        memory.handleIO16(address xor 2, 2, kind)
    else:
      return memory.handleIO16(address, size, kind)

  if not handleIO32(memory, address, size, kind):
    case kind
    of Read: echo fmt"Couldn't read {size} bytes from I/O address {address:x}"
    of Write: echo fmt"Couldn't write {size} bytes to I/O address {address:x}"

proc fetch*(memory: Memory, address: word): word {.inline.} =
  ## Fetch a word of memory as an instruction.
  ## Raises a MachineError if the address is invalid.

  memory.resolve[:word](address, Fetch).pointer[]

proc rawRead*[T](memory: Memory, address: word, io: var bool): T {.inline.} =
  ## Read data from memory, without invoking any I/O handlers.
  ## Raises a MachineError if the address is invalid.

  let resolved = memory.resolve[:T](address, Load)
  io = resolved.io
  resolved.pointer[]

proc read*[T](memory: Memory, address: word): T {.inline.} =
  ## Read data from memory.
  ## Raises a MachineError if the address is invalid.

  var io: bool
  if io: memory.handleIO(address, T.sizeof, Read)
  result = rawRead[T](memory, address, io)

proc rawWrite*[T](memory: Memory, address: word, value: T, io: var bool): void {.inline.} =
  ## Write data to memory, without invoking any I/O handlers.
  ## Raises a MachineError if the address is invalid.

  let resolved = memory.resolve[:T](address, Store)
  if resolved.writable:
    resolved.pointer[] = value
  io = resolved.io

proc write*[T](memory: Memory, address: word, value: T): void {.inline.} =
  ## Write data to memory.
  ## Raises a MachineError if the address is invalid.

  var io: bool
  rawWrite[T](memory, address, value, io)
  if io: memory.handleIO(address, T.sizeof, Write)
