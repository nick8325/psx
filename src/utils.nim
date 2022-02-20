## Utility functions used throughout the program.

import std/[bitops, typetraits, logging]
export Level

const
  lvlTrace*: Level = lvlAll

type
  Component* = enum
    logCPU, logMemory, logEventQueue, logGPU, logRasteriser,
    logDMA, logIRQ, logTimer, logCDROM, logMachine, logJoy

func initialLevel(component: Component): Level {.inline.} =
  case component
  of logMachine: lvlDebug
  of logCDROM: lvlTrace
  else: lvlInfo

func minLevel*(component: Component): Level {.inline.} =
  lvlInfo.min(component.initialLevel)

var
  loggers: array[Component, Logger]

for component, logger in loggers.mpairs:
  let name = ($component)["log".len..^1]
  logger =
    newConsoleLogger(levelThreshold = component.initialLevel,
                     fmtStr = defaultFmtStr & name & ": ",
                     useStderr = true)

template level*(component: Component): Level =
  loggers[component].levelThreshold

template `level=`*(component: Component, level: Level) =
  assert level >= component.minLevel
  loggers[component].levelThreshold = level

template log*(component: Component, lev: Level, args: varargs[string, `$`]) =
  when lev >= component.minLevel:
    if lev >= component.level:
      loggers[component].log(lev, args)
template trace* (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlTrace, args)
template debug* (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlDebug, args)
template info*  (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlInfo, args)
template notice*(args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlNotice, args)
template warn*  (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlWarn, args)
template error* (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlError, args)
template fatal* (args: varargs[string, `$`]) =
  mixin loggerComponent
  loggerComponent.log(lvlFatal, args)

func sliceArray*[size: static int, T](
  arr: var openArray[T], offset: int): ptr array[size, T] =
  ## Take a slice of an array, returning a raw pointer.

  assert arr.len >= offset+size
  cast[ptr array[size, T]](addr(arr[offset]))

type
  ## Like a slice, but on a range of bits in a word
  BitSlice*[T, U] = object
    pos*: int
    width*: int

func bit*[T](pos: int): BitSlice[bool, T] {.inline.} =
  BitSlice[bool, T](pos: pos, width: 1)

func bit*[T, U](slice: BitSlice[T, U], i: int): BitSlice[bool, U] {.inline.} =
  assert 0 <= i and i < slice.width
  bit[U](slice.pos + i)

func `[]`*[T, U](value: U, slice: BitSlice[T, U]): T {.inline.} =
  cast[T](distinctBase(U)(value).bitsliced(slice.toSlice))

func toSlice*[T, U](slice: BitSlice[T, U]): Slice[int] {.inline.} =
  slice.pos ..< slice.pos+slice.width

func toMask*[T, U](slice: BitSlice[T, U]): U {.inline.} =
  U(slice.toSlice.toMask[:distinctBase(U)])

proc `[]=`*[T, U](value: var U, slice: BitSlice[T, U], part: T) {.inline.} =
  let mask = distinctBase(U)(slice.toMask)
  distinctBase(U)(value).clearMask mask
  distinctBase(U)(value).setMask(mask and (cast[distinctBase(U)](part) shl slice.pos))

type
  ## A range of bits that should be sign extended on extraction.
  SignedBitSlice*[T, U] = object
    pos*: int
    width*: int

func signExtendFrom[T](x: T, bit: int): T {.inline.} =
  let
    low = 1 shl (bit-1)
    range = 1 shl bit
  ((x +% low) mod range) -% low

static:
  assert signExtendFrom(0x8f, 8) == -113
  assert signExtendFrom(0x7f, 8) == 127
  assert signExtendFrom(2047, 11) == -1

func unsign[T, U](slice: SignedBitSlice[T, U]): BitSlice[T, U] {.inline.} =
  BitSlice[T, U](pos: slice.pos, width: slice.width)

func `[]`*[T, U](value: U, slice: SignedBitSlice[T, U]): T {.inline.} =
  value[slice.unsign].signExtendFrom(slice.width)

func toSlice*[T, U](slice: SignedBitSlice[T, U]): Slice[int] {.inline.} =
  slice.unsign.toSlice

func toMask*[T, U](slice: SignedBitSlice[T, U]): U {.inline.} =
  slice.unsign.toMask

proc `[]=`*[T, U](value: var U, slice: SignedBitSlice[T, U], part: T) {.inline.} =
  value[slice.unsign] = part

type
  ## A pattern that matches a given part of a word against a given value
  Pattern*[T: SomeInteger] = tuple[mask: T, value: T]

func initPattern*[T](mask: T, value: T): Pattern[T] {.inline.} =
  ## Create a pattern from a bitmask and a value
  (mask: mask, value: mask and value)

func equals*[T, U](slice: BitSlice[T, U], value: U): Pattern[U] {.inline.} =
  ## Create a pattern given a bitslice and an (unshifted) value
  let mask = toMask[U](slice.pos ..< slice.pos+slice.width)
  initPattern(mask, value shl slice.pos)

func `and`*[T](m1, m2: Pattern[T]): Pattern[T] {.inline.} =
  ## Take the conjunction of two patterns.
  let commonMask = m1.mask and m2.mask
  assert (m1.value and commonMask) == (m2.value and commonMask)
  (mask: m1.mask or m2.mask, value: m1.value or m2.value)

func signed*(x: uint32): int32 {.inline.} =
  ## Convert unsigned to signed.
  cast[int32](x)

func unsigned*(x: int32): uint32 {.inline.} =
  ## Convert signed to unsigned.
  cast[uint32](x)

type
  ## An integer where only some bits are allowed to be freely set
  ## 'mask' = writable bits
  Masked*[T] = object
    value*: T
    mask*: T

func `$`*[T](x: Masked[T]): string {.inline.} =
  $x.value

proc update*[T](x: var Masked[T], value: T) {.inline.} =
  x.value = (x.value and not x.mask) or (value and x.mask)

converter value*[T](x: Masked[T]): T {.inline.} =
  x.value

func `[]`*[T, U](val: Masked[U], slice: BitSlice[T, U]): T {.inline.} =
  val.value[slice]

template bitfield*(U: typedesc, name: untyped, T: typedesc, thePos: int, theWidth: int, signed: bool = false) =
  when signed:
    const slice = SignedBitSlice[T, U](pos: thePos, width: theWidth)
  else:
    const slice = BitSlice[T, U](pos: thePos, width: theWidth)

  proc name(whole: U): T {.inject, used, inline.} =
    whole[slice]

  proc `name =`(whole: var U, part: T) {.inject, used, inline.} =
    whole[slice] = part

func clampedConvert*[T](x: int): T {.inline.} =
  x.clamp(T.low.int, T.high.int).T

type
  Consumer*[T] = distinct iterator(t: T)

template consumer*[T](_: typedesc[T], body: untyped): untyped =
  mixin consumerArg
  iterator it(consumerArg {.inject.}: T) {.closure.} =
    body
  it

proc toIter[T](co: Consumer[T]): iterator(t: T) =
  (iterator(t: T))(co)

proc start*[T](iter: iterator(t: T)): Consumer[T] =
  result = Consumer[T](iter)
  toIter(result)(T.default)

proc give*[T](co: Consumer[T], value: T) =
  assert not toIter(co).finished
  toIter(co)(value)

template take*: untyped =
  mixin consumerArg
  yield
  consumerArg

template cmpKey*[T](key: untyped): untyped =
  proc compare(x, y: T): int = cmp(key(x), key(y))
  compare

func signum*[T](x: T): T =
  if x > 0: return 1
  elif x < 0: return -1
  else: return 0

func divRoundUp*[T](x, y: T): T =
  if x == 0: T(0) else: (x-1) div y + 1
