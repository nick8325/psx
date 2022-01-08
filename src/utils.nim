## Utility functions used throughout the program.

import bitops

func sliceArray*[size: static int, T](
  arr: var openArray[T], offset: int): ptr array[size, T] =
  ## Take a slice of an array, returning a raw pointer.

  assert arr.len >= offset+size
  cast[ptr array[size, T]](addr(arr[offset]))

type
  ## Like a slice, but on a range of bits in a word
  BitSlice*[T, U] = tuple[pos: int, width: int]

func bit*(pos: int): tuple[pos: int, width: int] {.inline.} =
  (pos: pos, width: 1)

func `[]`*[T, U](value: U, slice: BitSlice[T, U]): T {.inline.} =
  cast[T](value.bitsliced(slice.toSlice))

func toSlice*[T, U](slice: BitSlice[T, U]): Slice[int] {.inline.} =
  slice.pos ..< slice.pos+slice.width

func toMask*[T, U](slice: BitSlice[T, U]): U {.inline.} =
  slice.toSlice.toMask[:U]

proc `[]=`*[T, U](value: var U, slice: BitSlice[T, U], part: T) {.inline.} =
  let mask = slice.toMask
  value.clearMask mask
  value.setMask(mask and cast[U](part))

type
  ## A pattern that matches a given part of a word against a given value
  Pattern*[T: SomeInteger] = tuple[mask: T, value: T]

func initPattern*[T](mask: T, value: T): Pattern[T] =
  ## Create a pattern from a bitmask and a value
  (mask: mask, value: mask and value)

func equals*[T, U](slice: BitSlice[T, U], value: U): Pattern[U] =
  ## Create a pattern given a bitslice and an (unshifted) value
  let mask = toMask[U](slice.pos ..< slice.pos+slice.width)
  initPattern(mask, value shl slice.pos)

func `and`*[T](m1, m2: Pattern[T]): Pattern[T] =
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
