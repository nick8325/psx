import std/[tables, setutils]
import std/options
import utils, common

type
  Phase = -1..3
  Key = (Phase, set[uint8])
  Trie* = object
    children: array[256, array[256, uint8]]
    values: array[256, uint8]
    next: uint8
    root: uint8
    hash: Table[Key, uint8]
  AmbiguousEncoding* = object of Defect

proc addNode(trie: var Trie, phase: Phase, vals: set[uint8], value: uint8, children: array[256, uint8]): uint8 =
  let pos = trie.next
  trie.next += 1

  trie.values[pos] = value
  trie.children[pos] = children
  trie.hash[(phase, vals)] = pos

  pos

func theVal(vals: set[uint8]): uint8 =
  for val in vals:
    return val
  return 0xff

proc makeTrie(trie: var Trie, patterns: Table[uint8, Pattern[word]], phase: Phase, vals: set[uint8]): uint8 =
  if (phase, vals) in trie.hash:
    return trie.hash[(phase, vals)]

  if phase == -1:
    if vals.card <= 1:
      return theVal(vals)
    else:
      raise newException(AmbiguousEncoding, "vals=" & $vals & ", patterns=" & $patterns)
  elif vals.card <= 1:
    let
      val = theVal(vals)
      subnode = makeTrie(trie, patterns, phase-1, {})
    var children: array[256, uint8]
    for i in 0..<256:
      children[i] = subnode
    return addNode(trie, phase, vals, val, children)
  else:
    var children: array[256, uint8]
    for i in 0..<256:
      var newVals: set[uint8]
      let
        phaseMask = 0xffu32 shl (phase*8)
        phaseValue = cast[uint32](i) shl (phase*8)
      for val in vals:
        let
          pattern = patterns[val]
          mask = phaseMask and pattern.mask
        if (phaseValue and mask) == (pattern.value and mask):
          newVals.incl(val)
      children[i] = makeTrie(trie, patterns, phase-1, newVals)
    return addNode(trie, phase, vals, 0xff, children)

proc makeTrie*[T](patterns: Table[T, Pattern[word]]): Trie =
  assert ord(low(T)) >= 0 and ord(high(T)) < 255
  var patternsU8: Table[uint8, Pattern[word]]
  for key, value in patterns.pairs:
    patternsU8[cast[uint8](key.ord)] = value

  var trie: Trie
  trie.root = makeTrie(trie, patternsU8, 3, patternsU8.keys.toSet)
  trie

proc find*[T](trie: Trie, val: uint32): Option[T] {.inline.} =
  let
    byte1 = (val shr 24) and 0xff
    byte2 = (val shr 16) and 0xff
    byte3 = (val shr 8) and 0xff
    byte4 = val and 0xff

  var res: uint8 = 0xff
  var pos: uint8 = trie.root
  res = res and trie.values[pos]
  pos = trie.children[pos][byte1]
  res = res and trie.values[pos]
  pos = trie.children[pos][byte2]
  res = res and trie.values[pos]
  pos = trie.children[pos][byte3]
  res = res and trie.values[pos]
  pos = trie.children[pos][byte4]
  res = res and pos

  if res == 0xff:
    none(T)
  else:
    some(T(res))
