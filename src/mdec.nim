## Motion decoder chip.

import basics, utils, savestates
import std/[strformat, deques, bitops]

const loggerComponent = logMDEC
logMDEC.level = lvlDebug

type
  CurrentBlock = enum
    cbY1, cbY2, cbY3, cbY4, cbCr, cbCb

var
  outputFIFO {.saved.}: Deque[uint32]
  dmaIn {.saved.}: bool
  dmaOut {.saved.}: bool
  statOutputDepth {.saved.}: int
  statOutputSigned {.saved.}: bool
  statOutputBit15 {.saved.}: bool
  statCurrentBlock {.saved.}: CurrentBlock
  statParamWordsRemaining {.saved.}: int # TODO: how to do this?

let
  command = BitSlice[int, word](pos: 29, width: 3)
  dataOutputDepth = BitSlice[int, word](pos: 27, width: 2)
  dataOutputSigned = BitSlice[bool, word](pos: 26, width: 1)
  dataOutputBit15 = BitSlice[bool, word](pos: 25, width: 1)
  macroblockSize = BitSlice[int, word](pos: 0, width: 16)
  quantTableColour = BitSlice[bool, word](pos: 0, width: 1)

var processCommand {.saved.}: Consumer[word]
processCommand = consumer(word):
  let cmd = take
  statOutputDepth = cmd[dataOutputDepth]
  statOutputSigned = cmd[dataOutputSigned]
  statOutputBit15 = cmd[dataOutputBit15]
  statParamWordsRemaining = cmd[macroblockSize]
  case cmd[command]
  of 1:
    debug fmt"Decoding macroblock"
    for i in 0..<cmd[macroblockSize]: discard take()
  of 2:
    debug fmt"Setting quant table"
    for i in 0..<16: discard take()
    if cmd[quantTableColour]:
      for i in 0..<16: discard take()
  of 3:
    debug fmt"Setting scale table (ignored)"
    for i in 0..<32: discard take()
  else:
    warn fmt"Unknown command {cmd:x}"

proc mdecCommand*(value: word) =
  trace fmt"command {value:x}"
  processCommand.give(value)

proc mdecResponse*: word =
  if outputFIFO.len >= 1:
    return outputFIFO.popFirst()
  else:
    warn fmt"read from empty FIFO"
    return 0

proc mdecReadDMA*: word =
  mdecResponse() # TODO: re-order block

proc mdecWriteDMA*(val: word) =
  mdecCommand(val)

proc mdecControl*(value: word) =
  trace fmt "control {value:x}"
  if value.testBit 31:
    debug fmt "reset"
    processCommand.reset
    outputFIFO.clear

proc mdecStatus*: word =
  word(outputFIFO.len == 0) shl 31 or
  # TODO: data-in FIFO full
  word(processCommand.busy) shl 29 or
  word(dmaIn) shl 28 or
  word(dmaOut) shl 27 or
  word(statOutputDepth) shl 25 or
  word(statOutputSigned) shl 24 or
  word(statOutputBit15) shl 23 or
  word(statCurrentBlock) shl 16 or
  word(statParamWordsRemaining)
