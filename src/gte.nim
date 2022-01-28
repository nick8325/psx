## Geometry transformation engine.

import basics
import std/strformat

type
  Register* = distinct range[0..63]
  GTE* = object
    registers: array[Register, word]

func dataReg*(r: 0..63): Register =
  Register(r)

func controlReg*(r: 0..63): Register =
  Register(r+32)

let initGTE*: GTE = block:
  var registers: array[Register, word]
  GTE(registers: registers)

proc execute*(gte: var GTE, op: int) =
  echo fmt"GTE op {op:x}"

func `[]`*(gte: GTE, reg: Register): word =
  gte.registers[reg]

proc `[]=`*(gte: var GTE, reg: Register, value: word) =
  gte.registers[reg] = value
