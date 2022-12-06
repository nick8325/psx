# Convert a BIN file to ISO format.

let cdfile = readFile "test.bin"
var iso: string

const
  rawSectorSize = 2352
  isoSectorSize = 2048
  offset = 0x18

let
  length = cdfile.len div rawSectorSize

for i in 0..<length:
  let start = i * rawSectorSize + offset
  iso.add cdfile[start ..< start+isoSectorSize]

writeFile "test.iso", iso
