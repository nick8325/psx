## Support for reading CDs from CUE files.
# TODO: add ISO support too.
# TODO: support BIN files with 2048-byte sectors.

import std/[memfiles, sequtils, tables, os]
import libcue

type
  Sector = int ## A sector number.

const
  ## The size of a raw sector, in bytes.
  rawSectorSize* = 2352
  ## How many sectors per second.
  sectorsPerSec* = 75

proc toSector*(min, sec, sect: int): Sector =
  ## Convert min:sec:sect to a sector number.
  min*60*sectorsPerSec + sec*sectorsPerSec + sect

proc toTime*(sect: Sector): tuple[min: int, sec: int, sect: int] =
  ## Convert a sector number to min:sec:sect.
  let totalSecs = sect div sectorsPerSec
  result.min = totalSecs div 60
  result.sec = totalSecs mod 60
  result.sect = sect mod sectorsPerSec

type
  CD* = object
    ## A CD image.
    kind*: CDKind ## What kind of CD it is.
    trackTimes: seq[Sector] ## The time when each track starts, in sectors.
    data*: seq[Part] ## The data stored on the CD.

  CDKind* = enum
    ## What kind of CD it is.
    ckAudio, ckMode1, ckMode2

  PartKind* = enum
    ## What kind of data a CD part holds.
    pkBlank, pkRawData

  Part* = object
    ## A single part of a CD image.
    sectors*: int
    case kind: PartKind
    of pkBlank: discard
    of pkRawData: data: MemFile

proc `=destroy`(part: var Part) =
  if part.kind == pkRawData: part.data.close()

proc openRawData(filename: string): Part =
  ## Open a BIN file using 2352-byte sectors.
  let file = memfiles.open(filename)
  assert file.size mod rawSectorSize == 0
  let sectors = file.size div rawSectorSize
  Part(kind: pkRawData, sectors: sectors, data: file)

func sectors*(cd: CD): Sector =
  ## Return the total length of a CD in sectors.
  for part in cd.data: result += part.sectors

proc `[]`(file: MemFile, x: HSlice[int, int]): seq[uint8] =
  ## Safely read a slice from a MemFile.
  assert x.a <= x.b
  assert x.a >= 0 and x.a < file.size
  assert x.b >= 0 and x.b < file.size
  let bytes = cast[ptr UncheckedArray[uint8]](file.mem)
  bytes.toOpenArray(x.a, x.b).toSeq

proc trackCount*(cd: CD): int =
  ## Get the number of tracks in a CD.
  cd.trackTimes.len

proc trackStart*(cd: CD, track: int): int =
  ## Get the start of a given track.
  cd.trackTimes[track-1]

proc read*(part: Part, pos: Sector): seq[uint8] =
  ## Read a given sector from a part of a CD.
  assert pos >= 0 and pos < part.sectors

  case part.kind
  of pkBlank:
    result = newSeq[uint8](rawSectorSize)
  of pkRawData:
    let bytePos = rawSectorSize * pos
    result = part.data[bytePos ..< bytePos + rawSectorSize]

proc read*(cd: CD, pos: Sector): seq[uint8] =
  ## Read a given sector from a CD.
  assert pos >= 0 and pos < cd.sectors

  var pos = pos
  for part in cd.data:
    if pos < part.sectors: return part.read(pos)
    pos -= part.sectors

  assert false

######################################################################
# Reading CUE files.
######################################################################

type ParseError* = object of CatchableError

proc readCUE*(cuePath: string): CD =
  ## Read a CUE file.

  let dir = splitFile(cuePath.expandSymlink).dir.absolutePath
  let contents = cstring(readFile(cuePath))
  let cd = cue_parse_string(contents)
  if cd == nil: raise newException(ParseError, "parse error")
  defer: cd_delete(cd)

  result.kind =
    case cd_get_mode(cd)
    of MODE_CD_DA: ckAudio
    of MODE_CD_ROM: ckMode1
    of MODE_CD_ROM_XA: ckMode2
    else: raise newException(AssertionDefect, "unknown CD mode")

  var pos: Sector = 0

  template addPart(part: Part) =
    result.data.add part
    pos += part.sectors

  # The first 2 seconds are always blank
  addPart Part(sectors: toSector(0, 2, 0), kind: pkBlank)

  let trackCount = cd_get_ntrack(cd)
  for i in 1..trackCount:
    let track = cd_get_track(cd, i)
    assert track != nil
    let pre = track_get_zero_pre(track)
    let post = track_get_zero_post(track)
    let filename = $track_get_filename(track)
    let mode = track_get_mode(track)

    result.trackTimes.add pos + track_get_start(track)
    if pre != -1: addPart Part(sectors: pre, kind: pkBlank)
    case mode
      of MODE_AUDIO, MODE_MODE1_RAW, MODE_MODE2_RAW:
        addPart openRawData(filename.absolutePath(dir))
      else: raise newException(ParseError, "unsupported track mode")
    if post != -1: addPart Part(sectors: post, kind: pkBlank)
