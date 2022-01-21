## The backend of the GPU - converts drawing commands into a framebuffer.

import std/[options, strformat]

type
  TransparencyMode* {.pure.} = enum
    ## 0-4 must match what the GPU uses.
    Mean = 0,
    Add = 1,
    Subtract = 2,
    AddQuarter = 3,
    Opaque = 4

  TextureColourDepth* {.pure.} = enum
    ## Must match what the GPU uses.
    FourBit = 0,
    EightBit = 1,
    FifteenBit = 2

  Coord* = tuple
    x, y: int

  Colour* = tuple
    red: uint8
    green: uint8
    blue: uint8

  TextureColourMode* = object
    case depth*: TextureColourDepth
    of FourBit, EightBit: palette*: Coord
    of FifteenBit: discard

  Texture*[N: static int] = object
    base*: Coord
    coords*: array[N, Coord]
    colourMode*: TextureColourMode

  Settings* = object
    drawingArea*: tuple[x1, x2, y1, y2: int]
    displayArea*: Option[tuple[x1, x2, y1, y2: int]]
    transparency*: TransparencyMode
    dither*: bool
    setMaskBit*: bool
    skipMaskedPixels*: bool

  Triangle* = object
    vertices*: array[3, Coord]
    colours*: Option[array[3, Colour]]
    texture*: Option[Texture[3]]

  Rectangle* = object
    position*: tuple[x: int, y: int]
    size*: tuple[x: int, y: int]
    colour*: Option[Colour]
    texture*: Option[Texture[1]]
    flipX*, flipY*: bool

  Line* = object
    start*: tuple[x: int, y: int, colour: Colour]
    stop*: tuple[x: int, y: int, colour: Colour]

func `$`*(c: Colour): string =
  fmt"#{c.red:02x}{c.green:02x}{c.blue:02x}"

func `$`*[N: static int](tex: Texture[N]): string =
  result = fmt"({tex.base.x:04x}, {tex.base.y:04x})"
  result &= fmt", colour mode=({tex.colourMode.depth})"
  if tex.colourMode.depth != FifteenBit:
    result &= fmt", palette=({tex.colourMode.palette.x}, {tex.colourMode.palette.y})"
  for i, coord in tex.coords:
    if i > 0: result &= "--"
    result &= fmt"({coord.x},{coord.y})"

func `$`*(tri: Triangle): string =
  result = "triangle, "
  for i, v in tri.vertices:
    if i > 0: result &= "--"
    result &= fmt"({v.x},{v.y})"
  if tri.colours.isSome:
    result &= fmt", colours "
    for i, c in tri.colours.get():
      if i > 0: result &= "--"
      result &= $c
  if tri.texture.isSome: result &= fmt", texture {tri.texture.get()}"

func `$`*(rect: Rectangle): string =
  result = "rectangle, "
  let x1 = rect.position.x + rect.size.x - 1
  let y1 = rect.position.y + rect.size.y - 1
  result &= fmt"({rect.position.x}, {rect.position.y})--({x1},{y1})"
  if rect.colour.isSome:
    result &= fmt", colour {rect.colour.get()}"
  if rect.texture.isSome:
    result &= fmt", texture {rect.texture.get()}"
  if rect.flipX:
    result &= ", x flipped"
  if rect.flipY:
    result &= ", y flipped"

proc draw*(x: Triangle) =
  echo fmt"draw {x}"
