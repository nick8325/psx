## The backend of the GPU - converts drawing commands into a framebuffer.

import std/[options, strformat]

type
  Coord* = tuple
    ## A pixel coordinate.
    x, y: int

  Rect* = tuple
    ## A rectangular area.
    ## x1 and y1 are inclusive, x2 and y2 are exclusive.
    x1, y1, x2, y2: int

  Colour* = tuple
    ## A 24-bit colour.
    red: uint8
    green: uint8
    blue: uint8

  TransparencyMode* {.pure.} = enum
    ## A transparency mode. 0-3 must match what the GPU uses.
    Mean = 0,
    Add = 1,
    Subtract = 2,
    AddQuarter = 3,
    Opaque = 4

  TextureColourDepth* {.pure.} = enum
    ## The colour depth of a texture. Must match what the GPU uses.
    FourBit = 0,
    EightBit = 1,
    FifteenBit = 2

  TextureColourMode* = object
    ## The colour depth of a texture, plus palette if needed.
    case depth*: TextureColourDepth
    of FourBit, EightBit: palette*: Coord
    of FifteenBit: discard

  Texture*[N: static int] = object
    ## A texture with N vertices.
    page*: Coord ## x must be a multiple of 64, y a multiple of 256.
    windowMask*, windowOffset*: Coord
    coords*: array[N, Coord] ## relative to page.
    colourMode*: TextureColourMode

  Settings* = object
    ## Global drawing settings.
    drawingArea*: Rect
    displayArea*: Option[Rect] ## \
      ## This is only set if the display area should be skipped.
    transparency*: TransparencyMode
    dither*: bool
    setMaskBit*: bool ## Force mask bit to 1 when drawing
    skipMaskedPixels*: bool ## Don't overwrite masked pixels

  Triangle* = object
    ## A triangle. At least one of 'colours' and 'texture' should be set.
    vertices*: array[3, Coord]
    colours*: Option[array[3, Colour]]
    texture*: Option[Texture[3]]

  Rectangle* = object
    ## A rectangle. At least one of 'colours' and 'texture' should be set.
    rect*: Rect
    colour*: Option[Colour]
    texture*: Option[Texture[1]]
    flipX*, flipY*: bool

  Line* = object
    ## A straight line.
    start*: tuple[x: int, y: int, colour: Colour]
    stop*: tuple[x: int, y: int, colour: Colour]

func `$`*(c: Colour): string =
  fmt"#{c.red:02x}{c.green:02x}{c.blue:02x}"

func `$`*[N: static int](tex: Texture[N]): string =
  result = fmt"({tex.page.x:04x}, {tex.page.y:04x})"
  result &= fmt", colour mode={tex.colourMode.depth}"
  if tex.colourMode.depth != FifteenBit:
    result &= fmt", palette=({tex.colourMode.palette.x}, {tex.colourMode.palette.y})"
  result &= ", coords="
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
  result &= fmt"({rect.rect.x1}, {rect.rect.x2})--({rect.rect.y1},{rect.rect.y2})"
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
