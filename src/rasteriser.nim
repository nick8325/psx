## The backend of the GPU - converts drawing commands into a framebuffer.

import utils
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

  Pixel* = distinct uint16

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
    ## Common drawing settings.
    drawingArea*: Rect
    displayArea*: Rect
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

Pixel.bitfield red5, int, 0, 5
Pixel.bitfield green5, int, 5, 5
Pixel.bitfield blue5, int, 10, 5
Pixel.bitfield mask, bool, 15, 1

func toPixel(c: Colour, mask: bool = false): Pixel =
  result.red5 = int(c.red shr 3)
  result.green5 = int(c.green shr 3)
  result.blue5 = int(c.blue shr 3)
  result.mask = mask

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

var
  vram: array[1024, array[512, Pixel]]

proc getPixel*(xIn, yIn: int): Pixel {.inline.} =
  let x = xIn mod 1024
  let y = yIn mod 512
  vram[x][y]

proc putPixel*(xIn, yIn: int, pixelIn: Pixel, settings: Settings) {.inline.} =
  ## Put a pixel to the VRAM. Handles:
  ## * Mask bit
  ## * Transparency
  ## * Cropping to the drawing area
  ## * Skipping the display area
  ## Does not handle dithering yet.

  # TODO: speed up drawing

  # Handle wraparound
  let x = xIn mod 1024
  let y = yIn mod 512

  # Check pixel against drawing/display areas
  if x < settings.drawingArea.x1 or x >= settings.drawingArea.x2 or
     y < settings.drawingArea.y1 or y >= settings.drawingArea.y2:
    return
  if x >= settings.displayArea.x1 and x < settings.displayArea.x2 and
     y >= settings.displayArea.y1 and y < settings.displayArea.y2:
    return

  # Check existing pixel's mask
  let oldPixel = getPixel(x, y)
  if settings.skipMaskedPixels and oldPixel.mask:
    return

  # Force mask bit if requested
  var pixel = pixelIn
  if settings.setMaskBit: pixel.mask = true

  # Handle transparency
  func blend(c1, c2: int): int =
    result =
      case settings.transparency
      of Mean: (c1+c2) div 2
      of Add: c1+c2
      of Subtract: c1-c2
      of AddQuarter: c1 + c2 div 4
      of Opaque: c2
    result = result.clamp(0, 31)

  # Write pixel with possible transparency
  pixel.red5 = blend(oldPixel.red5, pixel.red5)
  pixel.green5 = blend(oldPixel.green5, pixel.green5)
  pixel.blue5 = blend(oldPixel.blue5, pixel.blue5)
  vram[x][y] = pixel

proc draw*(settings: Settings, x: Triangle) =
  echo fmt"draw {x}"
