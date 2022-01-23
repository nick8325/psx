## The backend of the GPU - converts drawing commands into a framebuffer.

import utils
import std/[options, strformat, logging, sugar, algorithm]
import fusion/matching
{.experimental: "caseStmtMacros".}

var logger = newLogger("Rasteriser")

type
  Point* = tuple
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
    of FourBit, EightBit: palette*: Point
    of FifteenBit: discard

  Texture*[N: static int] = object
    ## A texture with N vertices.
    page*: Point ## x must be a multiple of 64, y a multiple of 256.
    windowMask*, windowOffset*: Point
    coords*: array[N, Point] ## relative to page.
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
    vertices*: array[3, Point]
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
  vram*: array[512, array[1024, Pixel]]

proc getPixel*(xIn, yIn: int): Pixel {.inline.} =
  let x = xIn mod 1024
  let y = yIn mod 512
  vram[y][x]

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
    logger.debug fmt"skip write to {x},{y} since out of drawing area {settings.drawingArea}"
    return
  # TODO in interlaced mode, only currently drawn part should be masked
  if x >= settings.displayArea.x1 and x < settings.displayArea.x2 and
     y >= settings.displayArea.y1 and y < settings.displayArea.y2:
    logger.debug fmt"skip write to {x},{y} since inside display area {settings.displayArea}"
    return

  # Check existing pixel's mask
  let oldPixel = getPixel(x, y)
  if settings.skipMaskedPixels and oldPixel.mask:
    logger.debug fmt"skip write to {x},{y} since masked"
    return

  # Force mask bit if requested
  var pixel = pixelIn
  if settings.setMaskBit: pixel.mask = true

  # Handle transparency
  template blend(c1, c2: int): int =
    var result =
      case settings.transparency
      of Mean: (c1+c2) div 2
      of Add: c1+c2
      of Subtract: c1-c2
      of AddQuarter: c1 + c2 div 4
      of Opaque: c2
    result = result.clamp(0, 31)
    result

  # Write pixel with possible transparency
  pixel.red5 = blend(oldPixel.red5, pixel.red5)
  pixel.green5 = blend(oldPixel.green5, pixel.green5)
  pixel.blue5 = blend(oldPixel.blue5, pixel.blue5)
  #logger.info fmt"setting vram {x},{y} to {uint16(pixel):04x}"
  vram[y][x] = pixel

import sdl2, sdl2/gfx
let surface = createRGBSurfaceFrom(addr vram, 1024, 512, 16, 2*1024, 0x1f, 0x1fu32 shl 5, 0x1fu32 shl 10, 0)

proc lineKeepingLeft(p1, p2: Point): seq[(Point, bool)] =
  ## Compute a line from p1 to p2 (using integer coordinates).
  ## If it is not possible to stay exactly on the line, keep
  ## to the left of it. Here left is interpreted relative to
  ## the direction of motion p1->p2.

  # Idea: at every step, move towards the line if we can,
  # otherwise move away. The directions of "towards" and
  # "away" depend on the orientation of the line, but are
  # always increasing/decreasing x or y by 1.

  let
    dx = abs (p2.x - p1.x)
    dy = abs (p2.y - p1.y)
    sx = signum (p2.x - p1.x)
    sy = signum (p2.y - p1.y)

  var
    p = p1
    err = 0

  # How to take steps depending on direction of line:
  # "/" - away: y += sy, towards: x += sx
  # "\" - away: x += sx, towards: y += sy
  # "|" - away: N/A, towards: y += sy (treat as "\")
  # "-" - away: N/A, towards: x += sx (treat as "/")

  if sx == sy or sx == 0: # Line of "\" or "|" shape
    while p != p2:
      result.add (p, err == 0)
      if err >= dx:
        p.y += sy
        err -= dx
      else:
        p.x += sx
        err += dy
  else: # Line of "/" or "-" shape
    while p != p2:
      result.add (p, err == 0)
      if err >= dy:
        p.x += sx
        err -= dy
      else:
        p.y += sy
        err += dx

  # Add endpoint
  result.add (p, true)

proc draw*(settings: Settings, tri: Triangle) =
  logger.debug fmt"draw {tri}"

  var colour: Colour
  case tri.colours
  of None(): colour = (red: 0u8, green: 0xffu8, blue: 0u8)
  of Some(@arr): colour = arr[0]

  var vs = tri.vertices

  # Put the topmost point at vs[0], using leftmost to break ties
  vs.sort cmpKey[Point]((p: Point) => (p.y, p.x))

  # Compare angle of two points relative to point vs[0].
  # Returns -1 if p1 is clockwise of p2, 1 if anticlockwise.
  proc cmpAngle(p1, p2: Point): int =
    let p0 = vs[0]
    # Return 1 if
    #     (x1-x0)/(y1-y0) < (x2-x0)/(y2-y0)
    # <=> (x1-x0)(y2-y0) < (x2-x0)(y1-y0) since y0 <= y1,y2
    # Also works if y2=y0 or y1=y0
    cmp((p1.x-p0.x)*(p2.y-p0.y), (p2.x-p0.x)*(p1.y-p0.y))

  # Special case: if all vertices are collinear, nothing should be drawn
  if cmpAngle(vs[1], vs[2]) == 0:
    logger.debug fmt"skip collinear triangle {tri}"
    return

  # Put vs[1] and vs[2] in order of angle, so that the movement
  # vs[0]->vs[1]->vs[2]->vs[0] goes anticlockwise around the triangle
  if cmpAngle(vs[1], vs[2]) > 0:
    swap(vs[1], vs[2])

  # Classify each line as either on the left or right side of the
  # triangle, or horizontal. Points exactly on a right-sided line of the
  # triangle are not drawn. Points on a horizontal bottom line of the triangle
  # should also not be drawn.
  type Kind = enum Left, Right, Horizontal
  var kinds: array[3, Kind]
  # vs[0] -> vs[1]
  kinds[0] = Left # cannot be horizontal, or all 3 points would be collinear
  # vs[1] -> vs[2]
  kinds[1] =
    if vs[1].y < vs[2].y: Left # going down from vs[1] to vs[2]
    elif vs[1].y > vs[2].y: Right # going up from vs[1] to vs[2]
    else: Horizontal # going straight along
  # vs[2] -> vs[0]
  kinds[2] = if vs[2].y == vs[0].y: Horizontal else: Right

  # Next step: figure out the minimum and maximum x-coordinate we should draw
  # for each y-coordinate.

  let ytop = vs[0].y
  let ybot = max(vs[1].y, vs[2].y)
  var mins, maxs: seq[int]
  for _ in ytop..ybot:
    mins.add int.high
    maxs.add int.low

  # We traverse the triangle anticlockwise, vs[0]->vs[1]->vs[2]->vs[0].
  # We convert each line into a set of points, but in order to stay inside the
  # triangle we only consider points that lie on the line or to the left of it
  # (relative to the direction of travel around the triangle). The computed
  # points are used to update mins or maxs depending on what kind of line it is:
  # * Left line: update mins
  # * Right line: update maxs. If the point lies exactly on the line, reduce
  #   x-coordinate by 1 first.
  # * Horizontal line: ignored - both endpoints must connect to another line,
  #   so we rely on those instead. If the line is at the bottom of the triangle,
  #   we don't draw the bottom row of the triangle.

  template update(pp: (Point, bool), kind: Kind) =
    var (p, onLine) = pp
    case kind
    of Left:
      if mins[p.y - ytop] > p.x: mins[p.y - ytop] = p.x
    of Right:
      if onLine: p.x -= 1
      if maxs[p.y - ytop] < p.x: maxs[p.y - ytop] = p.x
    of Horizontal: discard

  for p in lineKeepingLeft(vs[0], vs[1]): p.update(kinds[0])
  for p in lineKeepingLeft(vs[1], vs[2]): p.update(kinds[1])
  for p in lineKeepingLeft(vs[2], vs[0]): p.update(kinds[2])

  # Skip last line if vs[1]->vs[2] is horizontal (bottom line)
  for y in ytop..ybot - (if kinds[1] == Horizontal: 1 else: 0):
    for x in mins[y-ytop]..maxs[y-ytop]:
      putPixel(x, y, colour.toPixel, settings)
