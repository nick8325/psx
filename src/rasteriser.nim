## The backend of the GPU - converts drawing commands into a framebuffer.

import utils, basics
import std/[options, strformat, logging, sugar, algorithm]
import glm

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
    skipLines*: Option[bool] # false=skip even lines, true=skip odd lines
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

  Pixel32 = distinct uint32

Pixel.bitfield red5, int, 0, 5
Pixel.bitfield green5, int, 5, 5
Pixel.bitfield blue5, int, 10, 5
Pixel.bitfield mask, bool, 15, 1

Pixel32.bitfield red, int, 0, 8
Pixel32.bitfield green, int, 8, 8
Pixel32.bitfield blue, int, 16, 8
Pixel32.bitfield mask, bool, 24, 1

func toPixel(c: Colour, mask: bool = false): Pixel =
  result.red5 = int(c.red shr 3)
  result.green5 = int(c.green shr 3)
  result.blue5 = int(c.blue shr 3)
  result.mask = mask

func toPixel(p: Pixel32): Pixel =
  result.red5 = int(p.red shr 3)
  result.green5 = int(p.green shr 3)
  result.blue5 = int(p.blue shr 3)
  result.mask = p.mask

func toPixel32(c: Colour, mask: bool = false): Pixel32 =
  result.red = int(c.red)
  result.green = int(c.green)
  result.blue = int(c.blue)
  result.mask = mask

func toPixel32(p: Pixel): Pixel32 =
  result.red = p.red5 * 8
  result.green = p.green5 * 8
  result.blue = p.blue5 * 8
  result.mask = p.mask

func `$`*(c: Colour): string =
  fmt"#{c.red:02x}{c.green:02x}{c.blue:02x}"

func `$`*[N: static int](tex: Texture[N]): string =
  result = fmt"page ({tex.page.x:04x}, {tex.page.y:04x})"
  result &= fmt", window mask ({tex.windowMask.x:04x}, {tex.windowMask.y:04x})"
  result &= fmt", window offset ({tex.windowOffset.x:04x}, {tex.windowOffset.y:04x})"
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
  vram32*: array[512, array[1024, Pixel32]]

proc getPixel*(xIn, yIn: int): Pixel {.inline.} =
  let x = xIn mod 1024
  let y = yIn mod 512
  vram[y][x]

proc getPixel32*(xIn, yIn: int): Pixel32 {.inline.} =
  let x = xIn mod 1024
  let y = yIn mod 512
  vram32[y][x]

proc putPixel*(xIn, yIn: int, pixelIn: Pixel32, settings: Settings) {.inline.} =
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
  if settings.skipLines.isSome:
    if settings.skipLines.get.int == (y and 1):
      logger.debug fmt"skip write to {x},{y} in interlaced mode"
      return

  # Check existing pixel's mask
  let oldPixel = getPixel32(x, y)
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
    result = result.clamp(0, 255)
    result

  # Write pixel with possible transparency
  pixel.red = blend(oldPixel.red, pixel.red)
  pixel.green = blend(oldPixel.green, pixel.green)
  pixel.blue = blend(oldPixel.blue, pixel.blue)
  #logger.info fmt"setting vram {x},{y} to {uint16(pixel):04x}"
  vram[y][x] = pixel.toPixel
  vram32[y][x] = pixel

proc putPixel*(xIn, yIn: int, pixelIn: Pixel, settings: Settings) {.inline.} =
  putPixel(xIn, yIn, pixelIn.toPixel32, settings)

proc putPixel*(xIn, yIn: int, c: Colour, settings: Settings) {.inline.} =
  putPixel(xIn, yIn, c.toPixel32, settings)

type
  Interpolator = distinct Vec3d

func makeInterpolator[T](inputs: array[3, Point],
                         outputs: array[3, T]): Interpolator =
  # Call the inputs x1,y1..x3,y3, and the outputs z1..z3
  # Define M = [x1 y1 1; x2 y2 1; x3 y3 1] and N = [z1; z2; z3].
  # Then we are looking for A such that MA = N, that is, A = M^-1 N.

  # Note: mat3 takes a list of column vectors.
  let M = mat3(vec3(inputs[0].x.float64, inputs[1].x.float64, inputs[2].x.float64),
               vec3(inputs[0].y.float64, inputs[1].y.float64, inputs[2].y.float64),
               vec3(1.float64, 1, 1))
  let N = vec3(outputs[0].float64, outputs[1].float64, outputs[2].float64)
  Interpolator(M.inverse * N)

func interpolate(interpolator: Interpolator, p: Point): float64 =
  let M = vec3(p.x.float64, p.y.float64, 1)
  # Multiplication, viewing M as a 3x1 matrix
  M.dot(interpolator.Vec3d)

func step(i: Interpolator): tuple[x, y: float64] =
  (x: i.interpolate((x: 1, y: 0)) - i.interpolate((x: 0, y: 0)),
   y: i.interpolate((x: 0, y: 1)) - i.interpolate((x: 0, y: 0)))

iterator lineKeepingLeft(p1, p2: Point): (Point, bool) =
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
      yield (p, err == 0)
      if err >= dx:
        p.y += sy
        err -= dx
      else:
        p.x += sx
        err += dy
  else: # Line of "/" or "-" shape
    while p != p2:
      yield (p, err == 0)
      if err >= dy:
        p.x += sx
        err -= dy
      else:
        p.y += sy
        err += dx

  # Add endpoint
  yield (p, true)

proc draw*(settings: Settings, tri: Triangle) =
  logger.debug fmt"draw {tri}"

  var colours: array[3, Colour]
  if tri.colours.isSome(): colours = tri.colours.get()

  var texture: Texture[3]
  if tri.texture.isSome(): texture = tri.texture.get()

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

  var ytop = vs[0].y
  var ybot = max(vs[1].y, vs[2].y)

  # Skip last line if vs[1]->vs[2] is horizontal (bottom line)
  if kinds[1] == Horizontal: ybot -= 1

  # Clamp to drawing area
  ytop = max(ytop, settings.drawingArea.y1)
  ybot = min(ybot, settings.drawingArea.y2)

  # Use a global variable to avoid an allocation for each triangle
  var mins, maxs {.global.}: array[vramHeight, int]
  for i in ytop..ybot:
    mins[i] = int.high
    maxs[i] = int.low

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
      if p.y >= ytop and p.y <= ybot and mins[p.y] > p.x: mins[p.y] = p.x
    of Right:
      if onLine: p.x -= 1
      if p.y >= ytop and p.y <= ybot and maxs[p.y] < p.x: maxs[p.y] = p.x
    of Horizontal: discard

  for p in lineKeepingLeft(vs[0], vs[1]): p.update(kinds[0])
  for p in lineKeepingLeft(vs[1], vs[2]): p.update(kinds[1])
  for p in lineKeepingLeft(vs[2], vs[0]): p.update(kinds[2])

  logger.debug fmt"triangle, vs: {vs}, range: {ytop}--{ybot}, horiz: {kinds[1] == Horizontal}"
  logger.debug fmt"mins: {mins[ytop..ybot]}"
  logger.debug fmt"maxs: {maxs[ytop..ybot]}"

  let red = makeInterpolator(tri.vertices, [colours[0].red, colours[1].red, colours[2].red])
  let green = makeInterpolator(tri.vertices, [colours[0].green, colours[1].green, colours[2].green])
  let blue = makeInterpolator(tri.vertices, [colours[0].blue, colours[1].blue, colours[2].blue])
  let tx = makeInterpolator(tri.vertices, [texture.coords[0].x, texture.coords[1].x, texture.coords[2].x])
  let ty = makeInterpolator(tri.vertices, [texture.coords[0].y, texture.coords[1].y, texture.coords[2].y])

  for y in ytop..ybot:
    for x in mins[y]..maxs[y]:
      let p = (x: x, y: y)
      var colour: Colour
      colour.red = red.interpolate(p).clamp(0, 255).uint8
      colour.green = green.interpolate(p).clamp(0, 255).uint8
      colour.blue = blue.interpolate(p).clamp(0, 255).uint8

      if tri.texture.isSome():
        # Coordinates relative to texture page
        var xtex = tx.interpolate(p).int mod 256
        var ytex = ty.interpolate(p).int mod 256
        xtex = (xtex and not (texture.windowMask.x * 8)) or ((texture.windowOffset.x and texture.windowMask.x)*8)
        ytex = (ytex and not (texture.windowMask.y * 8)) or ((texture.windowOffset.y and texture.windowMask.y)*8)

        # Now look up pixel data
        var textureColour =
          case texture.colourMode.depth:
          of FifteenBit: getPixel32(texture.page.x + xtex, texture.page.y + ytex)
          of EightBit:
            let val = getPixel(texture.page.x + xtex div 2, texture.page.y + ytex)
            let palette = texture.colourMode.palette
            if xtex mod 2 == 0:
              getPixel32(palette.x + (val.int and 0xff), palette.y)
            else:
              getPixel32(palette.x + (val.int shr 8), palette.y)
          of FourBit:
            let val = getPixel(texture.page.x + xtex div 4, texture.page.y + ytex)
            let palette = texture.colourMode.palette
            if xtex mod 4 == 0:
              getPixel32(palette.x + (val.int and 0xf), palette.y)
            elif xtex mod 4 == 1:
              getPixel32(palette.x + ((val.int shr 4) and 0xf), palette.y)
            elif xtex mod 4 == 2:
              getPixel32(palette.x + ((val.int shr 8) and 0xf), palette.y)
            else:
              getPixel32(palette.x + ((val.int shr 12) and 0xf), palette.y)

        if textureColour.uint16 != 0:
          if tri.colours.isSome():
            textureColour.red = (colour.red.int * textureColour.red) div 128
            textureColour.green = (colour.green.int * textureColour.green) div 128
            textureColour.blue = (colour.blue.int * textureColour.blue) div 128

          putPixel(x, y, textureColour, settings)
      else:
        putPixel(x, y, colour, settings)
