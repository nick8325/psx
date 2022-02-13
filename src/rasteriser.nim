## The backend of the GPU - converts drawing commands into a framebuffer.

import utils, basics
import std/[options, strformat, sugar, algorithm]
import glm

const loggerComponent = logRasteriser

# Type definitions for the rendering primitives.

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
    red: int
    green: int
    blue: int

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

  ShadingMode* {.pure.} = enum
    Colours, Textures, Both

  Triangle* = object
    ## A triangle. At least one of 'colours' and 'texture' should be set.
    vertices*: array[3, Point]
    shadingMode*: ShadingMode
    colours*: array[3, Colour] # Only used if shadingMode is Colour or Both
    texture*: Texture[3] # Only used if shadingMode is Texture or Both

  Rectangle* = object
    ## A rectangle. At least one of 'colours' and 'texture' should be set.
    rect*: Rect
    shadingMode*: ShadingMode
    colour*: Colour # Only used if shadingMode is Colour or Both
    texture*: Texture[1] # Only used if shadingMode is Texture or Both
    flipX*, flipY*: bool

  Line* = object
    ## A straight line.
    start*: tuple[point: Point, colour: Colour]
    stop*: tuple[point: Point, colour: Colour]

  Settings* = object
    ## Drawing settings common to all primitives.
    drawingArea*: Rect
    skipLines*: Option[bool] ## false=skip even lines, true=skip odd lines
    transparency*: TransparencyMode
    dither*: bool ## Fake dithering by using 24-bit colour.
    setMaskBit*: bool ## Force mask bit to 1 when drawing
    skipMaskedPixels*: bool ## Don't overwrite masked pixels

func `+`*(p1, p2: Point): Point =
  result.x = p1.x + p2.x
  result.y = p1.y + p2.y

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
  if tri.shadingMode != Textures:
    result &= fmt", colours "
    for i, c in tri.colours:
      if i > 0: result &= "--"
      result &= $c
  if tri.shadingMode != Colours:
    result &= fmt", texture {tri.texture}"

func `$`*(rect: Rectangle): string =
  result = "rectangle, "
  result &= fmt"({rect.rect.x1}, {rect.rect.x2})--({rect.rect.y1},{rect.rect.y2})"
  if rect.shadingMode != Textures:
    result &= fmt", colour {rect.colour}"
  if rect.shadingMode != Colours:
    result &= fmt", texture {rect.texture}"
  if rect.flipX:
    result &= ", x flipped"
  if rect.flipY:
    result &= ", y flipped"

# Pixel formats.
# We use 24-bit colour internally, and convert to PSX format on demand.

type
  Pixel* = distinct uint32 ##\
    ## A 24-bit colour plus semi-transparency mask bit,
    ## ready for storing to VRAM.

  Pixel16* = distinct uint16 ##\
    ## A 16-bit colour in the native PSX format.

Pixel.bitfield red, int, 0, 8
Pixel.bitfield green, int, 8, 8
Pixel.bitfield blue, int, 16, 8
Pixel.bitfield mask, bool, 24, 1

Pixel16.bitfield red5, int, 0, 5
Pixel16.bitfield green5, int, 5, 5
Pixel16.bitfield blue5, int, 10, 5
Pixel16.bitfield mask, bool, 15, 1

func toPixel(c: Colour, mask: bool = false): Pixel =
  result.red = int(c.red)
  result.green = int(c.green)
  result.blue = int(c.blue)
  result.mask = mask

func toPixel(p: Pixel16): Pixel =
  result.red = p.red5 * 8
  result.green = p.green5 * 8
  result.blue = p.blue5 * 8
  result.mask = p.mask

func toPixel16(p: Pixel): Pixel16 =
  result.red5 = int(p.red shr 3)
  result.green5 = int(p.green shr 3)
  result.blue5 = int(p.blue shr 3)
  result.mask = p.mask

func toColour(p: Pixel): Colour =
  result.red = p.red
  result.green = p.green
  result.blue = p.blue

# Video RAM.

var
  vram*: array[512, array[1024, Pixel]]

proc getPixel*(x, y: int): Pixel {.inline.} =
  ## Read a pixel from the VRAM.

  vram[y and (vramHeight-1)][x and (vramWidth-1)]

proc getHalfword*(x, y: int): int {.inline.} =
  ## Read halfword data from the VRAM.

  getPixel(x, y).toPixel16.int

proc getByte*(x, y: int): int {.inline.} =
  ## Read byte data from the VRAM.

  let val = getHalfword(x div 2, y)
  if (x and 1) == 0:
    return val and 0xff
  else:
    return val shr 8

proc getNybble*(x, y: int): int {.inline.} =
  ## Read nybble data from the VRAM.

  let val = getHalfword(x div 4, y)
  case x and 3
  of 0: return val and 0xf
  of 1: return (val shr 4) and 0xf
  of 2: return (val shr 8) and 0xf
  of 3: return (val shr 12) and 0xf
  else: assert false

proc putPixel*(x, y: int, pixel: Pixel, settings: Settings) {.inline.} =
  ## Put a pixel to the VRAM. Handles:
  ## * Mask bit
  ## * Transparency
  ## * Cropping to the drawing area
  ## * Skipping the display area
  ## * Fake dithering

  # TODO: speed up drawing

  # Check pixel against drawing/display areas
  if x < 0 or x >= vramWidth or y < 0 or y >= vramHeight:
    trace fmt"skip write to {x},{y} since out of VRAM"
  if x < settings.drawingArea.x1 or x >= settings.drawingArea.x2 or
     y < settings.drawingArea.y1 or y >= settings.drawingArea.y2:
    trace fmt"skip write to {x},{y} since out of drawing area {settings.drawingArea}"
    return
  if settings.skipLines.isSome:
    if settings.skipLines.get.int == (y and 1):
      trace fmt"skip write to {x},{y} in interlaced mode"
      return

  # Check existing pixel's mask
  let oldPixel = getPixel(x, y)
  if settings.skipMaskedPixels and oldPixel.mask:
    trace fmt"skip write to {x},{y} since masked"
    return

  # Force mask bit if requested
  var finalPixel = pixel
  if settings.setMaskBit: finalPixel.mask = true

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
  finalPixel.red = blend(oldPixel.red, pixel.red)
  finalPixel.green = blend(oldPixel.green, pixel.green)
  finalPixel.blue = blend(oldPixel.blue, pixel.blue)

  # We interpret "dither" as "draw in full 24-bit colour"
  if not settings.dither: finalPixel = finalPixel.toPixel16.toPixel

  vram[y and (vramHeight-1)][x and (vramWidth-1)] = finalPixel

proc putPixel*(x, y: int, c: Colour, settings: Settings) {.inline.} =
  ## Put a pixel to the VRAM.

  putPixel(x, y, c.toPixel, settings)

proc putHalfword*(x, y: int, value: int, settings: Settings) {.inline.} =
  ## Write halfword data to the VRAM.

  putPixel(x, y, value.Pixel16.toPixel, settings)

# Shading and texture mapping.

type Interpolator = distinct Vec3d ## Does bilinear interpolation

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

type
  ColourInterpolator = object
    red, green, blue: Interpolator
  PointInterpolator = object
    x, y: Interpolator

func makeInterpolator(inputs: array[3, Point], outputs: array[3, Colour]): ColourInterpolator =
  ColourInterpolator(
    red: inputs.makeInterpolator [outputs[0].red, outputs[1].red, outputs[2].red],
    green: inputs.makeInterpolator [outputs[0].green, outputs[1].green, outputs[2].green],
    blue: inputs.makeInterpolator [outputs[0].blue, outputs[1].blue, outputs[2].blue])

func makeInterpolator(inputs: array[3, Point], outputs: array[3, Point]): PointInterpolator =
  PointInterpolator(
    x: inputs.makeInterpolator [outputs[0].x, outputs[1].x, outputs[2].x],
    y: inputs.makeInterpolator [outputs[0].y, outputs[1].y, outputs[2].y])

func interpolate(i: ColourInterpolator, p: Point): Colour =
  (red: i.red.interpolate(p).int.clamp(0, 255),
   green: i.green.interpolate(p).int.clamp(0, 255),
   blue: i.blue.interpolate(p).int.clamp(0, 255))

func interpolate(i: PointInterpolator, p: Point): Point =
  (x: i.x.interpolate(p).int, y: i.y.interpolate(p).int)

proc getPixel[N: static int](texture: Texture[N]; x, y: int): Pixel =
  ## Look up a pixel coordinate in a texture.

  let mask = texture.windowMask
  let offset = texture.windowOffset

  let xt = ((x and 0xff) and not (mask.x * 8)) or ((offset.x and mask.x)*8)
  let yt = ((y and 0xff) and not (mask.y * 8)) or ((offset.y and mask.y)*8)

  case texture.colourMode.depth:
  of FifteenBit: getPixel(texture.page.x + xt, texture.page.y + yt)
  of EightBit:
    let val = getByte(texture.page.x*2 + xt, texture.page.y + yt)
    let palette = texture.colourMode.palette
    getPixel(palette.x + val, palette.y)
  of FourBit:
    let val = getNybble(texture.page.x*4 + xt, texture.page.y + yt)
    let palette = texture.colourMode.palette
    getPixel(palette.x + val, palette.y)

func mix(c1, c2: Colour): Colour =
  ## Mix two colours the same way as a shaded texture does it.
  result.red = (c1.red * c2.red) div 128
  result.green = (c1.green * c2.green) div 128
  result.blue = (c1.blue * c2.blue) div 128

func mix(p: Pixel, c: Colour): Pixel =
  ## Mix a Gouraud-shaded colour into a texture-shaded pixel.

  p.toColour.mix(c).toPixel(p.mask)

proc putTexturePixel(x, y: int; textureColour: Pixel; settings: Settings) =
  ## Put a pixel that came from a texture.

  # Black is transparent
  if textureColour.uint16 != 0:
      # Bit 1 unset means always opaque
      var newSettings = settings
      if not textureColour.mask: newSettings.transparency = Opaque
      putPixel(x, y, textureColour, newSettings)

# Triangle drawing.

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
  ## Draw a triangle.

  debug fmt"draw {tri}"

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
  ytop = max(ytop, settings.drawingArea.y1).max(0)
  ybot = min(ybot, settings.drawingArea.y2).min(vramHeight-1)

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

  trace fmt"triangle, vs: {vs}, range: {ytop}--{ybot}, horiz: {kinds[1] == Horizontal}"
  trace fmt"mins: {mins[ytop..ybot]}"
  trace fmt"maxs: {maxs[ytop..ybot]}"

  let shader = tri.vertices.makeInterpolator tri.colours
  let textureMapper = tri.vertices.makeInterpolator tri.texture.coords

  for y in ytop..ybot:
    for x in mins[y]..maxs[y]:
      let p = (x: x, y: y)

      case tri.shadingMode
      of Colours:
        let colour = shader.interpolate p
        putPixel(x, y, colour, settings)
      of Textures:
        let coord = textureMapper.interpolate p
        putTexturePixel(x, y, getPixel(tri.texture, coord.x, coord.y), settings)
      of Both:
        let colour = shader.interpolate p
        let coord = textureMapper.interpolate p
        putTexturePixel(x, y, getPixel(tri.texture, coord.x, coord.y).mix(colour), settings)

proc draw*(settings: Settings, rect: Rectangle) =
  ## Draw a rectangle.

  debug fmt"draw {rect}"

  for y in rect.rect.y1..<rect.rect.y2:
    for x in rect.rect.x1..<rect.rect.x2:
      let tx =
        if rect.flipX: rect.texture.coords[0].x - x + rect.rect.x1
        else: rect.texture.coords[0].x + x - rect.rect.x1
      let ty =
        if rect.flipY: rect.texture.coords[0].y - y + rect.rect.y1
        else: rect.texture.coords[0].y + y - rect.rect.y1

      case rect.shadingMode
      of Colours:
        putPixel(x, y, rect.colour, settings)
      of Textures:
        putTexturePixel(x, y, getPixel(rect.texture, tx, ty), settings)
      of Both:
        putTexturePixel(x, y, getPixel(rect.texture, tx, ty).mix(rect.colour), settings)

proc fill*(settings: Settings; x, y, w, h: int; c: Colour) =
  ## Fill a rectangle with a solid colour.

  debug fmt"fill ({x},{y}) size ({w},{h}) colour {c}"

  for j in y..<y+h:
    for i in x..<x+w:
      putPixel(i, j, c, settings)

proc draw*(settings: Settings, line: Line) =
  ## Draw a line.

  debug fmt"draw {line}"
