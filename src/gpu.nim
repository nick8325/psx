## The GPU.

import basics, utils, irq, eventqueue, rasteriser, savestates
import std/[bitops, strformat, deques, options]

const loggerComponent = logGPU

type
  DotclockMultiplier {.pure.} = enum
    Dot10 = 0,
    Dot8 = 1,
    Dot5 = 2,
    Dot4 = 3,
    Dot7 = 4
  VerticalRes {.pure.} = enum
    ResNormal = 0,
    ResDouble = 1
  ColourDepth {.pure.} = enum
    Depth15 = 0,
    Depth24 = 1
  DMADirection {.pure.} = enum
    Off = 0,
    Unknown = 1,
    Write = 2,
    Read = 3
  Vertex = distinct word
  ScreenCoord = distinct word
  Colour = distinct word
  TexPage = distinct uint16
  TextureWindow = distinct word
  PackedMiniCoord = distinct word
  PackedCoord = distinct word
  PackedScreenCoord = distinct word
  PackedSignedCoord = distinct word
  Palette = distinct uint16
  TexCoord = distinct uint16

Vertex.bitfield x, int, 0, 11, signed=true
Vertex.bitfield y, int, 16, 11, signed=true

func unpack(v: Vertex): Point =
  (x: v.x, y: v.y)

ScreenCoord.bitfield x, int, 0, 10
ScreenCoord.bitfield y, int, 16, 9

func unpack(v: ScreenCoord): Point =
  (x: v.x, y: v.y)

Colour.bitfield red, int, 0, 8
Colour.bitfield green, int, 8, 8
Colour.bitfield blue, int, 16, 8

func unpack(c: Colour): rasteriser.Colour =
  (red: c.red, green: c.green, blue: c.blue)

TexPage.bitfield baseX64, int, 0, 4
TexPage.bitfield baseY256, int, 4, 1
TexPage.bitfield transparency, TransparencyMode, 5, 2
# int not TextureColourMode, because the value could be invalid
TexPage.bitfield colours, int, 7, 2
TexPage.bitfield dither, bool, 9, 1
TexPage.bitfield drawToDisplayArea, bool, 10, 1
TexPage.bitfield textureDisable, bool, 11, 1
TexPage.bitfield flipX, bool, 12, 1
TexPage.bitfield flipY, bool, 13, 1

func base(page: TexPage): Point =
  (x: page.baseX64 * 64, y: page.baseY256 * 256)

TextureWindow.bitfield mask, PackedMiniCoord, 0, 10
TextureWindow.bitfield offset, PackedMiniCoord, 10, 10

PackedMiniCoord.bitfield x8, int, 0, 5
PackedMiniCoord.bitfield y8, int, 5, 5

func unpack(c: PackedMiniCoord): Point =
  (x: c.x8 * 8, y: c.y8 * 8)

PackedCoord.bitfield x, int, 0, 10
PackedCoord.bitfield y, int, 10, 10

func unpack(c: PackedCoord): Point =
  (x: c.x, y: c.y)

# Absolute screen coordinates seem to get masked to 0<=y<512
PackedScreenCoord.bitfield x, int, 0, 10
PackedScreenCoord.bitfield y, int, 10, 9

func unpack(c: PackedScreenCoord): Point =
  (x: c.x, y: c.y)

PackedSignedCoord.bitfield x, int, 0, 11, signed=true
PackedSignedCoord.bitfield y, int, 11, 11, signed=true

func unpack(c: PackedSignedCoord): Point =
  (x: c.x, y: c.y)

TexCoord.bitfield x, int, 0, 8
TexCoord.bitfield y, int, 8, 8

func unpack(c: TexCoord): Point =
  (x: c.x, y: c.y)

Palette.bitfield x16, int, 0, 6
Palette.bitfield y, int, 6, 9

func unpack(p: Palette): Point =
  (x: p.x16 * 16, y: p.y)

type
  # Texture settings
  TextureSettings = object
    # x is multiplied by 64, y by 256
    base: tuple[x64: int, y256: int] # GPUSTAT 0-4
    # In 8-pixel steps
    window: TextureWindow            # GP0(E2h)
    colourDepth: TextureColourDepth  # GPUSTAT 7-8
    rawColourDepth: int              # GPUSTAT 7-8, unclamped value
    enabled: bool                    # GPUSTAT 15
    allowDisable: bool               # GP1(09h)
    flip: tuple[x: bool, y: bool]    # GP0(E1h).12-13

  # Drawing (to framebuffer) settings
  DrawingSettings = object
    transparency: TransparencyMode   # GPUSTAT 5-6
    setMaskBit: bool                 # GPUSTAT 11
    skipMaskedPixels: bool           # GPUSTAT 12
    drawToDisplayArea: bool          # GPUSTAT 10
    dither: bool                     # GPUSTAT 9
    drawingAreaTopLeft: PackedScreenCoord  # GP0(E3h)
    drawingAreaBottomRight: PackedScreenCoord # GP0(E4h)
    drawingAreaOffset: PackedSignedCoord # GP0(E5h)

  # Screen settings
  ScreenSettings = object
    displayAreaStart: PackedScreenCoord # GP1(05h)
    displayAreaDepth: ColourDepth    # GPUSTAT 21
    dotclockMultiplier: DotclockMultiplier   # GPUSTAT 16-18
    verticalRes: VerticalRes       # GPUSTAT 19
    verticalInterlace: bool        # GPUSTAT 22
    enabled: bool                  # GPUSTAT 23
    framesDrawn: int               # Number of frames drawn since boot
    frameNumber: int               # Current frame number
    # N.B. framesDrawn increases at the start of vblank,
    # frameNumber during vsync (in the middle of vblank)
    vblank: bool                   # Currently in vblank interval?
    # In GPU clocks
    horizontalRange: tuple[start: int, stop: int]
    # In scanlines - need to double in interlaced mode
    verticalRange: tuple[start: int, stop: int]

  ControlSettings = object
    interruptRequested: bool       # GPUSTAT 24
    dmaDirection: DMADirection     # GPUSTAT 29-30
    result: word                   # GPUREAD

var
  textures {.saved.} = TextureSettings(enabled: true)
  drawing {.saved.} = DrawingSettings()
  screen {.saved.} = ScreenSettings(vblank: true)
  control {.saved.} = ControlSettings()

# Screen settings, including dot-clocks and other such analogue stuff

const
  scanlinesPerFrame: array[Region, int64] =
    [NTSC: 263i64, PAL: 314]
  gpuClocksPerScanline: array[Region, int64] =
    [NTSC: 3413i64, PAL: 3406]
  gpuClocksPerDotClock: array[DotclockMultiplier, int64] =
    [Dot10: 10i64, Dot8: 8, Dot5: 5, Dot4: 4, Dot7: 7]

var
  lastVBlankStart {.saved.}: int64 = 0
  lastRegion {.saved.}: Region = region

proc clocksPerPixel*: int64 {.inline.} =
  ## Clock cycles per pixel drawn.

  gpuClock * gpuClocksPerDotClock[screen.dotclockMultiplier]
  # TODO: support 480p mode?
  # (PSX hardware supports it if connected to VGA monitor)

proc clocksPerScanline*(region: Region): int64 {.inline.} =
  ## Clock cycles per scanline.

  gpuClock * gpuClocksPerScanline[region]

proc clocksPerScanline*: int64 {.inline.} =
  ## Clock cycles per scanline.
  
  clocksPerScanline(region)

proc clocksPerFrame*(region: Region): int64 {.inline.} =
  ## Clock cycles per screen refresh.

  clocksPerScanline(region) * scanlinesPerFrame[region]

proc clocksPerFrame*: int64 {.inline.} =
  ## Clock cycles per screen refresh.

  clocksPerFrame(region)

proc maxScreenWidth: int64 =
  ## Maximum permissible screen width.

  gpuClocksPerScanline[region] div gpuClocksPerDotClock[screen.dotClockMultiplier]

proc screenWidth*: int =
  ## Width of screen in pixels.

  # Info comes from Nocash PSX
  let clocks = screen.horizontalRange.stop - screen.horizontalRange.start
  result = clocks div gpuClocksPerDotClock[screen.dotClockMultiplier].int
  result = (result + 2) and not 3
  result = clamp(result, 1, maxScreenWidth().int-1)

proc visibleScanlines*: int =
  ## Number of scanlines drawn per frame.
  ## In interlaced mode, this is half the screen height.

  clamp(screen.verticalRange.stop - screen.verticalRange.start,
        1, scanlinesPerFrame[region].int-1)

proc screenHeight*: int =
  ## Height of screen in pixels.

  visibleScanlines() * (if screen.verticalInterlace and screen.verticalRes == ResDouble: 2 else: 1)

# TODO: measure HBLANK/VBLANK timings on a real console

proc hblankClocks*: int64 {.inline.} =
  ## Clock cycles taken by one hblank.

  clocksPerPixel() * (maxScreenWidth() - screenWidth())

proc vblankClocks*: int64 {.inline.} =
  ## Clock cycles taken by one vblank.

  clocksPerScanline() * (scanlinesPerFrame[region] - visibleScanlines())

proc preVsyncClocks*: int64 {.inline.} =
  ## Clock cycles from start of vblank to vsync.

  clocksPerScanline() *
  clamp(scanlinesPerFrame[region].int - screen.verticalRange.stop,
        1, scanlinesPerFrame[region].int-1)

proc lastVBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles since the last VBLANK started.

  result = events.now() - lastVBlankStart
  assert result >= 0 and result < clocksPerFrame(lastRegion)

proc nextVBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles until the next VBLANK starts.

  result = clocksPerFrame(lastRegion) - lastVBlankDelta()
  assert result > 0 and result <= clocksPerFrame(lastRegion)

proc inVBlank*(): bool {.inline.} =
  ## Are we currently in the VBLANK interval?

  lastVBlankDelta() < vblankClocks()

proc remainingVBlankClocks*(): int64 {.inline.} =
  ## How many clock cycles remain of the current VBLANK interval?
  ## Returns 0 if not in VBLANK.

  return max(vblankClocks() - lastVBlankDelta(), 0)

proc lastHBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles since the last HBLANK started.

  # "The hblank signal is generated even during vertical blanking/retrace."
  result = lastVBlankDelta() mod clocksPerScanline(lastRegion)
  assert result >= 0 and result < clocksPerScanline(lastRegion)

proc nextHBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles until the next HBLANK starts.

  result = clocksPerScanline(lastRegion) - lastHBlankDelta()
  assert result > 0 and result <= clocksPerScanline(lastRegion)

proc inHBlank*(): bool {.inline.} =
  ## Are we currently in the HBLANK interval?

  lastHBlankDelta() < hblankClocks()

proc remainingHBlankClocks*(): int64 {.inline.} =
  ## How many clock cycles remain of the current HBLANK interval?
  ## Returns 0 if not in HBLANK.

  return max(hblankClocks() - lastHBlankDelta(), 0)

proc currentFrame*(): int64 {.inline.} =
  ## The current frame being drawn.

  screen.frameNumber

proc currentScanline*(): int64 {.inline.} =
  ## The current scanline number being drawn.

  result = lastVBlankDelta() div clocksPerScanline(lastRegion)
  assert result >= 0 and result < scanlinesPerFrame[lastRegion]

proc screenSettings*(): string =
  ## Dump the screen settings.

  result = fmt"{screenWidth()}x{screenHeight()}, {region}"
  if screen.verticalInterlace: result &= " interlaced"
  result &= fmt", hblank {hblankClocks()}"
  result &= fmt", vblank {vblankClocks()}"

proc onHBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextHBlankDelta(), name, p)

proc afterHBlank*(name: string, p: proc()) =
  onHBlank(name) do ():
     events.after(hblankClocks(), name, p)

proc onVBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextVBlankDelta(), name, p)

proc onVSync*(name: string, p: proc()) =
  onVBlank(name) do ():
     events.after(preVsyncClocks(), name, p)

proc afterVBlank*(name: string, p: proc()) =
  onVBlank(name) do ():
     events.after(vblankClocks(), name, p)

# VBLANK IRQ
onVBlank("gpu vblank") do ():
  debug "VBLANK"
  lastVBlankStart = events.now()
  lastRegion = region
  screen.framesDrawn += 1
  screen.vblank = true
  irqs.signal 0

  # PSX supports 480p if hooked up to a VGA monitor, but we don't
  if screen.verticalRes == ResDouble and not screen.verticalInterlace:
    warn "480p mode not supported"

onVSync("gpu vsync") do ():
  debug "VSYNC"
  screen.frameNumber += 1

afterVBlank("gpu end vblank") do ():
  debug "END VBLANK"
  screen.vblank = false

proc displayArea*: Rect =
  result.x1 = screen.displayAreaStart.x
  result.y1 = screen.displayAreaStart.y
  result.x2 = result.x1 + screenWidth() - 1
  result.y2 = result.y1 + screenHeight() - 1

proc renderedLines*: Option[bool] =
  if screen.verticalRes == ResDouble and
    screen.verticalInterlace:
    result = some(screen.frameNumber.testBit(0))

# Helper functions

proc rasteriserSettings(transparent: bool, dither: bool, crop: bool, interlace: bool): rasteriser.Settings =
  if crop:
    result.drawingArea =
      (x1: drawing.drawingAreaTopLeft.x,
       y1: drawing.drawingAreaTopLeft.y,
       x2: drawing.drawingAreaBottomRight.x+1,
       y2: drawing.drawingAreaBottomRight.y+1)

  else:
      result.drawingArea = (x1: 0, y1: 0, x2: vramWidth, y2: vramHeight)

  if interlace:
    if screen.verticalRes == ResDouble and
      screen.verticalInterlace and
      not drawing.drawToDisplayArea:
      # During vblank, GPU considers itself to be rendering even lines
      result.skipLines = some(screen.frameNumber.testBit(0) and not screen.vblank)

  result.transparency =
    if transparent: drawing.transparency
    else: Opaque

  result.dither = dither and drawing.dither
  result.setMaskBit = drawing.setMaskBit
  result.skipMaskedPixels = drawing.skipMaskedPixels

proc load(page: TexPage, loadMore: bool) =
  ## Load settings from a texpage.
  textures.base.x64 = page.baseX64
  textures.base.y256 = page.baseY256
  drawing.transparency = page.transparency
  # Reserved = 15-bit
  textures.colourDepth = page.colours.clampedConvert[:TextureColourDepth]
  textures.rawColourDepth = page.colours
  textures.enabled = not (textures.allowDisable and page.textureDisable)

  if loadMore:
    # These are only loaded by GP0(E1h),
    # not by textured polygon commands.
    drawing.dither = page.dither
    drawing.drawToDisplayArea = page.drawToDisplayArea
    textures.flip.x = page.flipX
    textures.flip.y = page.flipY

proc colourMode(palette: Point): TextureColourMode =
  ## Return a TextureColourMode for the given colour depth and palette.
  case textures.colourDepth
  of FourBit: TextureColourMode(depth: FourBit, palette: palette)
  of EightBit: TextureColourMode(depth: EightBit, palette: palette)
  of FifteenBit: TextureColourMode(depth: FifteenBit)

proc makeTexture[N: static int](coords: array[N, Point], palette: Point): Texture[N] =
  ## Create a texture using the current texture settings.
  Texture[N](
    page: (x: textures.base.x64 * 64, y: textures.base.y256 * 256),
    windowMask: textures.window.mask.unpack,
    windowOffset: textures.window.offset.unpack,
    coords: coords,
    colourMode: colourMode(palette))

# I/O interface

var
  resultQueue {.saved.} = initDeque[word]()

proc readyToSendVRAM: bool =
  resultQueue.len > 0

proc readyToReceiveDMA: bool =
  # TODO make this more accurate
  true

proc readyToReceiveCommand: bool =
  # TODO make this more accurate
  true

let
  command = BitSlice[int, word](pos: 24, width: 8)
  rest = BitSlice[int, word](pos: 0, width: 24)
  half1 = BitSlice[int, word](pos: 12, width: 12)
  half2 = BitSlice[int, word](pos: 0, width: 12)
  word1 = BitSlice[int, word](pos: 16, width: 16)
  word2 = BitSlice[int, word](pos: 0, width: 16)

# TODO: Measure on a real console how GPUSTAT changes throughout the frame

proc gpustat*: word =
  let bit13 =
    if screen.verticalInterlace: not screen.frameNumber.testBit(0)
    else: true
  let bit25 =
    case control.dmaDirection
    of DMADirection.Write: readyToReceiveDMA()
    of DMADirection.Read: readyToSendVRAM()
    else: false
  let bit31 =
    if screen.verticalInterlace: not screen.vblank and screen.frameNumber.testBit(0)
    else: not screen.vblank and currentScanline().testBit(0)
  result =
    word(textures.base.x64) or
    word(textures.base.y256) shl 4 or
    word(drawing.transparency) shl 5 or
    word(textures.rawColourDepth) shl 7 or
    word(drawing.dither) shl 9 or
    word(drawing.drawToDisplayArea) shl 10 or
    word(drawing.setMaskBit) shl 11 or
    word(drawing.skipMaskedPixels) shl 12 or
    word(bit13) shl 13 or
    # Don't bother with GPUSTAT 14 (reverseflag)
    word(not textures.enabled) shl 15 or
    word(screen.dotclockMultiplier == Dot7) shl 16 or
    (if screen.dotclockMultiplier == Dot7: 0u32 else: word(screen.dotclockMultiplier)) shl 17 or
    word(screen.verticalRes) shl 19 or
    word(region) shl 20 or
    word(screen.displayAreaDepth) shl 21 or
    word(screen.verticalInterlace) shl 22 or
    word(not screen.enabled) shl 23 or
    word(control.interruptRequested) shl 24 or
    word(bit25) shl 25 or
    word(readyToReceiveCommand()) shl 26 or
    word(readyToSendVRAM()) shl 27 or
    word(readyToReceiveDMA()) shl 28 or
    word(control.dmaDirection) shl 29 or
    word(bit31) shl 31
  trace fmt"GPUSTAT returned {result:08x}"

var processCommand {.saved.}: Consumer[word]
processCommand = consumer(word):
  let value = take
  let cmd = value[command]
  case cmd
  of 0x00: discard # NOP
  of 0x01: discard # Clear cache
  of 0x02:
    # Fill rectangle
    let colour = Colour(value[rest]).unpack
    var
      coord = take.ScreenCoord
      size = take.ScreenCoord
    var settings = rasteriserSettings(transparent = false,
                                      dither = false, crop = false, interlace = true)
    # Rounding from Nocash PSX
    let x = coord.x and 0x3f0
    let y = coord.y and 0x1ff
    let w = ((size.x and 0x3ff) + 0xf) and not 0xf
    let h = size.y and 0x1ff

    effect: settings.fill x, y, w, h, colour

  of 0x1f:
    # Interrupt requested
    effect:
      control.interruptRequested = true
      irqs.set(1, true)
  of 0x20..0x3f:
    # A polygon.
    # The bits of the command word have the following meaning:
    # * bit 0: if 1 then texture blending is disabled
    # * bit 1: enable transparency (using global transparency settings)
    # * bit 2: polygon is textured
    # * bit 3: if 0, draw a triangle; if 1, draw a quadrilateral
    # * bit 4: polygon is shaded
    #
    # The parameters come in the following order, where (0) is the command word:
    # (0) command+colour (1) vertex (2) texcoord+palette
    # (3) colour (4) vertex (5) texcoord+texpage
    # (6) colour (7) vertex (8) texcoord
    # (9) colour (10) vertex (11) texcoord
    # but only those parameters appropriate to the particular drawing
    # command are sent:
    # * (9)-(11) skipped for triangles
    # * Texcoords skipped for untextured polygons
    # * Colours (3),(6),(9) (but not 0) skipped for unshaded polygons

    let
      rawTextures = cmd.testBit 0
      transparent = cmd.testBit 1
      textured = cmd.testBit 2
      quad = cmd.testBit 3
      shaded = cmd.testBit 4
      sides = if quad: 4 else: 3

    var
      vertices: array[4, Point]
      colours: array[4, rasteriser.Colour]
      texcoords: array[4, Point]
      palette: Point

    colours[0] = Colour(value[rest]).unpack

    for i in 0..<sides:
      if i > 0:
        if shaded: colours[i] = Colour(take()).unpack
        else: colours[i] = colours[0] # monochrome
      vertices[i] = Vertex(take()).unpack + drawing.drawingAreaOffset.unpack
      if textured:
        let arg = take()
        texcoords[i] = TexCoord(arg[word2]).unpack
        case i
        of 0: palette = Palette(arg[word1]).unpack
        of 1: TexPage(arg[word1]).load(loadMore = false)
        else: discard

    effect:
      for i in 0..<(if quad: 2 else: 1):
        # Convert vertices [i,i+1,i+2] to a triangle
        let
          vs = [vertices[i], vertices[i+1], vertices[i+2]]
          cs = [colours[i], colours[i+1], colours[i+2]]
          texture = makeTexture([texcoords[i], texcoords[i+1], texcoords[i+2]], palette)

          shadingMode =
            if textured and textures.enabled:
              if rawTextures: Textures else: Both
            else: Colours

        var settings = rasteriserSettings(transparent = transparent,
                                          dither = shaded, crop = true, interlace = true)
        settings.draw Triangle(vertices: vs, shadingMode: shadingMode,
                                colours: cs, texture: texture)

  of 0x40..0x5f:
    # A straight line or polyline.
    # The bits of the command word have the following meaning:
    # * bit 1: enable transparency (using global transparency settings)
    # * bit 3: if 0, draw a line; if 1, draw a polyline
    # * bit 4: polygon is shaded
    #
    # The parameters come in the following order, where (0) is the command word:
    # (0) command+colour (1) vertex (2) optional colour (3) vertex etc.
    # Polylines end in a 0x5xxx5xxx (TODO: check)

    let
      transparent = cmd.testBit 1
      polyline = cmd.testBit 3
      shaded = cmd.testBit 4

    var
      vertices: seq[Point]
      colours: seq[rasteriser.Colour]

    template addColour(arg: word) =
      colours.add Colour(arg).unpack
    template addVertex(arg: word) =
      vertices.add(Vertex(arg).unpack + drawing.drawingAreaOffset.unpack)

    addColour value[rest].word
    addVertex take()

    if polyline:
      while true:
        let arg = take()
        if (arg and 0x50005000u32) == 0x50005000u32: break
        if shaded:
          addColour(arg)
          addVertex(take())
        else:
          colours.add colours[0]
          addVertex(arg)

    else:
      if shaded:
        addColour(take())
      else:
        colours.add colours[0]
      addVertex(take())

    effect:
      let settings = rasteriserSettings(transparent = transparent,
                                        dither = true, crop = true, interlace = true)

      for i in 0..vertices.len-2:
        # Draw a line from vertices[i] to vertices[i+1]
        let line = Line(start: (point: vertices[i], colour: colours[i]),
                        stop: (point: vertices[i+1], colour: colours[i+1]))
        settings.draw line

      # Fill in the endpoint of the last line, if it hasn't been drawn yet
      if vertices[^1] notin vertices[0..^2]:
        putPixel(vertices[^1].x, vertices[^1].y, colours[^1], settings)

  of 0x60..0x7f:
    # A rectangle.
    # The bits of the command word have the following meaning:
    # * bit 0: if 1 then texture blending is disabled
    # * bit 1: enable transparency (using global transparency settings)
    # * bit 2: polygon is textured
    # * bit 3-4: if 0, variable size; if 1, 1x1; if 2, 8x8; if 3, 16x16
    #
    # The parameters come in the following order, where (0) is the command word:
    # (0) command+colour (1) vertex (2) (optional) texcoord+palette
    # (3) (optional) width+height

    let
      rawTextures = cmd.testBit 0
      transparent = cmd.testBit 1
      textured = cmd.testBit 2

    var
      texcoord: Point
      palette: Point
      width, height: int

    let
      colour = Colour(value[rest]).unpack
      pos = Vertex(take()).unpack + drawing.drawingAreaOffset.unpack

    if textured:
      let arg = take()
      palette = Palette(arg[word1]).unpack
      texcoord = TexCoord(arg[word2]).unpack

    if cmd.testBit 4:
      if cmd.testBit 3:
        width = 16
        height = 16
      else:
        width = 8
        height = 8
    else:
      if cmd.testBit 3:
        width = 1
        height = 1
      else:
        let arg = take()
        width = ScreenCoord(arg).unpack.x
        height = ScreenCoord(arg).unpack.y

    let
      rect = (x1: pos.x, y1: pos.y, x2: pos.x + width, y2: pos.y + height)
      texture = makeTexture([texcoord], palette)

      shadingMode =
        if textured and textures.enabled:
          if rawTextures: Textures else: Both
        else: Colours

      settings = rasteriserSettings(transparent = transparent,
                                    dither = false, crop = true, interlace = true)
    effect:
      settings.draw Rectangle(rect: rect, shadingMode: shadingMode,
                              colour: colour, texture: texture,
                              flipX: textures.flip.x, flipY: textures.flip.y)

  of 0xe1:
    # Texpage
    effect:
      let page = value[rest].TexPage
      page.load(loadMore = true)

  of 0xe2:
    effect: textures.window = value[rest].TextureWindow

  of 0xe3:
    effect: drawing.drawingAreaTopLeft = value[rest].PackedScreenCoord

  of 0xe4:
    effect: drawing.drawingAreaBottomRight = value[rest].PackedScreenCoord

  of 0xe5:
    effect: drawing.drawingAreaOffset = value[rest].PackedSignedCoord

  of 0xe6:
    # Mask bit settings
    effect:
      drawing.setMaskBit = value.testBit 0
      drawing.skipMaskedPixels = value.testBit 1

  of 0x80..0x9f:
    # Copy VRAM to VRAM
    var
      src = take.ScreenCoord
      dest = take.ScreenCoord
      size = take.ScreenCoord
    var settings = rasteriserSettings(transparent = false,
                                      dither = false, crop = false, interlace = false)
    effect:
      # This detail comes from Nocash PSX
      for j in 0..<(if size.y == 0: vramHeight else: size.y):
        for i in 0..<(if size.x == 0: vramWidth else: size.x):
          let x1 = (src.x + i) mod vramWidth
          let y1 = (src.y + j) mod vramHeight
          let x2 = (dest.x + i) mod vramWidth
          let y2 = (dest.y + j) mod vramHeight
          putPixel(x2, y2, getPixel(x1, y1), settings)

  of 0xa0..0xbf:
    # Copy rectangle to VRAM
    var
      coord = take.ScreenCoord
      size = take.ScreenCoord
    var settings = rasteriserSettings(transparent = false,
                                      dither = false, crop = false, interlace = false)

    var
      arg: word = 0
      even = true
    # This detail comes from Nocash PSX
    for j in 0..<(if size.y == 0: vramHeight else: size.y):
      for i in 0..<(if size.x == 0: vramWidth else: size.x):
        let x = (coord.x + i) mod vramWidth
        let y = (coord.y + j) mod vramHeight
        if even:
          arg = take()
          effect: putHalfword(x, y, arg[word2], settings)
          even = false
        else:
          effect: putHalfword(x, y, arg[word1], settings)
          even = true
  of 0xc0..0xdf:
    # Copy rectangle to CPU
    let
      coord = take.Vertex
      size = take.Vertex

    var
      val: word = 0
      even = true
    for j in 0..<(if size.y == 0: vramHeight else: size.y):
      for i in 0..<(if size.x == 0: vramWidth else: size.x):
        let x = (coord.x + i) mod vramWidth
        let y = (coord.y + j) mod vramHeight
        if even:
          val = getHalfword(x, y).word
          even = false
        else:
          val = val or (getHalfword(x, y).word shl 16)
          effect: resultQueue.addLast val
          even = true
    # Handle copy of an odd number of words
    if not even:
      effect: resultQueue.addLast val
  else:
    effect:
      warn fmt"Unrecognised GP0 command {value[command]:02x}"
    while true: discard take()

proc gp0*(value: word) =
  trace fmt"GP0 {value:08x}"
  processCommand.give(value)

proc gp1*(value: word) =
  debug fmt"GP1 {value:08x}"
  case value[command] and 0x3f
  of 0x00:
    # Reset
    processCommand.reset
    let cmds = [0x01_000000u32, 0x02_000000u32, 0x03_000001u32, 0x04_000000u32,
                0x05_000000u32, 0x06_c00_200u32, 0x07_100_010u32, 0x08000001u32]
    for cmd in cmds:
      gp1 cmd

    for cmd in 0xe1..0x6:
      gp0 (word(cmd) shl 24)
  of 0x01:
    # Supposed to reset command buffer
    discard
  of 0x02:
    # Acknowledge interrupt
    control.interruptRequested = false
    irqs.set(1, false)
  of 0x03:
    # Display enable
    screen.enabled = not (value.testBit 0)
  of 0x04:
    # DMA direction
    control.dmaDirection = DMADirection(value and 3)
  of 0x05:
    # Start of display area
    screen.displayAreaStart = value[rest].PackedScreenCoord
  of 0x06:
    # Horizontal display range
    screen.horizontalRange = (start: value[half2], stop: value[half1])
  of 0x07:
    # Vertical display range
    screen.verticalRange = (start: value.PackedCoord.x, stop: value.PackedCoord.y)
  of 0x08:
    # Display mode
    let oldSettings = screenSettings()
    screen.dotclockMultiplier =
      if value.testBit 6: Dot7 else: DotclockMultiplier(value and 3)
    screen.verticalRes = VerticalRes(value.testBit 2)
    region = Region(value.testBit 3)
    screen.displayAreaDepth = ColourDepth(value.testBit 4)
    screen.verticalInterlace = value.testBit 5
    let newSettings = screenSettings()
    if oldSettings != newSettings: info newSettings
  of 0x09:
    # Texture disable
    textures.allowDisable = value.testBit 0
  of 0x10..0x1f:
    # Get GPU info
    case value and 0xf
    of 2: control.result = word(textures.window)
    of 3: control.result = word(drawing.drawingAreaTopLeft)
    of 4: control.result = word(drawing.drawingAreaBottomRight)
    of 5: control.result = word(drawing.drawingAreaOffset)
    of 7: control.result = 2 # GPU Type
    of 8: control.result = 0
    else:
      warn fmt"Unrecognised GPU info query {value and 0xf:02x}"
    debug fmt"GPU info returned {control.result:08x}"
  else:
    warn fmt"Unrecognised GP1 command {value:08x}"

proc gpuread*: word =
  if resultQueue.len == 0:
    result = control.result
    debug fmt"GPUREAD returns {result:08x} (result)"
  else:
    result = resultQueue.popFirst()
    trace fmt"GPUREAD returns {result:08x} (VRAM)"

proc gpuWriteDMA*(value: word) =
  case control.dmaDirection
  of DMADirection.Write: gp0(value)
  else: warn fmt"GPU DMA write of {value:08x} while direction is {control.dmaDirection}"

proc gpuReadDMA*: word =
  case control.dmaDirection
  of DMADirection.Read: result = gpuread()
  else: warn "GPU DMA read while direction is {control.dmaDirection}"

