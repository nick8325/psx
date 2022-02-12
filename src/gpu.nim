## The GPU.

import basics, utils, irq, eventqueue, rasteriser
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

func colourMode(page: TexPage, palette: Point): TextureColourMode =
  # depth=3 apparently is treated as 15-bit
  case page.colours.clampedConvert[:TextureColourDepth]
  of FourBit: TextureColourMode(depth: FourBit, palette: palette)
  of EightBit: TextureColourMode(depth: EightBit, palette: palette)
  of FifteenBit: TextureColourMode(depth: FifteenBit)

type
  # Texture settings
  TextureSettings = object
    # x is multiplied by 64, y by 256
    base: tuple[x64: int, y256: int] # GPUSTAT 0-4
    # In 8-pixel steps
    window: TextureWindow            # GP0(E2h)
    colourDepth: TextureColourDepth  # GPUSTAT 7-8
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
    frameNumber: int               # Number of frames drawn since boot
    # (in interlaced mode, both even and odd frames count as 1)
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
  textures = TextureSettings(enabled: true)
  drawing = DrawingSettings()
  screen = ScreenSettings(vblank: true)
  control = ControlSettings()

# Screen settings, including dot-clocks and other such analogue stuff

const
  scanlinesPerFrame: array[Region, int64] =
    [NTSC: 263i64, PAL: 314]
  gpuClocksPerScanline: array[Region, int64] =
    [NTSC: 3413i64, PAL: 3406]
  gpuClocksPerDotClock: array[DotclockMultiplier, int64] =
    [Dot10: 10i64, Dot8: 8, Dot5: 5, Dot4: 4, Dot7: 7]

var
  lastVBlankStart: int64 = 0

proc clocksPerPixel*: int64 {.inline.} =
  ## Clock cycles per pixel drawn.

  gpuClock * gpuClocksPerDotClock[screen.dotclockMultiplier]
  # TODO: support 480p mode?
  # (PSX hardware supports it if connected to VGA monitor)

proc clocksPerScanline*: int64 {.inline.} =
  ## Clock cycles per scanline.

  gpuClock * gpuClocksPerScanline[region]

proc clocksPerFrame*: int64 {.inline.} =
  ## Clock cycles per screen refresh.

  clocksPerScanline() * scanlinesPerFrame[region]

proc maxScreenWidth: int64 =
  ## Maximum permissible screen width.

  gpuClocksPerScanline[region] div gpuClocksPerDotClock[screen.dotClockMultiplier]

proc screenWidth*: int =
  ## Width of screen in pixels.

  # Info comes from Nocash PSX
  let clocks = screen.horizontalRange.stop - screen.horizontalRange.start
  result = clocks div gpuClocksPerDotClock[screen.dotClockMultiplier].int
  result = (result + 2) and not 3
  result = clamp(result, 0, maxScreenWidth().int)

proc visibleScanlines*: int =
  ## Number of scanlines drawn per frame.
  ## In interlaced mode, this is half the screen height.

  clamp(screen.verticalRange.stop - screen.verticalRange.start + 1,
        0, scanlinesPerFrame[region].int)

proc screenHeight*: int =
  ## Height of screen in pixels.

  visibleScanlines() * (if screen.verticalInterlace: 2 else: 1)

# TODO: measure HBLANK/VBLANK timings on a real console

proc hblankClocks*: int64 {.inline.} =
  ## Clock cycles taken by one hblank.

  clocksPerPixel() * (maxScreenWidth() - screenWidth())

proc vblankClocks*: int64 {.inline.} =
  ## Clock cycles taken by one vblank.

  clocksPerScanline() * (scanlinesPerFrame[region] - visibleScanlines())

proc lastVBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles since the last VBLANK started.

  result = events.now() - lastVBlankStart
  assert result >= 0 and result < clocksPerFrame()

proc nextVBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles until the next VBLANK starts.

  result = clocksPerFrame() - lastVBlankDelta()
  assert result > 0 and result <= clocksPerFrame()

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
  result = lastVBlankDelta() mod clocksPerScanline()
  assert result >= 0 and result < clocksPerScanline()

proc nextHBlankDelta*(): int64 {.inline.} =
  ## Number of clock cycles until the next HBLANK starts.

  result = clocksPerScanline() - lastHBlankDelta()
  assert result > 0 and result <= clocksPerScanline()

proc inHBlank*(): bool {.inline.} =
  ## Are we currently in the HBLANK interval?

  lastHBlankDelta() < hblankClocks()

proc remainingHBlankClocks*(): int64 {.inline.} =
  ## How many clock cycles remain of the current HBLANK interval?
  ## Returns 0 if not in HBLANK.

  return max(hblankClocks() - lastHBlankDelta(), 0)

proc currentScanline*(): int64 {.inline.} =
  ## The current scanline number being drawn.

  result = lastVBlankDelta() div clocksPerScanline()
  assert result >= 0 and result < scanlinesPerFrame[region]

proc screenSettings*(): string =
  ## Dump the screen settings.

  result = fmt"{screenWidth()}x{screenHeight()}"
  if screen.verticalInterlace: result &= " interlaced"
  result &= fmt", hblank time {hblankClocks()}"
  result &= fmt", vblank time {vblankClocks()}"
  result &= fmt", current scanline {currentScanline()}"

proc onHBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextHBlankDelta(), name, p)

proc afterHBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextHBlankDelta() + hblankClocks(), name, p)

proc onVBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextVBlankDelta(), name, p)

proc afterVBlank*(name: string, p: proc()) =
  events.every(proc(): int64 = nextVBlankDelta() + vblankClocks(), name, p)

# VBLANK IRQ
onVBlank("gpu vblank") do ():
  debug "VBLANK"
  lastVBlankStart = events.now()
  screen.frameNumber += 1
  screen.vblank = true
  irqs.signal 0

  # PSX supports 480p if hooked up to a VGA monitor, but we don't
  if screen.verticalRes == ResDouble and not screen.verticalInterlace:
    warn "480p mode not supported"

afterVBlank("gpu end vblank") do ():
  debug "END VBLANK"
  screen.vblank = false

proc displayArea: Rect =
  result.x1 = screen.displayAreaStart.x
  result.x2 = screen.displayAreaStart.y
  result.x2 = result.x1 + screenWidth()
  result.y2 = result.y1 + screenHeight()

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

# I/O interface

var
  resultQueue = initDeque[word]()

proc readyToSendVRAM: bool =
  resultQueue.len > 0

proc readyToReceiveDMA: bool =
  # TODO is this right?
  not readyToSendVRAM()

proc readyToReceiveCommand: bool =
  readyToReceiveDMA()

let
  command = BitSlice[int, word](pos: 24, width: 8)
  rest = BitSlice[int, word](pos: 0, width: 24)
  half1 = BitSlice[int, word](pos: 12, width: 12)
  half2 = BitSlice[int, word](pos: 0, width: 12)
  word1 = BitSlice[int, word](pos: 16, width: 16)
  word2 = BitSlice[int, word](pos: 0, width: 16)

# TODO: Measure on a real console how GPUSTAT changes throughout the frame

proc gpustat*: word =
  let bit25 =
    case control.dmaDirection
    of DMADirection.Write: readyToReceiveDMA()
    of DMADirection.Read: readyToSendVRAM()
    else: false
  let bit31 =
    if screen.verticalInterlace: currentScanline().testBit(0)
    else: not screen.vblank and screen.frameNumber.testBit(0)
  result =
    word(textures.base.x64) or
    word(textures.base.y256) shl 4 or
    word(drawing.transparency) shl 5 or
    word(textures.colourDepth) shl 7 or
    word(drawing.dither) shl 9 or
    word(drawing.drawToDisplayArea) shl 10 or
    word(drawing.setMaskBit) shl 11 or
    word(drawing.skipMaskedPixels) shl 12 or
    word(not screen.verticalInterlace or not screen.frameNumber.testBit(0)) shl 13 or
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

let processCommand = consumer(word):
  while true:
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

      settings.fill x, y, w, h, colour

    of 0x1f:
      # Interrupt requested
      control.interruptRequested = true
      irqs.signal 1
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
        texpage: TexPage
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
          of 1: texpage = TexPage(arg[word1])
          else: discard

      for i in 0..<(if quad: 2 else: 1):
        # Convert vertices [i,i+1,i+2] to a triangle
        let
          vs = [vertices[i], vertices[i+1], vertices[i+2]]
          cs = [colours[i], colours[i+1], colours[i+2]]
          texture = Texture[3](
            page: texpage.base,
            windowMask: textures.window.mask.unpack,
            windowOffset: textures.window.offset.unpack,
            coords: [texcoords[i], texcoords[i+1], texcoords[i+2]],
            colourMode: texpage.colourMode(palette))

          shadingMode =
            if textured and textures.enabled:
              if rawTextures: Textures else: Both
            else: Colours

        let settings = rasteriserSettings(transparent = transparent,
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

      let settings = rasteriserSettings(transparent = transparent,
                                        dither = true, crop = true, interlace = true)

      for i in 0..vertices.len-2:
        # Draw a line from vertices[i] to vertices[i+1]
        let line = Line(start: (point: vertices[i], colour: colours[i]),
                        stop: (point: vertices[i+1], colour: colours[i+1]))
        settings.draw line

    of 0xe1:
      # Texpage
      let page = value[rest].TexPage
      textures.base.x64 = page.baseX64
      textures.base.y256 = page.baseY256
      drawing.transparency = page.transparency
      # Reserved = 15-bit
      textures.colourDepth = page.colours.clampedConvert[:TextureColourDepth]
      drawing.dither = page.dither
      drawing.drawToDisplayArea = page.drawToDisplayArea
      textures.enabled = not page.textureDisable
      textures.flip.x = page.flipX
      textures.flip.y = page.flipY

    of 0xe2:
      textures.window = value[rest].TextureWindow

    of 0xe3:
      drawing.drawingAreaTopLeft = value[rest].PackedScreenCoord

    of 0xe4:
      drawing.drawingAreaBottomRight = value[rest].PackedScreenCoord

    of 0xe5:
      drawing.drawingAreaOffset = value[rest].PackedSignedCoord

    of 0xe6:
      # Mask bit settings
      drawing.setMaskBit = value.testBit 0
      drawing.skipMaskedPixels = value.testBit 1

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
      for j in coord.y..<coord.y+(if size.y == 0: vramHeight else: size.y):
        for i in coord.x..<coord.x+(if size.x == 0: vramWidth else: size.x):
          if even:
            arg = take()
            putHalfword(i, j, arg[word2], settings)
            even = false
          else:
            putHalfword(i, j, arg[word1], settings)
            even = true
    of 0xc0..0xdf:
      # Copy rectangle to CPU
      let
        coord = take.Vertex
        size = take.Vertex

      var
        val: word = 0
        even = true
      for j in coord.y..<coord.y+(if size.y == 0: vramHeight else: size.y):
        for i in coord.x..<coord.x+(if size.x == 0: vramWidth else: size.x):
          if even:
            val = getHalfword(i, j).word
            even = false
          else:
            val = val or (getHalfword(i, j).word shl 16)
            resultQueue.addLast val
            even = true
    else:
      warn fmt"Unrecognised GP0 command {value[command]:02x}"
      while true: yield

var iter = processCommand.start
proc gp0*(value: word) =
  trace fmt"GP0 {value:08x}"
  iter.give(value)

proc gp1*(value: word) =
  debug fmt"GP1 {value:08x}"
  case value[command] and 0x3f
  of 0x00:
    # Reset
    iter = processCommand.start
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
    screen.dotclockMultiplier =
      if value.testBit 6: Dot7 else: DotclockMultiplier(value and 3)
    screen.verticalRes = VerticalRes(value.testBit 2)
    region = Region(value.testBit 3)
    screen.displayAreaDepth = ColourDepth(value.testBit 4)
    screen.verticalInterlace = value.testBit 5
  of 0x09:
    # Texture disable
    textures.allowDisable = value.testBit 0
  of 0x10..0x1f:
    # Get GPU info
    case value and 0x10
    of 2: control.result = word(textures.window)
    of 3: control.result = word(drawing.drawingAreaTopLeft)
    of 4: control.result = word(drawing.drawingAreaBottomRight)
    of 5: control.result = word(drawing.drawingAreaOffset)
    of 7: control.result = 2 # GPU Type
    of 8: control.result = 0
    else:
      warn fmt"Unrecognised GPU info query {value[command]:02x}"
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

