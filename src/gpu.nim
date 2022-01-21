## The GPU.

import basics, utils, irq, eventqueue, rasteriser
import std/[bitops, strformat, deques, options]

var logger = newLogger("GPU")

type
  HorizontalRes {.pure.} = enum
    Res256 = 0,
    Res320 = 1,
    Res512 = 2,
    Res640 = 3,
    Res368 = 4
  VerticalRes {.pure.} = enum
    Res240 = 0,
    Res480 = 1
  ColourDepth {.pure.} = enum
    Depth15 = 0,
    Depth24 = 1
  DMADirection {.pure.} = enum
    Off = 0,
    Unknown = 1,
    Write = 2,
    Read = 3
  Vertex = distinct word
  Colour = distinct word
  TexPage = distinct uint16
  TextureWindow = distinct word
  PackedMiniCoord = distinct word
  PackedCoord = distinct word
  PackedSignedCoord = distinct word
  Palette = distinct uint16
  TexCoord = distinct uint16

Vertex.bitfield x, int, 0, 11, signed=true
Vertex.bitfield y, int, 16, 11, signed=true

func unpack(v: Vertex): rasteriser.Coord =
  (x: v.x, y: v.y)

Colour.bitfield red, uint8, 0, 8
Colour.bitfield green, uint8, 8, 8
Colour.bitfield blue, uint8, 16, 8

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

func base(page: TexPage): rasteriser.Coord =
  (x: page.baseX64 * 64, y: page.baseY256 * 256)

TextureWindow.bitfield mask, PackedMiniCoord, 0, 10
TextureWindow.bitfield offset, PackedMiniCoord, 10, 10

PackedMiniCoord.bitfield x8, int, 0, 5
PackedMiniCoord.bitfield y8, int, 5, 5

func unpack(c: PackedMiniCoord): rasteriser.Coord =
  (x: c.x8 * 8, y: c.y8 * 8)

PackedCoord.bitfield x, int, 0, 10
PackedCoord.bitfield y, int, 10, 10

func unpack(c: PackedCoord): rasteriser.Coord =
  (x: c.x, y: c.y)

PackedSignedCoord.bitfield x, int, 0, 11, signed=true
PackedSignedCoord.bitfield y, int, 11, 11, signed=true

func unpack(c: PackedSignedCoord): rasteriser.Coord =
  (x: c.x, y: c.y)

TexCoord.bitfield x, int, 0, 8
TexCoord.bitfield y, int, 8, 8

func unpack(c: TexCoord): rasteriser.Coord =
  (x: c.x, y: c.y)

Palette.bitfield x16, int, 0, 6
Palette.bitfield y, int, 6, 9

func unpack(p: Palette): rasteriser.Coord =
  (x: p.x16 * 16, y: p.y)

func colourMode(page: TexPage, palette: Coord): TextureColourMode =
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
    drawingAreaTopLeft: PackedCoord  # GP0(E3h)
    drawingAreaBottomRight: PackedCoord # GP0(E4h)
    drawingAreaOffset: PackedSignedCoord # GP0(E5h)

  # Screen settings
  ScreenSettings = object
    displayAreaStart: PackedCoord    # GP1(05h)
    displayAreaDepth: ColourDepth    # GPUSTAT 21
    horizontalRes: HorizontalRes   # GPUSTAT 16-18
    verticalRes: VerticalRes       # GPUSTAT 19
    verticalInterlace: bool        # GPUSTAT 22
    enabled: bool                  # GPUSTAT 23
    oddLine: bool                  # GPUSTAT 31
    # In GPU clocks, not in pixels!
    horizontalRange: tuple[start: int, stop: int]
    # Scanline numbers relative to VSYNC
    verticalRange: tuple[start: int, stop: int]

  ControlSettings = object
    interruptRequested: bool       # GPUSTAT 24
    dmaDirection: DMADirection     # GPUSTAT 29-30
    result: word                   # GPUREAD

var
  textures = TextureSettings(enabled: true)
  drawing = DrawingSettings()
  screen = ScreenSettings()
  control = ControlSettings()

proc displayArea: Rect =
  result.x1 = screen.displayAreaStart.x
  result.x2 = screen.displayAreaStart.y
  let
    ## TODO: calculate width/height using horizontalRange/verticalRange.
    ## horizontalRes and verticalRes supposedly encode dotclock speed
    width =
      case screen.horizontalRes
      of Res256: 256
      of Res320: 320
      of Res512: 512
      of Res640: 640
      of Res368: 368
    height =
      case screen.verticalRes
      of Res240: 240
      of Res480: 480
  result.x2 = result.x1 + width
  result.y2 = result.y1 + height

proc rasteriserSettings(transparent: bool): rasteriser.Settings =
  result.drawingArea =
    (x1: drawing.drawingAreaTopLeft.x,
     y1: drawing.drawingAreaTopLeft.y,
     x2: drawing.drawingAreaBottomRight.x+1,
     y2: drawing.drawingAreaBottomRight.y+1)

  if drawing.drawToDisplayArea:
    result.displayArea = displayArea()
  else:
    result.displayArea = (x1: -1, y1: -1, x2: -1, y2: -1)

  result.transparency =
    if transparent: drawing.transparency
    else: Opaque

  result.dither = drawing.dither
  result.setMaskBit = drawing.setMaskBit
  result.skipMaskedPixels = drawing.skipMaskedPixels

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
  byte1 = BitSlice[int, word](pos: 16, width: 8)
  byte2 = BitSlice[int, word](pos: 8, width: 8)
  byte3 = BitSlice[int, word](pos: 0, width: 8)
  half1 = BitSlice[int, word](pos: 12, width: 12)
  half2 = BitSlice[int, word](pos: 0, width: 12)
  word1 = BitSlice[int, word](pos: 16, width: 16)
  word2 = BitSlice[int, word](pos: 0, width: 16)

proc gpustat*: word =
  let bit25 =
    case control.dmaDirection
    of DMADirection.Write: readyToReceiveDMA()
    of DMADirection.Read: readyToSendVRAM()
    else: false
  word(textures.base.x64) or
  word(textures.base.y256) shl 4 or
  word(drawing.transparency) shl 5 or
  word(textures.colourDepth) shl 7 or
  word(drawing.dither) shl 9 or
  word(drawing.drawToDisplayArea) shl 10 or
  word(drawing.setMaskBit) shl 11 or
  word(drawing.skipMaskedPixels) shl 12 or
  word(not screen.verticalInterlace) shl 13 or # TODO: is this right?
  # Don't bother with GPUSTAT 14 (reverseflag)
  word(not textures.enabled) shl 15 or
  word(screen.horizontalRes == Res368) shl 16 or
  (if screen.horizontalRes == Res368: 0u32 else: word(screen.horizontalRes)) shl 17 or
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
  word(screen.oddLine) shl 31
  # logger.debug fmt"GPUSTAT returned {result:08x}"

let processCommand = consumer(word):
  while true:
    let value = take
    let cmd = value[command]
    case cmd
    of 0x00: discard # NOP
    of 0x01: discard # Clear cache
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
        vertices: array[4, Coord]
        colours: array[4, rasteriser.Colour]
        texcoords: array[4, Coord]
        texpage: TexPage
        palette: Coord

      colours[0] = Colour(value[rest]).unpack

      for i in 0..<sides:
        if i > 0:
          if shaded: colours[i] = Colour(take()).unpack
          else: colours[i] = colours[0] # monochrome
        vertices[i] = Vertex(take()).unpack
        vertices[i].x += drawing.drawingAreaTopLeft.x + drawing.drawingAreaOffset.x
        vertices[i].y += drawing.drawingAreaTopLeft.y + drawing.drawingAreaOffset.y
        if textured:
          let arg = take()
          texcoords[i] = TexCoord(arg[word2]).unpack
          case i
          of 0: palette = Palette(arg[word1]).unpack
          of 1: texpage = TexPage(arg[word1])
          else: discard

      for i in 0..<(if quad: 2 else: 1):
        # Convert vertices [i,i+1,i+2] to a triangle
        let vs = [vertices[i], vertices[i+1], vertices[i+2]]
        let cs =
          if textured and rawTextures: none(array[3, rasteriser.Colour])
          else: some [colours[i], colours[i+1], colours[i+2]]
        let texture =
          if textured:
            some Texture[3](
              page: texpage.base,
              windowMask: textures.window.mask.unpack,
              windowOffset: textures.window.offset.unpack,
              coords: [texcoords[i], texcoords[i+1], texcoords[i+2]],
              colourMode: texpage.colourMode(palette))
          else:
            none(Texture[3])
        let settings = rasteriserSettings(transparent)
        settings.draw Triangle(vertices: vs, colours: cs, texture: texture)

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
      drawing.drawingAreaTopLeft = value[rest].PackedCoord

    of 0xe4:
      drawing.drawingAreaBottomRight = value[rest].PackedCoord

    of 0xe5:
      drawing.drawingAreaOffset = value[rest].PackedSignedCoord

    of 0xe6:
      # Mask bit settings
      drawing.setMaskBit = value.testBit 0
      drawing.skipMaskedPixels = value.testBit 1

    of 0xa0:
      # Copy rectangle to VRAM
      let
        coord = take.Vertex
        size = take.Vertex
      var settings = rasteriserSettings(false)
      settings.drawingArea = (x1: 0, y1: 0, x2: 1024, y2: 512)
      settings.displayArea = (x1: -1, y1: -1, x2: -1, y2: -1)

      var
        arg: word = 0
        even = true
      for j in coord.y..<coord.y+size.y:
        for i in coord.x..<coord.x+size.x:
          if even:
            arg = take()
            putPixel(i, j, arg[word2].Pixel, settings)
            even = false
          else:
            putPixel(i, j, arg[word1].Pixel, settings)
            even = true
    of 0xc0:
      # Copy rectangle to CPU
      let
        coord = take.Vertex
        size = take.Vertex

      var
        val: word = 0
        even = true
      for j in coord.y..<coord.y+size.y:
        for i in coord.x..<coord.x+size.x:
          if even:
            val = getPixel(i, j).word
            even = false
          else:
            val = val or (getPixel(i, j).word shl 16)
            resultQueue.addLast val
            even = true
    else:
      logger.warn fmt"Unrecognised GP0 command {value[command]:02x}"
      while true: yield

var iter = processCommand.start
proc gp0*(value: word) =
  logger.debug fmt"GP0 {value:08x}"
  iter.give(value)

proc gp1*(value: word) =
  logger.debug fmt"GP1 {value:08x}"
  case value[command] and 0x3f
  of 0x00:
    # Reset
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
    screen.displayAreaStart.x = value[half1]
    screen.displayAreaStart.y = value[half2]
  of 0x06:
    # Horizontal display range
    screen.horizontalRange = (start: value[half1], stop: value[half2])
  of 0x07:
    # Vertical display range
    screen.verticalRange = (start: value[half1], stop: value[half2])
  of 0x08:
    # Display mode
    screen.horizontalRes =
      if value.testBit 6: Res368 else: HorizontalRes(value and 3)
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
    else:
      logger.warn fmt"Unrecognised GPU info query {value[command]:02x}"
    logger.debug fmt"GPU info returned {control.result:08x}"
  else:
    logger.warn fmt"Unrecognised GP1 command {value:08x}"

proc gpuread*: word =
  if resultQueue.len == 0:
    result = control.result
    logger.debug fmt"GPUREAD returns {result:08x} (result)"
  else:
    result = resultQueue.popFirst()
    logger.debug fmt"GPUREAD returns {result:08x} (VRAM)"

proc gpuWriteDMA*(value: word) =
  case control.dmaDirection
  of DMADirection.Write: gp0(value)
  else: logger.warn fmt"GPU DMA write of {value:08x} while direction is {control.dmaDirection}"

proc gpuReadDMA*: word =
  case control.dmaDirection
  of DMADirection.Read: result = gpuread()
  else: logger.warn "GPU DMA read while direction is {control.dmaDirection}"

# VBLANK IRQ
proc signalVBlank =
  events.after(clockRate div refreshRate[region]) do ():
    logger.debug "VBLANK"
    screen.oddLine = not screen.oddLine
    irqs.signal 0
    signalVBlank()

signalVBlank()
