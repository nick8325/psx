## The GPU.

import basics, utils, irq, eventqueue
import std/[bitops, strformat, logging, deques, options]

var logger = newLogger("GPU", lvlDebug)

type
  TransparencyMode {.pure.} = enum
    Mean = 0,
    Add = 1,
    Subtract = 2,
    AddQuarter = 3
  TextureColourMode {.pure.} = enum
    FourBit = 0,
    EightBit = 1,
    FifteenBit = 2
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

Vertex.bitfield x, int, 0, 10, signed=true
Vertex.bitfield y, int, 26, 10, signed=true

Colour.bitfield red, int, 0, 8
Colour.bitfield green, int, 8, 8
Colour.bitfield blue, int, 16, 8

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

TextureWindow.bitfield mask, PackedMiniCoord, 0, 10
TextureWindow.bitfield offset, PackedMiniCoord, 10, 10

PackedMiniCoord.bitfield x8, int, 0, 5
PackedMiniCoord.bitfield y8, int, 5, 5

PackedCoord.bitfield x, int, 0, 10
PackedCoord.bitfield y, int, 10, 10

PackedSignedCoord.bitfield x, int, 0, 11, signed=true
PackedSignedCoord.bitfield y, int, 11, 11, signed=true

type
  # Texture settings
  TextureSettings = object
    # x is multiplied by 64, y by 256
    base: tuple[x: int, y: int]    # GPUSTAT 0-4
    # In 8-pixel steps
    window: TextureWindow          # GP0(E2h)
    colourMode: TextureColourMode  # GPUSTAT 7-8
    enabled: bool                  # GPUSTAT 15
    allowDisable: bool             # GP1(09h)
    flip: tuple[x: bool, y: bool]  # GP0(E1h).12-13

  # Drawing (to framebuffer) settings
  DrawingSettings = object
    transparency: TransparencyMode # GPUSTAT 5-6
    setMaskBit: bool               # GPUSTAT 11
    skipMaskedPixels: bool         # GPUSTAT 12
    drawToDisplayArea: bool        # GPUSTAT 10
    displayAreaDepth: ColourDepth  # GPUSTAT 21
    dither: bool                   # GPUSTAT 9
    displayAreaStart: PackedCoord  # GP1(05h)
    drawingAreaTopLeft: PackedCoord # GP0(E3h)
    drawingAreaBottomRight: PackedCoord # GP0(E4h)
    drawingAreaOffset: PackedSignedCoord # GP0(E5h)

  # Screen settings
  ScreenSettings = object
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

type
  TriangleTexture = object
    palette: tuple[x: int, y: int]
    texpage: tuple[x: int, y: int]
    coordinate: tuple[x: int, y: int]
  Triangle = object
    transparency: Option[TransparencyMode]
    vertices: array[3, tuple[x: int, y: int, colour: Colour]]
    textures: Option[TriangleTexture]

proc drawTriangle(x: Triangle) =
  echo "draw {x.repr}"

var
  commandQueue = initDeque[word]()
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

var cachedGPUStat: word

proc updateGPUStat* =
  let bit25 =
    case control.dmaDirection
    of DMADirection.Write: readyToReceiveDMA()
    of DMADirection.Read: readyToSendVRAM()
    else: false
  cachedGPUStat =
    word(textures.base.x) or
    word(textures.base.y) shl 4 or
    word(drawing.transparency) shl 5 or
    word(textures.colourMode) shl 7 or
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
    word(drawing.displayAreaDepth) shl 21 or
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

updateGPUStat()

proc processCommand =
  template args(n: int): seq[word] =
    block:
      # Remove the command and arguments, but only if all arguments are available.
      if commandQueue.len < n+1: # include command itself
        return
      commandQueue.popFirst
      var res: seq[word] = @[]
      for i in 1..n: res.add commandQueue.popFirst
      res

  if commandQueue.len > 0:
    let value = commandQueue[0]
    case value[command]
    of 0x00: discard args 0 # NOP
    of 0x1f:
      # Interrupt requested
      discard args 0
      control.interruptRequested = true
      irqs.signal 1
    of 0x20, 0x22:
      # Monochrome triangle
      let
        args = args 3
        colour = Colour(value[rest])
        v1 = Vertex(args[0])
        v2 = Vertex(args[1])
        v3 = Vertex(args[2])
        transparency =
          if value[command] == 0x20: none(TransparencyMode)
          else: some(drawing.transparency)
      drawTriangle Triangle(transparency: transparency,
                            vertices: [(x: v1.x, y: v1.y, colour: colour),
                                       (x: v2.x, y: v2.y, colour: colour),
                                       (x: v3.x, y: v3.y, colour: colour)])
    else:
      logger.warn fmt"Unrecognised GP0 command {value[command]:02x}"

proc gp0*(value: word) =
  logger.debug fmt"GP0 {value:08x}"
  commandQueue.addLast(value)
  processCommand()
  updateGPUStat()

proc gp1*(value: word) =
  logger.debug fmt"GP1 {value:08x}"
  case value[command] and 0x3f
  of 0x00:
    # Reset
    let cmds= [0x01_000000u32, 0x02_000000u32, 0x03_000001u32, 0x04_000000u32,
               0x05_000000u32, 0x06_c00_200u32, 0x07_100_010u32, 0x08000001u32]
    for cmd in cmds:
      gp1 cmd

    for cmd in 0xe1..0x6:
      gp0 (word(cmd) shl 24)
  of 0x01:
    # Reset command buffer
    commandQueue.clear
    if resultQueue.len > 0:
      logger.warn "Result queue non-empty on GP1(1)"
    resultQueue.clear
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
    drawing.displayAreaStart.x = value[half1]
    drawing.displayAreaStart.y = value[half2]
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
    drawing.displayAreaDepth = ColourDepth(value.testBit 4)
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
  updateGPUStat()

proc gpuread*: word =
  if resultQueue.len == 0:
    result = control.result
    logger.debug fmt"GPUREAD returns {result:08x} (result)"
  else:
    result = resultQueue.popFirst()
    logger.debug fmt"GPUREAD returns {result:08x} (VRAM)"
  updateGPUStat()

proc gpustat*: word =
  cachedGPUStat

proc gpuWriteDMA*(value: word) =
  case control.dmaDirection
  of DMADirection.Write: gp0(value)
  else: logger.warn fmt"GPU DMA write of {value:08x} while direction is {control.dmaDirection}"
  updateGPUStat()

proc gpuReadDMA*: word =
  case control.dmaDirection
  of DMADirection.Read: result = gpuread()
  else: logger.warn "GPU DMA read while direction is {control.dmaDirection}"
  updateGPUStat()

# VBLANK IRQ
proc signalVBlank =
  events.after(clockRate div refreshRate[region]) do ():
    logger.debug "VBLANK"
    screen.oddLine = not screen.oddLine
    updateGPUStat()
    irqs.signal 0
    signalVBlank()

signalVBlank()
