## The GPU.

import machine, utils, irq
import std/[bitops, strformat, logging, deques]

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

type
  # Texture settings
  TextureSettings = object
    # x is multiplied by 64, y by 256
    base: tuple[x: int, y: int]    # GPUSTAT 0-4
    # In 8-pixel steps
    mask: tuple[x: int, y: int]    # GP0(E2h).0-9
    offset: tuple[x: int, y: int]  # GP0(E2h).10-19
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
    displayAreaStart: tuple[x: int, y: int] # GP1(05h)
    drawingAreaTopLeft: tuple[x: int, y: int] # GP0(E3h)
    drawingAreaBottomRight: tuple[x: int, y: int] # GP0(E4h)
    drawingAreaOffset: tuple[x: int, y: int] # GP0(E5h)

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

var
  commandQueue: Deque[word]
  resultQueue: Deque[word]

proc readyToSendVRAM: bool =
  resultQueue.len > 0

proc readyToReceiveDMA: bool =
  # TODO is this right?
  not readyToSendVRAM()

proc readyToReceiveCommand: bool =
  readyToReceiveDMA()

let
  command: BitSlice[int, word] = (pos: 24, width: 8)
  byte1: BitSlice[int, word] = (pos: 16, width: 8)
  byte2: BitSlice[int, word] = (pos: 8, width: 8)
  byte3: BitSlice[int, word] = (pos: 0, width: 8)
  half1: BitSlice[int, word] = (pos: 12, width: 12)
  half2: BitSlice[int, word] = (pos: 0, width: 12)
  five1: BitSlice[int, word] = (pos: 15, width: 5)
  five2: BitSlice[int, word] = (pos: 10, width: 5)
  five3: BitSlice[int, word] = (pos: 5, width: 5)
  five4: BitSlice[int, word] = (pos: 0, width: 5)

proc processCommand =
  discard

proc gp0*(value: word) =
  logger.debug fmt"GP0 {value:08x}"
  commandQueue.addLast(value)
  processCommand()
  # case value[byte1]
  # of 0x1f:
  #   # Interrupt requested
  #   control.interruptRequested = true
  #   irqs.signal 1

proc gp1*(value: word) =
  logger.debug fmt"GP1 {value:08x}"
  case value[command]
  of 0:
    # Reset
    let cmds= [0x01_000000u32, 0x02_000000u32, 0x03_000001u32, 0x04_000000u32,
               0x05_000000u32, 0x06_c00_200u32, 0x07_100_010u32, 0x08000001u32]
    for cmd in cmds:
      gp1 cmd

    for cmd in 0xe1..0x6:
      gp0 (word(cmd) shl 24)
  of 1:
    # Reset command buffer
    commandQueue.clear
    if resultQueue.len > 0:
      logger.warn "Result queue non-empty on GP1(1)"
    resultQueue.clear
  of 2:
    # Acknowledge interrupt
    control.interruptRequested = false
  of 3:
    # Display enable
    screen.enabled = not (value.testBit 0)
  of 4:
    # DMA direction
    control.dmaDirection = DMADirection(value and 3)
  of 5:
    # Start of display area
    drawing.displayAreaStart = (x: value[half1], y: value[half2])
  of 6:
    # Horizontal display range
    screen.horizontalRange = (start: value[half1], stop: value[half2])
  of 8:
    # Display mode
    screen.horizontalRes =
      if value.testBit 6: HorizontalRes(value and 3) else: Res368
    screen.verticalRes =
      if value.testBit 2: Res480 else: Res240
    region =
      if value.testBit 3: PAL else: NTSC
    drawing.displayAreaDepth =
      ColourDepth((value shr 4) and 1)
    screen.verticalInterlace = value.testBit 5
  of 9:
    # Texture disable
    textures.allowDisable = value.testBit 0
  of 0x10..0x1f:
    # Get GPU info
    case value and 0x10
    of 2:
      # Texture window
      control.result = 0
      control.result[five4] = textures.mask.x
      control.result[five3] = textures.mask.y
      control.result[five2] = textures.offset.x
      control.result[five1] = textures.offset.y
    of 3:
      # Drawing area top left
      control.result = 0
    else: discard
    # TODO: not all cases handled yet
  else:
    logger.warn fmt"Unrecognised GP1 command {value:08x}"

proc gpuread*: word =
  if resultQueue.len == 0:
    result = control.result
    logger.debug fmt"GPUREAD returns {result:08x} (result)"
  else:
    result = resultQueue.popFirst()
    logger.debug fmt"GPUREAD returns {result:08x} (VRAM)"

proc gpustat*: word =
  let bit25 =
    case control.dmaDirection
    of DMADirection.Write: readyToReceiveDMA()
    of DMADirection.Read: readyToSendVRAM()
    else: false
  result =
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
  logger.debug fmt"GPUSTAT returned {result:08x}"

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
  events.after(clockRate div refreshRate[region]) do:
    irqs.signal 0
    signalVBlank
