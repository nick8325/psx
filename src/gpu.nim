## The GPU.

import machine, utils, irq
import std/[bitops, strformat, logging]

var logger = newLogger("GPU", lvlDebug)

type
  GPUStat = distinct word
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
    Command = 2,
    ReadVRAM = 3

type
  # Texture settings
  TextureSettings = object
    # x is multiplied by 64, y by 256
    base: tuple[x: int, y: int]    # GPUSTAT 0-4
    mask: tuple[x: int, y: int]    # GP0(E2h).0-9
    offset: tuple[x: int, y: int]  # GP0(E2h).10-19
    colourMode: TextureColourMode  # GPUSTAT 7-8
    enabled: bool                  # GPUSTAT 15
    flip: tuple[x: bool, y: bool]  # GP0(E1h).12-13

  # Drawing (to framebuffer) settings
  DrawingSettings = object
    transparency: TransparencyMode # GPUSTAT 5-6
    setMaskBit: bool               # GPUSTAT 11
    skipMaskedPixels: bool         # GPUSTAT 12
    drawToDisplayArea: bool        # GPUSTAT 10
    displayAreaDepth: ColourDepth  # GPUSTAT 21
    dither: bool                   # GPUSTAT 9

  # Screen settings
  ScreenSettings = object
    horizontalRes: HorizontalRes   # GPUSTAT 16-18
    verticalRes: VerticalRes       # GPUSTAT 19
    region: Region                 # GPUSTAT 20
    verticalInterlace: bool        # GPUSTAT 22
    enabled: bool                  # GPUSTAT 23
    oddLine: bool                  # GPUSTAT 31

  ControlSettings = object
    interruptRequested: bool       # GPUSTAT 24
    dmaDirection: DMADirection     # GPUSTAT 29-30

var
  textures = TextureSettings(enabled: true)
  drawing = DrawingSettings()
  screen = ScreenSettings(enabled: true)
  control = ControlSettings()

func readyToReceiveCommand: bool = true
func readyToReceiveDMA: bool = true
func readyToSendVRAM: bool = true

proc gp0*(value: word) =
  logger.debug fmt"GP0 {value:08x}"

proc gp1*(value: word) =
  logger.debug fmt"GP1 {value:08x}"

proc gpuread*: word =
  logger.debug fmt"GPUREAD"

proc gpustat*: word =
  let bit25 =
    case control.dmaDirection
    of Off: false
    of Unknown: false
    of Command: readyToReceiveDMA()
    of ReadVRAM: readyToSendVRAM()
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
    word(screen.region) shl 20 or
    word(drawing.displayAreaDepth) shl 21 or
    word(screen.verticalInterlace) shl 22 or
    word(screen.enabled) shl 23 or
    word(control.interruptRequested) shl 24 or
    word(bit25) shl 25 or
    word(readyToReceiveCommand()) shl 26 or
    word(readyToSendVRAM()) shl 27 or
    word(readyToReceiveDMA()) shl 28 or
    word(control.dmaDirection) shl 29 or
    word(screen.oddLine) shl 31
  logger.debug fmt"GPUSTAT returned {result:08x}"

proc gpuWriteDMA*(value: word) =
  logger.debug fmt"GPU DMA write {value:08x}"

proc gpuReadDMA*: word =
  logger.debug "GPU DMA read"

# VBLANK IRQ
proc signalVBlank =
  events.after(clockRate div refreshRate[screen.region]) do:
    irqs.signal 0
    signalVBlank
