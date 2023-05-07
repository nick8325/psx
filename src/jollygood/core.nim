## A Jolly Good core for the emulator.
import jollygood/binding

import machine, rasteriser, basics, eventqueue, irq, gpu, savestates, cdrom, timer, cpu, spu
import std/[options, strformat]

type
  DebugButton = enum
    dbSingleStep,
    dbSingleStepGPU,
    dbContinue,
    dbTriggerIRQs,
    dbShowVram,
    dbWireframeGraphics,
    dbDumpCDROM,
    dbDebugTimers

const
  debugButtonNames: array[DebugButton, string] =
    ["Single step", "Single step GPU", "Continue",
     "Trigger all IRQs", "Show VRAM",
     "Wireframe graphics", "Dump CDROM", "Debug timers"]

type
  Framebuffer = ptr array[512, array[1024, uint32]]

# TODO: gamma ramp
# TODO: read CD from game info
# TODO: proper input support

proc loadGame(): bool =
  discard loadEXE(gameFile.data)
  true

var
  showVRam: bool = false

proc step(): float =
  let debugPressed = inputs[0].buttonPressed

  if debugPressed[dbSingleStep.int]:
    paused = false
    pauseOnPrimitive = false
  defer:
    if debugPressed[dbSingleStep.int]:
      paused = true

  if debugPressed[dbSingleStepGPU.int]:
    paused = false
    pauseOnPrimitive = true

  if debugPressed[dbContinue.int]:
    paused = false
    pauseOnPrimitive = false

  if debugPressed[dbTriggerIRQs.int]:
    for irq in 0..10:
      irqs.signal(irq)

  if debugPressed[dbShowVRAM.int]:
    showVRam = not showVRam

  if debugPressed[dbWireframeGraphics.int]:
    wireframe = not wireframe

  if debugPressed[dbDumpCDROM.int]:
    echo dumpCDROM()

  if debugPressed[dbDebugTimers.int]:
    timerDebug = not timerDebug

  let before = events.now()
  var after: int64
  runSystem(nextVBlankDelta())
  runSystem(vblankClocks())
  if paused:
    after = before + clockRate div 50
  else:
    after = events.now()

  let framebuffer = cast[Framebuffer](buffer())

  var lines = renderedLines()
  var area = displayArea()
  var width = screenWidth()
  var height = screenHeight()

  if showVram:
    lines = none(bool)
    area.x1 = 0
    area.y1 = 0
    width = 1024
    height = 512

  let shouldDrawFrame =
    displayEnabled() and width >= 100 and height >= 100

  if not shouldDrawFrame:
    for row in framebuffer[].mitems():
      for pixel in row.mitems():
        pixel = 0
    render(0, 0, 640, 480)
  else:
    for i in 0..<height:
      let shouldDrawLine =
        if lines.isNone(): true
        else: lines.get.int == i mod 2
      if shouldDrawLine:
        for j in 0..<width:
          let pixel =
            case displayAreaDepth()
            of Depth15: getPixel(area.x1 + j, area.y1 + i)
            of Depth24: getPixel24(area.x1, j, area.y1 + i)
          framebuffer[][i][j] = pixel.uint32
    render(0, 0, width, height)

  playAudio(audioBuffer)
  audioBuffer = @[]

  return (after - before).float / clockRate.float

let
  debugInput = Input(
    kind: ikController,
    name: "debugger",
    fullName: "Debug controls",
    axes: @[],
    buttons: @debugButtonNames)

  core = Core(
    name: "psx",
    fullName: "my toy psx emulator",
    version: "0.1",
    system: "psx",

    pixelFormat: pfXBGR8888,
    videoBufferSize: (width: 1024, height: 512),
    initialResolution: (width: 640, height: 480),
    aspectRatio: 4/3,

    audioSampleFormat: sfInt16,
    audioRate: 44100,
    audioChannels: 1,

    inputs: @[debugInput],
    loadGame: loadGame,
    step: step)

registerCore core
