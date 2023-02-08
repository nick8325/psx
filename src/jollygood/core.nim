## A Jolly Good core for the emulator.
import jollygood/binding

import machine, rasteriser, basics, eventqueue, irq, gpu, savestates, cdrom, timer, cpu
import std/options

type
  Framebuffer = ptr array[512, array[1024, uint32]]


# TODO: gamma ramp
# TODO: exe loading
# TODO: read CD from game info
# TODO: proper input support

proc step(): float =
  let before = events.now()
  runSystem(nextVBlankDelta())
  runSystem(vblankClocks())
  let after = events.now()

  let framebuffer = cast[Framebuffer](buffer())

  let lines = renderedLines()
  let area = displayArea()
  let width = screenWidth()
  let height = screenHeight()

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

  return (after - before).float / clockRate.float

let
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
    audioChannels: 2,

    inputs: @[],
    loadGame: proc(): bool = true,
    step: step)

registerCore core
