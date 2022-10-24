import sdl2, sdl2/gfx
import machine, rasteriser, basics, eventqueue, irq, gpu, savestates, cdrom
import std/os
import std/[strformat, monotimes]

discard sdl2.init(INIT_EVERYTHING)

var
  window: WindowPtr
  render: RendererPtr

window = createWindow("PSX Emulator", 100, 100, 1024, 512, SDL_WINDOW_SHOWN)
#render = createRenderer(window, -1, Renderer_Accelerated or Renderer_PresentVsync or Renderer_TargetTexture)

let surface = createRGBSurfaceFrom(addr vram, 1024, 512, 32, 4*1024, 0xff, 0xff00u32, 0xff0000u32, 0)

var
  evt = sdl2.defaultEvent
  runGame = true

var fps: FpsManager
fps.init
fps.setFramerate refreshRate[region].cint

func rampVal(i: int): int =
  const
    midpoint = 64
    midval = 128*256
    endpoint = 256
    endval = 256*256

  if i < midpoint:
    i * midval div midpoint
  else:
    midval + (i - midpoint) * (endval - midval) div (endpoint - midpoint)

var ramp: array[256, uint16]
for i in 0..<256: ramp[i] = rampVal(i).uint16
discard window.setGammaRamp(addr ramp[0], addr ramp[0], addr ramp[0])

# var prevClocks: MonoTime = getMonoTime()
# events.every(proc: int64 = clockRate, "rate") do():
#   let clocks: MonoTime = getMonoTime()
#   echo fmt"{(clocks.ticks-prevClocks.ticks).float/1000000000}s to simulate one second"
#   prevClocks = clocks

events.every(proc: int64 = clockRate, "dump ram") do():
  dumpRAM("ram")

if paramCount() >= 1:
  loadEXE(readFile(paramStr(1)))

var state: State = save()

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break
    elif evt.kind == KeyDown:
      case evt.key.keysym.sym
      of K_S:
        echo "saving"
        state = save()

      of K_L:
        echo "loading"
        load(state)

      of K_I:
        echo "triggering IRQs"
        for irq in 0..10:
          irqs.signal(irq)

      of K_C:
        echo dumpCDROM()

      else:
        echo "unknown key"

  # TODO: this seems to be running slow. Clean up the vblank handling.
  runSystem(nextVBlankDelta())
  runSystem(vblankClocks())

  surface.blitSurface nil, window.getSurface, nil
  discard window.updateSurface()
  if fps.getFramerate != refreshRate[region].cint:
    fps.setFramerate refreshRate[region].cint
#  fps.delay

destroy render
destroy window
