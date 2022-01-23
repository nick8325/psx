import sdl2, sdl2/gfx
import machine, rasteriser, basics, eventqueue
import std/segfaults

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

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break

  # TO DO: capture 'surface' some amount of time after emu VSYNC
  # Figure out what rate to run main loop at
  # (maybe better with polled keyboard inputs to match hardware?)
  runSystem(refreshRate[region])
  var dstRect = rect
  var srcRect = rect
  surface.blitSurface nil, window.getSurface, nil
  discard window.updateSurface()
  if fps.getFramerate != refreshRate[region].cint:
    fps.setFramerate refreshRate[region].cint
  fps.delay

  if events.time >= 5*clockRate: break

destroy render
destroy window
