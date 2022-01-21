import sdl2
import machine, rasteriser

discard sdl2.init(INIT_EVERYTHING)

var
  window: WindowPtr
  render: RendererPtr

window = createWindow("PSX Emulator", 100, 100, 1024, 512, SDL_WINDOW_SHOWN)
#render = createRenderer(window, -1, Renderer_Accelerated or Renderer_PresentVsync or Renderer_TargetTexture)

let surface = createRGBSurfaceFrom(addr vram, 1024, 512, 16, 2*1024, 0x1f, 0x1fu32 shl 5, 0x1fu32 shl 10, 0)

var
  evt = sdl2.defaultEvent
  runGame = true

while runGame:
  while pollEvent(evt):
    if evt.kind == QuitEvent:
      runGame = false
      break

  runSystem()
  var dstRect = rect
  var srcRect = rect
  surface.blitSurface nil, window.getSurface, nil
  discard window.updateSurface()

destroy render
destroy window
