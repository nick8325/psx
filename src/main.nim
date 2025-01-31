import sdl2, sdl2/gfx
import machine, rasteriser, basics, eventqueue, irq, gpu, savestates, cdrom, timer, cpu
import std/os
import std/[monotimes, options, heapqueue, tables, algorithm]
#import std/strformat
import imguin/nimsdl2_opengl
include imguin/simple

discard sdl2.init(INIT_EVERYTHING)

var
  window: WindowPtr
  render: RendererPtr

window = createWindow("PSX Emulator", 100, 100, 1024, 768, SDL_WINDOW_SHOWN)
render = createRenderer(window, -1, Renderer_Accelerated or Renderer_PresentVsync or Renderer_TargetTexture)

let
  surface = createRGBSurfaceFrom(addr vram, 1024, 512, 32, 4*1024, 0xff, 0xff00u32, 0xff0000u32, 0)

var
  pixel: Pixel
  pixelSurface = createRGBSurfaceFrom(addr pixel, 1, 1, 32, 4, 0xff, 0xff00u32, 0xff0000u32, 0)

var
  width, height: int
  display: SurfacePtr = nil
  showVRam: bool = false
  fastForward: bool = true

proc resizeDisplay: bool =
  let newWidth = screenWidth()
  let newHeight = screenHeight()

  if width != newWidth or height != newHeight or display == nil:
    width = newWidth
    height = newHeight
    display.destroy
    display = createRGBSurface(0, width.int32, height.int32, 32, 0xff, 0xff00, 0xff0000, 0)
    result = true
  else:
    result = false

discard resizeDisplay()

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

var prevClocks: MonoTime = getMonoTime()
events.every(proc: int64 = clockRate, "rate") do():
  let clocks: MonoTime = getMonoTime()
  #echo fmt"{(clocks.ticks-prevClocks.ticks).float/1000000000}s to simulate one second"
  prevClocks = clocks

events.every(proc: int64 = clockRate, "dump ram") do():
  dumpRAM("ram")

if paramCount() >= 1:
  assert loadEXE(readFile(paramStr(1)))

var state: State = save()

var lastFrameTime = getMonoTime()

events.every(proc: int64 = clockRate, "histogram") do():
  var heap: HeapQueue[tuple[count: int, pc: word]]
  var total: int = 0
  for pc, count in histogram.pairs:
    total += count
    heap.push((count: count, pc: pc))
    if heap.len > 20:
      discard heap.pop()
  histogram.clear()
  var sorted: seq[tuple[count: int, pc: word]]
  while heap.len > 0:
    sorted.add heap.pop()
  sorted.reverse()

#  echo "Histogram:"
#  for pair in sorted:
#    let freq = float(pair.count) / float(total)
#    echo fmt"  {pair.pc:8x}: {freq*100:.2f}% ({pair.count} times)"
#  echo ""

var showDemo = false
var showMenu = false
igCreateContext(nil)
#defer: igDestroyContext(nil)

doAssert ImGui_ImplSdl2_InitForSDLRenderer(cast[ptr SdlWindow](window) , cast[ptr SdlRenderer](render))
#defer: ImGui_ImplSDL2_Shutdown()

doAssert ImGui_ImplSdlRenderer2_Init(cast[ptr SdlRenderer](render))
#defer: ImGui_ImplOpenGL3_Shutdown()

igStyleColorsClassic(nil)

while runGame:
  while pollEvent(evt):
    ImGui_ImplSDL2_ProcessEvent(cast[ptr SdlEvent](addr evt))
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

      of K_V:
        showVRam = not showVRam

      of K_W:
        fastForward = not fastForward

      of K_T:
        timerDebug = not timerDebug

      of K_SPACE:
        paused = false

      of K_P:
        pauseOnPrimitive = not pauseOnPrimitive
        paused = false

      of K_M:
        showMenu = not showMenu

      of K_D:
        showDemo = not showDemo

      else:
        echo "unknown key"

  ImGui_ImplSdlRenderer2_NewFrame()
  ImGui_ImplSdl2_NewFrame()
  igNewFrame()

  if showDemo:
    igShowDemoWindow(addr showDemo)
  if showMenu:
    if igBeginMainMenuBar():
      if igBeginMenu("Options", true):
        igMenuItemBoolPtr("Pause", "p", addr paused, true)
        igMenuItemBoolPtr("Fast forward", "w", addr fastForward, true)
        igMenuItemBoolPtr("Debug timers", "t", addr timerDebug, true)
        igMenuItemBoolPtr("Show VRAM", "v", addr showVram, true)
        igMenuItemBoolPtr("Show Dear ImGui demo", "d", addr showDemo, true)
        igEndMenu()
      igEndMainMenuBar()

  # TODO: this seems to be running slow. Clean up the vblank handling.
  runSystem(nextVBlankDelta())
  runSystem(vblankClocks())

  if fps.getFramerate != refreshRate[region].cint:
    fps.setFramerate refreshRate[region].cint
  if not fastForward: fps.delay

  let resized = resizeDisplay()
  let lines =
    if resized: none(bool)
    else: renderedLines()
  let area = displayArea()

  for i in 0..<height:
    let shouldDraw =
      if lines.isNone(): true
      else: lines.get.int == i mod 2
    if shouldDraw:
      if displayAreaDepth() == Depth15 or showVram:
        var srcRect = rect(area.x1.cint, (area.y1 + i).cint, width.cint, 1)
        var destRect = rect(0, i.cint, width.cint, 1)
        surface.blitSurface addr(srcRect), display, addr(destRect)
      else:
        # 24-bit - we can't easily blit, but have to copy one by one
        for j in 0..<width:
          pixel = getPixel24(area.x1, j, area.y1 + i)
          var srcRect = rect(0, 0, width.cint, 1)
          var destRect = rect(j.cint, i.cint, 1, 1)
          pixelSurface.blitSurface addr(srcRect), display, addr(destRect)

  let time = getMonoTime()
  lastFrameTime = time
  let texture = render.createTextureFromSurface(if showVRam: surface else: display)
#  discard render.setLogicalSize(if showVRam: 1024 else: width, if showVRam: 512 else: height)
  render.setDrawColor 0, 0, 0, 255
  render.clear
  if displayEnabled() or showVRam:
    render.copy texture, nil, nil
  texture.destroy

  igRender()
  ImGui_ImplSdlRenderer2_RenderDrawData(igGetDrawData())
  render.present

destroy render
destroy window
