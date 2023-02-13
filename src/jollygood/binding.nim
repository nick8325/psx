## A binding to the Jolly Good emulation API. For writing emulation cores.
import jollygood/header
import std/sequtils

######################################################################
# The "Core" type. This is what the core author has to define.

type
  Core* = object
    ## An emulation core.
    name*: string
      ## A short name
    fullName*: string
      ## A longer, descriptive name
    version*: string
      ## A version number
    system*: string
      ## What system we emulate - for a list, see the JGRF project,
      ## file jgrf.c, function jgrf_core_default

    pixelFormat*: PixelFormat
      ## Wanted pixel format of the video buffer
    videoBufferSize*: tuple[width, height: int]
      ## Wanted video buffer size
    initialResolution*: tuple[width, height: int]
      ## Initial screen resolution (used to determine window size)
    aspectRatio*: float
      ## Pixel aspect ratio

    audioSampleFormat*: SampleFormat
      ## Audio sample format
    audioRate*: int
      ## Audio rate in Hz
    audioChannels*: int
      ## Number of audio channels

    inputs*: seq[Input]
      ## What inputs (e.g. joysticks) are attached to the console

    # TODO: allow changing inputs at runtime
    # TODO: add support for settings
    # TODO: add other callbacks such as unload, save/load state, etc.

    loadGame*: proc(): bool
      ## Load a game. Return true on success.
    step*: proc: float
      ## Step forward one frame. Return how much time elapsed.

  PixelFormat* = enum
    ## A pixel format.
    pfXRGB8888, ## 32-bit, LSB=blue
    pfXBGR8888, ## 32-bit, LSB=red
    pfRGBX5551, ## 16-bit, MSBs=red, LSB unused
    pfRGB565    ## 16-bit, MSBs=red, green has 6 bits

  SampleFormat* = enum
    ## An audio sample format.
    sfInt16, ## 16-bit signed samples
    sfFloat32 ## 32-bit floating point samples

  Input* = object
    ## An input device (e.g. joystick).
    kind*: InputKind
      ## What kind of input is it?
    name*, fullName*: string
      ## Short and long names for the device
    axes*: seq[string]
      ## Names for the analogue axes
    buttons*: seq[string]
      ## Names for the digital buttons

  InputKind* = enum
    ## An input type
    ikController, ikGun, ikKeyboard, ikPointer, ikSpinner, ikTouch, ikExternal

######################################################################
# The API available to the core author.

proc registerCore*(core: Core)
  ## Register a core. Can only be called once.

type
  LogLevel* = enum
    ## A log verbosity level
    logDebug, ## Debug message
    logInfo,  ## Informational message
    logWarn,  ## Loud warning
    logError, ## Fatal error
    logScreen ## Display on the GUI

proc log*(level: LogLevel, args: string)
  ## Write a message to the log

type
  File* = object
    ## A file, e.g. a game file
    path*: string
      ## A full path to the file
    name*: string
      ## The filename without directory
    basename*: string
      ## The filename without directory or extensoion
    data*: string
      ## The file contents

var
  gameFile*: File
    ## The current game file

type
  InputState* = object
    ## The current state of a particular input device
    axis*: seq[int16]
      ## The values of the analogue axes
    button*: seq[bool]
      ## The values of the buttons
    buttonPressed*: seq[bool]
      ## True for buttons which have just been pressed this clock cycle
    buttonReleased*: seq[bool]
      ## True for buttons which have just been released this clock cycle

var
  inputs*: seq[InputState]
    ## The current state of the input devices

proc playAudio*(samples: seq[int16])
  ## Feed samples into the audio buffer
proc playAudio*(samples: seq[float32])
  ## Feed samples into the audio buffer

proc buffer*: pointer
  ## The screen buffer, in the format requested in the "Core" record

proc render*(x, y, w, h: int)
  ## Draw a part of the buffer to the screen.

######################################################################
# The implementation.

var
  theCore: Core
  coreLoaded: bool = false
  coreInfo: jg_coreinfo_t
  videoInfo: jg_videoinfo_t
  audioInfo: jg_audioinfo_t
  inputInfo: seq[jg_inputinfo_t]
  inputState: seq[ptr jg_inputstate_t]

proc core: Core =
  assert coreLoaded
  theCore

static:
  assert JG_VERSION_MAJOR == 1 and JG_VERSION_MINOR == 0

static:
  # Check that our enum definitions match up with jg.h.
  proc checkEnum[T, U](values: openArray[(T, U)]) =
    for pair in values:
      assert pair[0].int == pair[1].int

  checkEnum {
    pfXRGB8888: JG_PIXFMT_XRGB8888,
    pfXBGR8888: JG_PIXFMT_XBGR8888,
    pfRGBX5551: JG_PIXFMT_RGBX5551,
    pfRGB565: JG_PIXFMT_RGB565
  }

  checkEnum {
    sfInt16: JG_SAMPFMT_INT16,
    sfFloat32: JG_SAMPFMT_FLT32
  }

  checkEnum {
    ikController: JG_INPUT_CONTROLLER,
    ikGun: JG_INPUT_GUN,
    ikKeyboard: JG_INPUT_KEYBOARD,
    ikPointer: JG_INPUT_POINTER,
    ikSpinner: JG_INPUT_SPINNER,
    ikTouch: JG_INPUT_TOUCH,
    ikExternal: JG_INPUT_EXTERNAL
  }

  checkEnum {
    logDebug: JG_LOG_DBG,
    logInfo: JG_LOG_INF,
    logWarn: JG_LOG_WRN,
    logError: JG_LOG_ERR,
    logScreen: JG_LOG_SCR
  }

{.pragma: api, exportc, dynlib.}

template callback(varname, vartype, cbname: untyped) =
  var varname* {.inject.}: vartype
  proc cbname(arg: vartype) {.api, inject.} =
    varname = arg

callback(callbackLog, jg_cb_log_t, jg_set_cb_log)
callback(callbackAudio, jg_cb_audio_t, jg_set_cb_audio)
callback(callbackFrametime, jg_cb_frametime_t, jg_set_cb_frametime)
callback(callbackRumble, jg_cb_rumble_t, jg_set_cb_rumble)

type
  constpointer {.importc: "const void *", nodecl.} = pointer
  constcstring {.importc: "const char *", nodecl.} = cstring

template trace(name: string) =
  #echo name
  discard

proc jg_init: cint {.api.} =
  trace "jg_init"
  1

proc jg_deinit {.api.} =
  trace "jg_deinit"
  discard

proc jg_reset(kind: cint) {.api.} =
  trace "jg_reset"
  discard

proc jg_exec_frame {.api.} =
  trace "jg_exec_frame"

  # Update controller buttons
  for port in 0..<inputs.len:
    let state = inputState[port]
    let axisPtr = cast[ptr UncheckedArray[int16]](state[].axis)
    let buttonPtr = cast[ptr UncheckedArray[bool]](state[].button)
    let numAxes = core().inputs[port].axes.len
    let numButtons = core().inputs[port].buttons.len
    let axis = axisPtr.toOpenArray(0, numAxes-1).toSeq
    let button = buttonPtr.toOpenArray(0, numButtons-1).toSeq
    inputs[port].axis = axis
    for i in 0 ..< button.len:
      inputs[port].buttonPressed[i] = button[i] and not inputs[port].button[i]
      inputs[port].buttonReleased[i] = not button[i] and inputs[port].button[i]
    inputs[port].button = button

  let frameTime = core().step()
  callbackFrametime(1/frameTime)

proc jg_game_load: cint {.api.} =
  trace "jg_game_load"
  core().loadGame().cint

proc jg_game_unload: cint {.api.} =
  trace "jg_game_unload"
  1

proc jg_state_load(state: constcstring): cint {.api.} =
  trace "jg_state_load"
  0

proc jg_state_load_raw(data: constpointer) {.api.} =
  trace "jg_state_load_raw"
  discard

proc jg_state_save(state: constcstring): cint {.api.} =
  trace "jg_state_save"
  0

proc jg_state_save_raw: constpointer {.api.} =
  trace "jg_state_save_raw"
  discard

proc jg_state_size: csize_t {.api.} =
  trace "jg_state_size"
  0

proc jg_media_select {.api.} =
  trace "jg_media_select"
  discard

proc jg_media_insert {.api.} =
  trace "jg_media_insert"
  discard

proc jg_cheat_clear {.api.} =
  trace "jg_cheat_clear"
  discard

proc jg_cheat_set(cheat: constcstring) {.api.} =
  trace "jg_cheat_set"
  discard

proc jg_rehash {.api.} =
  trace "jg_rehash"
  discard

proc jg_data_push(kind: uint32, port: cint, buf: constpointer, size: csize_t) {.api.} =
  trace "jg_data_push"
  discard

proc jg_get_coreinfo(system: constcstring): ptr jg_coreinfo_t {.api.} =
  trace "jg_get_coreinfo"
  return addr(coreInfo)

proc jg_get_videoinfo: ptr jg_videoinfo_t {.api.} =
  trace "jg_get_videoinfo"
  addr(videoInfo)

proc jg_get_audioinfo: ptr jg_audioinfo_t {.api.} =
  trace "jg_get_audioinfo"
  addr(audioInfo)

proc jg_get_inputinfo(port: cint): ptr jg_inputinfo_t {.api.} =
  trace "jg_get_inputinfo"
  addr(inputInfo[port])

proc jg_get_settings(psize: ptr csize_t): ptr UncheckedArray[jg_setting_t] {.api.} =
  trace "jg_get_settings"
  psize[] = 0
  nil

proc jg_setup_video {.api.} =
  trace "jg_setup_video"
  discard

proc jg_setup_audio {.api.} =
  trace "jg_setup_audio"
  discard

proc jg_set_inputstate(state: ptr jg_inputstate_t, port: cint) {.api.} =
  trace "jg_set_inputstate"
  inputState[port] = state

proc toFile(info: jg_fileinfo_t): File =
  result.path = $info.path
  result.name = $info.fname
  result.basename = $info.name
  result.data = newStringOfCap info.size
  for i in 0 ..< info.size.int:
    result.data.add cast[cstring](info.data)[i]

proc jg_set_gameinfo(info: jg_fileinfo_t) {.api.} =
  trace "jg_set_gameinfo"
  gameFile = info.toFile

proc jg_set_auxinfo(info: jg_fileinfo_t, num: cint) {.api.} =
  trace "jg_set_auxinfo"
  discard

proc jg_set_paths(paths: jg_pathinfo_t) {.api.} =
  trace "jg_set_paths"
  discard

# TODO: allow freeing a jg_inputinfo_t too
proc toInputInfo(i: int, input: Input): jg_inputinfo_t =
  result.`type` = input.kind.int.jg_inputtype
  result.index = i.cint
  result.name = input.name.cstring
  result.fname = input.fullName.cstring
  let defs = allocCStringArray(input.axes & input.buttons)
  result.defs = cast[ptr cstring](defs)
  result.numaxes = input.axes.len.cint
  result.numbuttons = input.buttons.len.cint

proc registerCore*(core: Core) =
  # JGRF needs at least one input
  var core = core
  if core.inputs.len == 0:
    core.inputs = @[
      Input(kind: ikExternal, name: "dummy", fullName: "dummy",
            axes: @[], buttons: @[])
    ]

  assert not coreLoaded
  coreLoaded = true
  theCore = core
  coreInfo.name = core.name.cstring
  coreInfo.fname = core.fullName.cstring
  coreInfo.version = core.version.cstring
  coreInfo.sys = core.system.cstring
  coreInfo.numinputs = core.inputs.len.uint8
  coreInfo.hints = 0
  videoInfo.pixfmt = core.pixelFormat.int.jg_pixfmt
  videoInfo.wmax = core.videoBufferSize.width.cuint
  videoInfo.hmax = core.videoBufferSize.height.cuint
  videoInfo.w = core.initialResolution.width.cuint
  videoInfo.h = core.initialResolution.height.cuint
  videoInfo.x = 0
  videoInfo.y = 0
  videoInfo.p = core.videoBufferSize.width.cuint
  videoInfo.aspect = core.aspectRatio
  audioInfo.sampfmt = core.audioSampleFormat.int.jg_sampfmt
  audioInfo.rate = core.audioRate.cuint
  audioInfo.channels = core.audioChannels.cuint
  audioInfo.spf = 50 # We will make sure not to overfill the buffer

  inputInfo = @[]
  inputs = @[]
  for i in 0..<core.inputs.len:
    let info = toInputInfo(i, core.inputs[i])
    let axis = newSeq[int16](info.numaxes)
    let button = newSeq[bool](info.numbuttons)
    inputInfo.add info
    inputState.add nil
    inputs.add InputState(axis: axis, button: button, buttonPressed: button, buttonReleased: button)

proc log*(level: LogLevel, args: string) =
  callbackLog(level.cint, args.cstring)

proc playAudio*(samples: seq[int16]) =
  discard # TODO
proc playAudio*(samples: seq[float32]) =
  discard

proc buffer*: pointer =
  videoInfo.buf

proc render*(x, y, w, h: int) =
  let wmax = videoInfo.wmax.int
  let hmax = videoInfo.hmax.int
  assert x >= 0 and x <= wmax and y >= 0 and y <= hmax and
    w >= 0 and h >= 0 and x + w <= wmax and y + h <= hmax
  videoInfo.x = x.cuint
  videoInfo.y = y.cuint
  videoInfo.w = w.cuint
  videoInfo.h = h.cuint
