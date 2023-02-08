import nimterop/cimport
import std/[strformat, strutils]

const
  exports = [
    "jg_set_cb_log", "jg_set_cb_audio", "jg_set_cb_frametime", "jg_set_cb_rumble",
    "jg_init", "jg_deinit", "jg_reset", "jg_exec_frame", "jg_game_load",
    "jg_game_unload", "jg_state_load", "jg_state_load_raw", "jg_state_save",
    "jg_state_save_raw", "jg_state_size", "jg_media_select", "jg_media_insert",
    "jg_cheat_clear", "jg_cheat_set", "jg_rehash", "jg_data_push", "jg_get_coreinfo",
    "jg_get_videoinfo", "jg_get_audioinfo", "jg_get_inputinfo", "jg_get_settings",
    "jg_setup_video", "jg_setup_audio", "jg_set_inputstate", "jg_set_gameinfo",
    "jg_set_auxinfo", "jg_set_paths"]
{.pragma: api, exportc, dynlib.}

cImport(@["/usr/include/jg/jg.h", "/usr/include/jg/jg_psx.h"],
        flags=fmt"--prefix _ --symOverride={exports.join "",""}")

static:
  assert JG_VERSION_MAJOR == 1 and JG_VERSION_MINOR == 0

template callback(varname, vartype, cbname: untyped) =
  var varname* {.inject.}: vartype
  proc cbname(arg: vartype) {.api, inject.} =
    varname = arg

callback(jgLog, jg_cb_log_t, jg_set_cb_log)
callback(jgAudio, jg_cb_audio_t, jg_set_cb_audio)
callback(jgFrametime, jg_cb_frametime_t, jg_set_cb_frametime)
callback(jgRumble, jg_cb_rumble_t, jg_set_cb_rumble)

type
  constpointer {.importc: "const void *", nodecl.} = pointer
  constcstring {.importc: "const char *", nodecl.} = cstring

proc jg_init: cint {.api.} =
  echo "jg_init"
  0

proc jg_deinit {.api.} =
  echo "jg_deinit"
  discard

proc jg_reset(kind: cint) {.api.} =
  echo "jg_reset"
  discard

proc jg_exec_frame {.api.} =
  echo "jg_exec_frame"
  discard

proc jg_game_load: cint {.api.} =
  echo "jg_game_load"
  0

proc jg_game_unload: cint {.api.} =
  echo "jg_game_unload"
  0

proc jg_state_load(state: constcstring): cint {.api.} =
  echo "jg_state_load"
  0

proc jg_state_load_raw(data: constpointer) {.api.} =
  echo "jg_state_load_raw"
  discard

proc jg_state_save(state: constcstring): cint {.api.} =
  echo "jg_state_save"
  0

proc jg_state_save_raw: constpointer {.api.} =
  echo "jg_state_save_raw"
  discard

proc jg_state_size: csize_t {.api.} =
  echo "jg_state_size"
  0

proc jg_media_select {.api.} =
  echo "jg_media_select"
  discard

proc jg_media_insert {.api.} =
  echo "jg_media_insert"
  discard

proc jg_cheat_clear {.api.} =
  echo "jg_cheat_clear"
  discard

proc jg_cheat_set(cheat: constcstring) {.api.} =
  echo "jg_cheat_set"
  discard

proc jg_rehash {.api.} =
  echo "jg_rehash"
  discard

proc jg_data_push(kind: uint32, port: cint, buf: constpointer, size: csize_t) {.api.} =
  echo "jg_data_push"
  discard

var coreInfo: jg_coreinfo_t
coreInfo.name = "psx"
coreInfo.fname = "my teeny psx emulator"
coreInfo.version = "0.1"
coreInfo.sys = "psx"
coreInfo.numinputs = 2
coreInfo.hints = 0
proc jg_get_coreinfo(system: constcstring): ptr jg_coreinfo_t {.api.} =
  echo "jg_get_coreinfo"
  return addr(coreInfo)

var videoInfo: jg_videoinfo_t
proc jg_get_videoinfo: ptr jg_videoinfo_t {.api.} =
  echo "jg_get_videoinfo"
  addr(videoInfo)

var audioInfo: jg_audioinfo_t
proc jg_get_audioinfo: ptr jg_audioinfo_t {.api.} =
  echo "jg_get_audioinfo"
  addr(audioInfo)

var inputInfo: jg_inputinfo_t
proc jg_get_inputinfo(port: cint): ptr jg_inputinfo_t {.api.} =
  echo "jg_get_inputinfo"
  addr(inputInfo)

proc jg_get_settings(psize: ptr csize_t): ptr UncheckedArray[jg_setting_t] {.api.} =
  echo "jg_get_settings"
  psize[] = 0
  nil

proc jg_setup_video {.api.} =
  echo "jg_setup_video"
  discard

proc jg_setup_audio {.api.} =
  echo "jg_setup_audio"
  discard

proc jg_set_inputstate(state: ptr jg_inputstate_t, port: cint) {.api.} =
  echo "jg_set_inputstate"
  discard

proc jg_set_gameinfo(info: jg_fileinfo_t) {.api.} =
  echo "jg_set_gameinfo"
  discard

proc jg_set_auxinfo(info: jg_fileinfo_t, num: cint) {.api.} =
  echo "jg_set_auxinfo"
  discard

proc jg_set_paths(paths: jg_pathinfo_t) {.api.} =
  echo "jg_set_paths"
  discard
