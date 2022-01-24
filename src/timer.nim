## Timers.

import basics, utils, irq
import std/[bitops, strformat]

const loggerComponent = logTimer

type
  TimerMode = distinct word
  Timer* = object
    mode: TimerMode
    value: word
    target: uint16
    lastSeen: uint64

TimerMode.bitfield sync, bool, 0, 1
TimerMode.bitfield syncMode, int, 1, 2
TimerMode.bitfield resetAfterTarget, bool, 3, 1
TimerMode.bitfield irqAtTarget, bool, 4, 1
TimerMode.bitfield irqAtMax, bool, 5, 1
TimerMode.bitfield repeat, bool, 6, 1
TimerMode.bitfield toggleIRQ, bool, 7, 1
TimerMode.bitfield clockSource, int, 8, 2
TimerMode.bitfield irq, bool, 10, 1
TimerMode.bitfield reachedTarget, bool, 11, 1
TimerMode.bitfield reachedMax, bool, 12, 1

var
  timers*: array[3, Timer]

proc `mode=`*(timer: var Timer, mode: word) =
  discard

func mode*(timer: var Timer): word =
  result = timer.mode.word
  timer.mode.reachedTarget = false
  timer.mode.reachedMax = false

proc `value=`*(timer: var Timer, value: word) =
  discard

func value*(timer: Timer): word =
  timer.value.word

proc `target=`*(timer: var Timer, target: word) =
  discard

func target*(timer: Timer): word =
  timer.target
