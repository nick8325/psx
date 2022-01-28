## Timers.

import basics, utils, irq, eventqueue
import std/[bitops, strformat]

const loggerComponent = logTimer

type
  TimerMode = distinct word
  Timer* = ref object
    mode: TimerMode
    value: uint16
    target: uint16
    lastSeen: int64

TimerMode.bitfield sync, bool, 0, 1
TimerMode.bitfield syncMode, int, 1, 2
TimerMode.bitfield resetAfterTarget, bool, 3, 1
TimerMode.bitfield irqAtTarget, bool, 4, 1
TimerMode.bitfield irqAtMax, bool, 5, 1
TimerMode.bitfield repeat, bool, 6, 1
TimerMode.bitfield toggleIRQ, bool, 7, 1
TimerMode.bitfield clockSource, int, 8, 2
TimerMode.bitfield noIRQ, bool, 10, 1
TimerMode.bitfield reachedTarget, bool, 11, 1
TimerMode.bitfield reachedMax, bool, 12, 1

func makeTimer: Timer =
  Timer()

var
  timers*: array[3, Timer] = [makeTimer(), makeTimer(), makeTimer()]

proc catchUp(timer: Timer) =
  discard

proc nextTime(timer: Timer): int64 =
  int64.high

proc schedule(timer: Timer) =
  # XXX a whole load of event loops are going to be created
  # - when the timer changes the old event loop will still keep going
  discard
#  let next = timer.nextTime()
#  if next != int64.high:
#    events.at(next) do():
#      timer.catchUp()
#      assert timer.nextTime() != 0
#      timer.schedule()

proc `mode=`*(timer: Timer, mode: word) =
  discard

func mode*(timer: Timer): word =
  timer.catchUp()
  result = timer.mode.word
  timer.mode.noIRQ = true
  timer.mode.reachedTarget = false
  timer.mode.reachedMax = false

proc `value=`*(timer: Timer, value: word) =
  timer.catchUp()
  timer.value = uint16(value and 0xffff)
  timer.schedule()

func value*(timer: Timer): word =
  timer.catchUp()
  timer.value.word

proc `target=`*(timer: Timer, target: word) =
  timer.catchUp()
  timer.target = uint16(target and 0xffff)
  timer.schedule()

func target*(timer: Timer): word =
  timer.catchUp()
  timer.target.word
