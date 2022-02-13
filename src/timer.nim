## Timers.

import basics, utils, irq, eventqueue, gpu
import std/[bitops, strformat, options, math]

const loggerComponent = logTimer

type
  ## Represents a signal which switches on and off repeatedly.
  RepeatingSignal = tuple
    ## How long is one complete on-off cycle?
    length: int64
    ## For how many clock cycles is the signal on?
    ## (This comes at the beginning of the cycle.)
    on: int64
    ## When did the current cycle start?
    start: int64

type
  TimerId = range[0..2]
  TimerMode = distinct word
  Timer* = object
    ## Which timer is it?
    id: TimerId

    ## Hardware fields.
    mode: TimerMode
    counter: uint16
    target: uint16

    ## Has an IRQ fired since the last write to the mode register?
    irqTriggered: bool

    ## Did the last counter increment trigger an IRQ?
    irqTriggeredNow: bool

    ## Input signal for timer synchronisation
    ## (e.g. to stop counting during HBLANK).
    syncInput: RepeatingSignal

    ## What clock cycle has the timer reached?
    time: int64

    ## A field that increments on every call to 'schedule'.
    ## Used to remove stale alarms.
    generation: int64

func `$`(signal: RepeatingSignal): string =
  fmt"length={signal.length} on={signal.on} start={signal.start}"

func `$`(timer: Timer): string =
  fmt"id={timer.id} mode={timer.mode.word:x} counter={timer.counter:x} target={timer.target:x} triggered={timer.irqTriggered} triggered_now={timer.irqTriggeredNow} sync=({timer.syncInput}) time={timer.time} gen={timer.generation}"

TimerMode.bitfield sync, bool, 0, 1
TimerMode.bitfield syncMode, range[0..3], 1, 2
TimerMode.bitfield resetAfterTarget, bool, 3, 1
TimerMode.bitfield irqAtTarget, bool, 4, 1
TimerMode.bitfield irqAtMax, bool, 5, 1
TimerMode.bitfield repeat, bool, 6, 1
TimerMode.bitfield toggleIRQ, bool, 7, 1
TimerMode.bitfield clockSourceBits, int, 8, 2
TimerMode.bitfield irq, bool, 10, 1
TimerMode.bitfield reachedTarget, bool, 11, 1
TimerMode.bitfield reachedMax, bool, 12, 1

var
  timers*: array[TimerId, Timer]

for i, timer in timers.mpairs:
  timer.id = i
  timer.time = -1
  timer.mode.irq = true
timers[1].mode.clockSourceBits = 1

type
  ClockSource {.pure.} = enum
    SystemClock, SystemClockDiv8, DotClock, HBlank

proc clockSource(timer: Timer): ClockSource =
  ## What is the clock source of a timer?

  result = SystemClock
  case timer.id
  of 0:
    if timer.mode.clockSourceBits.testBit(0): result = DotClock
  of 1:
    if timer.mode.clockSourceBits.testBit(0): result = HBlank
  of 2:
    if timer.mode.clockSourceBits.testBit(1): result = SystemClockDiv8

proc clockRate(source: ClockSource): int64 =
  ## How often does a clock source tick?

  case source
  of SystemClock: systemClock.int64
  of SystemClockDiv8: systemClock.int64 * 8
  of DotClock: clocksPerPixel()
  of HBlank: clocksPerScanline()

proc currentSyncInput(timer: Timer): RepeatingSignal =
  ## Compute the current value of the timer synchronisation input signal.

  case timer.id
  of 0: # HBLANK
    result.start = events.now() - lastHBlankDelta()
    result.on = hblankClocks()
    result.length = clocksPerScanline()
  of 1: # VBLANK
    result.start = events.now() - lastVBlankDelta()
    result.on = vblankClocks()
    result.length = clocksPerFrame()
  of 2: # Ungated - treated specially below
    result.start = events.now() - lastVBlankDelta()
    result.on = 0
    result.length = clocksPerFrame()

func irqsEnabled(timer: Timer): bool =
  ## Are IRQs enabled? (Only one-shot mode disables them.)

  (timer.mode.repeat or not timer.irqTriggered) and not timer.irqTriggeredNow

proc incsToIRQ(timer: Timer): int64 =
  ## How many times must the timer increment before we trigger an IRQ?

  result = int64.high
  if timer.irqsEnabled:
    if timer.mode.irqAtMax and (not timer.mode.resetAfterTarget or timer.target < timer.counter):
      result = result.min(int64(0xffff - timer.counter))
    if timer.mode.irqAtTarget:
      if timer.counter <= timer.target:
        result = result.min(int64(timer.target - timer.counter))
      else:
        result = result.min(timer.target.int64 + 0x10000 - timer.counter.int64)

proc advance(timer: var Timer, target: int64) =
  ## Tick the timer until it reaches a given clock cycle.
  ## Returns either when the timer reaches its target,
  ## or when an interrupt needs to be triggered.

  let rate = timer.clockSource.clockRate()

  template pause(finish: int64): void =
    # Only pause if the time is not in the past
    if timer.time < finish: timer.time = min(finish, target)
    # Stop if we are past the target time
    if finish >= target: return

  template reset(at: int64): void =
    # Stop if we are past the target time
    if at >= target: return

    if timer.time < at:
      timer.time = at
      timer.counter = 0

      if timer.target == 0: timer.mode.reachedTarget = true
      if timer.target == 0 and timer.mode.irqAtTarget and timer.irqsEnabled:
        return

  template count(startIn, finishIn: int64): void =
    # Clamp the start and finish time
    let start = startIn.max(timer.time+1)
    let finish = finishIn.min(target)
    var stopHere = finish >= target

    # Only run if the time interval is non-empty after clamping
    if start <= finish:
      template incsUntil(t: int64): int64 =
        # How many increments from syncInput.start until a given clock?
        divRoundUp(t - timer.syncInput.start + 1, rate)
      template clocksUntil(incs: int64): int64 =
        # At what clock after syncInput.start is a given number of increments reached?
        let result = rate * (incs-1)
        assert incsUntil(timer.syncInput.start + result) == incs
        result

      # How much should the timer be incremented by?
      var incs = incsUntil(finish) - incsUntil(start-1)

      # Did the timer just whizz through an IRQ?
      var irqIncs = timer.incsToIRQ

      if incs > 0 and irqIncs > 0 and timer.irqTriggeredNow:
        # Reschedule to recompute incsToIRQ (which always returns int64.high
        # when irqTriggeredNow is true)
        irqIncs = 1
        timer.irqTriggeredNow = false

      if irqIncs <= incs:
        stopHere = true
        let newTime = timer.syncInput.start + clocksUntil(irqIncs + incsUntil(timer.time))

        assert newTime >= timer.time and newTime <= finish
        timer.time = newTime
      else:
        timer.time = finish

      var wrap: int64 =
        if timer.mode.resetAfterTarget: timer.target.int64 + 1
        else: 0x10000

      # Special case: already passed target
      if timer.mode.resetAfterTarget and timer.counter > timer.target:
        if timer.counter.int64 + incs >= 0x10000:
          incs -= (0x10000 - timer.counter.int64)
          timer.counter = 0
          timer.mode.reachedMax = true
        else:
          wrap = 0x10000

      if timer.counter.int64 + incs >= 0x10000:
        timer.mode.reachedMax = true
      if timer.counter < timer.target and timer.counter.int64 + incs >= timer.target.int64:
        timer.mode.reachedTarget = true

      timer.counter = euclMod(timer.counter.int64 + incs, wrap).uint16

    if stopHere: return

  while timer.time < target:
    let syncStart = timer.syncInput.start
    let syncMid = syncStart + timer.syncInput.on
    let syncEnd = syncStart + timer.syncInput.length
    if timer.mode.sync:
      if timer.id == 2:
        case timer.mode.syncMode
        of 0, 3:
          # Pause forever
          pause syncEnd-1
        of 1, 2:
          # Free run
          count syncStart, syncEnd-1
      else:
        case timer.mode.syncMode
        of 0:
          # Pause during gate, then count
          pause syncMid-1
          count syncMid, syncEnd-1
        of 1:
          # Reset at gate, then count
          reset syncStart
          count syncStart+1, syncEnd-1
        of 2:
          # Reset at gate, pause outside gate
          reset syncStart
          count syncStart+1, syncMid-1
          pause syncEnd-1
        of 3:
          # Pause until gate then free run
          if timer.time == syncStart-1:
            timer.mode.sync = false
            count syncStart, syncEnd-1
          else:
            pause syncEnd-1
    else:
      # Free run
      count syncStart, syncEnd-1

    timer.syncInput.start += timer.syncInput.length

proc nextTime(timerIn: Timer, limit: int64 = nextVBlankDelta() - 1): int64 =
  ## When does the timer next need to be updated?

  var timer = timerIn # make a copy - we don't want to change the global timer

  # Stop at the end of the current frame (by default)
  timer.advance(events.now() + limit)
  timer.time

proc setIRQ(timer: var Timer, irq: bool) =
  ## Set the IRQ field for the given timer.

  timer.mode.irq = irq
  irqs.set(timer.id + 4, irq)

for timer in timers.mitems:
  timer.setIRQ(true)

proc triggerIRQ(timer: var Timer) =
  ## Trigger an IRQ for the given timer.

  debug fmt"Trigger IRQ timer {timer.id}, mode {timer.mode.word:x}, counter {timer.counter:x}, target {timer.target:x}"

  timer.irqTriggered = true
  timer.irqTriggeredNow = true

  if timer.mode.toggleIRQ:
    setIRQ(timer, false)
    setIRQ(timer, true)
  else:
    setIRQ(timer, not timer.mode.irq)

proc catchUp(timer: var Timer) =
  ## Update the timer state to what it should be now.
  ## Trigger any IRQs and similar.

  trace fmt"catchUp t={events.now()} {timer} rate={timer.clockSource.clockRate()}"

  timer.advance(events.now())
  assert timer.time == events.now(), fmt"t={events.now()} {timer} rate={timer.clockSource.clockRate()}"
  if timer.irqsEnabled:
    if timer.mode.irqAtTarget and timer.counter == timer.target:
      triggerIRQ(timer)
    elif timer.mode.irqAtMax and timer.counter == 0xffff:
      triggerIRQ(timer)

  trace fmt"end catchUp t={events.now()} {timer} rate={timer.clockSource.clockRate()}"

proc schedule(timer: var Timer) =
  ## Set an alarm for the timer to wake up when next needed.

  catchUp(timer)

  var next: int64
  if nextVBlankDelta() == 1:
    # Are we at the end of the current frame?
    # If so, reload timing information and advance to the next frame
    timer.syncInput = timer.currentSyncInput()
    next = nextTime(timer, clocksPerFrame())
  else:
    next = nextTime(timer)

  trace fmt"scheduling timer {timer.id} at t={next}"
  assert next != events.now()
  timer.generation += 1
  let generation = timer.generation
  let id = timer.id
  events.at(next, "timer update") do():
    if generation == timers[id].generation:
      schedule(timers[id])

for timer in timers.mitems:
  timer.syncInput = timer.currentSyncInput()
  schedule(timer)

proc setMode*(timer: var Timer, mode: word) =
  catchUp(timer)
  word(timer.mode) = mode and 0x1fff
  timer.mode.irq = true
  timer.irqTriggered = false
  timer.counter = 0
  debug fmt"Timer {timer.id} mode changed to {timer.mode.word:x}"
  schedule(timer)

proc mode*(timer: var Timer): word =
  catchUp(timer)
  result = timer.mode.word
  timer.mode.reachedTarget = false
  timer.mode.reachedMax = false
  setIRQ(timer, true)
  schedule(timer)
  debug fmt"Timer {timer.id} mode read as {timer.mode.word:x}"

proc setCounter*(timer: var Timer, counter: word) =
  catchUp(timer)
  timer.counter = uint16(counter and 0xffff)
  schedule(timer)
  debug fmt"Timer {timer.id} counter set to {counter:x}"

proc counter*(timer: var Timer): word =
  catchUp(timer)
  debug fmt"Timer {timer.id} counter read as {timer.counter:x}"
  result = timer.counter.word

proc setTarget*(timer: var Timer, target: word) =
  catchUp(timer)
  timer.target = uint16(target and 0xffff)
  schedule(timer)
  debug fmt"Timer {timer.id} target set to {timer.target:x}"

proc target*(timer: var Timer): word =
  catchUp(timer)
  debug fmt"Timer {timer.id} target read as {timer.target:x}"
  timer.target.word
