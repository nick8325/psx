## Timers.

import basics, utils, irq, eventqueue, gpu
import std/[bitops, strformat, options]

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

for i in TimerId.low..TimerId.high:
  timers[i].id = i
  timers[i].mode.irq = true
timers[1].mode.clockSourceBits = 1

type
  ClockSource {.pure.} = enum
    SystemClock, SystemClockDiv8, DotClock, HBlank

proc clockSource(timer: Timer): ClockSource =
  ## What is the clock source of a timer?

  if timer.mode.clockSourceBits.testBit(0):
    case timer.id
    of 0: DotClock
    of 1: HBlank
    of 2: SystemClockDiv8
  else: SystemClock

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

proc updateSyncInputs =
  for i in TimerId.low..TimerId.high:
    timers[i].syncInput = timers[i].currentSyncInput()
updateSyncInputs()
onVBlank("update timer sync inputs", updateSyncInputs)

func irqsEnabled(timer: Timer): bool =
  ## Are IRQs enabled? (Only one-shot mode disables them.)

  (timer.mode.repeat or not timer.irqTriggered) and not timer.irqTriggeredNow

func ticksToIRQ(timer: Timer): int64 =
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
    trace (fmt"t={timer.time}: pause until " & $finish)

    # Only pause if the time is not in the past
    if timer.time <= finish: timer.time = min(finish, target)
    # Stop if we are past the target time
    if finish > target: return

  template reset(at: int64): void =
    trace (fmt"t={timer.time}: reset at " & $at)

    if timer.time == at:
      timer.counter = 0

      # Move forward one clock cycle, to prevent "count" from ticking the
      # timer until the next cycle. But stop if we reached the target time.
      if at == target: return
      else: timer.time += 1

      if timer.target == 0 and timer.mode.irqAtTarget and timer.irqsEnabled:
        return

  template count(start_in, finish_in: int64): void =
    trace (fmt"t={timer.time}: count from " & $start_in & " to " & $finish_in)

    var start = start_in
    var finish = finish_in

    # Only run if the finish time is not in the past
    if timer.time <= finish:
      var stopHere = finish > target

      # Clamp the start and finish time
      start = max(start, timer.time)
      finish = min(finish, target)

      # How much should the timer be incremented by?
      var incs =
        divRoundUp(finish+1-timer.syncInput.start, rate) -
        divRoundUp(start-timer.syncInput.start, rate)

      if incs > 0: timer.irqTriggeredNow = false

      # Did the timer just whizz through an IRQ?
      let irqTicks = timer.ticksToIRQ

      if irqTicks <= incs:
        stopHere = true
        timer.counter += irqTicks.uint16
        # TODO: make this calculation clock cycle-accurate
        timer.time += irqTicks * rate
      else:
        var wrap: int64 =
          if timer.mode.resetAfterTarget: timer.target.int64 + 1
          else: 0x10000

        # Special case: already passed target
        if timer.mode.resetAfterTarget and timer.counter > timer.target:
          if timer.counter.int64 + incs >= 0x10000:
            incs -= (0x10000 - timer.counter.int64)
            timer.counter = 0
          else:
            wrap = 0x10000

        timer.counter = uint16((timer.counter.int64 + incs) mod wrap)
        timer.time = finish

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
          count syncStart, syncEnd-1
        of 2:
          # Reset at gate, pause outside gate
          reset syncStart
          count syncStart, syncMid-1
          pause syncEnd-1
        of 3:
          # Pause until gate then free run
          if timer.time == syncStart:
            timer.mode.sync = false
            count syncStart, syncEnd-1
          else:
            pause syncEnd-1
    else:
      # Free run
      count syncStart, syncEnd-1

    timer.syncInput.start += timer.syncInput.length

proc nextTime(id: TimerId): int64 =
  ## When does the timer next need to be updated?

  var timer = timers[id] # make a copy
  timer.advance(events.now() + nextVBlankDelta())
  timer.time

proc setIRQ(id: TimerID, irq: bool) =
  ## Set the IRQ field for the given timer.

  timers[id].mode.irq = irq
  irqs.set(id + 4, irq)

for id in TimerId.low..TimerId.high:
  id.setIRQ(true)

proc triggerIRQ(id: TimerId) =
  ## Trigger an IRQ for the given timer.

  debug fmt"Trigger IRQ timer {id}"

  timers[id].irqTriggered = true
  timers[id].irqTriggeredNow = true

  if timers[id].mode.toggleIRQ:
    setIRQ(id, false)
    setIRQ(id, true)
  else:
    setIRQ(id, not timers[id].mode.irq)

proc catchUp(id: TimerId) =
  ## Update the timer state to what it should be now.
  ## Trigger any IRQs and similar.

  timers[id].advance(events.now())
  trace fmt"caught up timer {id} to {timers[id].time} at {events.now()}"
  assert timers[id].time == events.now()
  if timers[id].irqsEnabled:
    if timers[id].mode.irqAtTarget and timers[id].counter == timers[id].target:
      triggerIRQ(id)
    elif timers[id].mode.irqAtMax and timers[id].counter == 0xffff:
      triggerIRQ(id)

proc schedule(id: TimerId) =
  ## Set an alarm for the timer to wake up when next needed.

  catchUp(id)
  let next = nextTime(id)
  assert next != events.now()
  if next != int64.high:
    timers[id].generation += 1
    let generation = timers[id].generation
    events.at(next, "timer update") do():
      if generation == timers[id].generation:
        schedule(id)

proc setMode*(id: TimerId, mode: word) =
  catchUp(id)
  word(timers[id].mode) = mode and 0x1fff
  timers[id].mode.irq = true
  timers[id].irqTriggered = false
  timers[id].counter = 0
  schedule(id)

proc mode*(id: TimerId): word =
  catchUp(id)
  result = timers[id].mode.word
  timers[id].mode.reachedTarget = false
  timers[id].mode.reachedMax = false
  setIRQ(id, true)

  debug fmt"Timer {id} counter is {result}"

proc setCounter*(id: TimerId, counter: word) =
  catchUp(id)
  timers[id].counter = uint16(counter and 0xffff)
  schedule(id)

proc counter*(id: TimerId): word =
  catchUp(id)
  result = timers[id].counter.word
  debug fmt"Timer {id} counter is {result}"

proc setTarget*(id: TimerId, target: word) =
  catchUp(id)
  timers[id].target = uint16(target and 0xffff)
  schedule(id)

proc target*(id: TimerId): word =
  catchUp(id)
  timers[id].target.word
