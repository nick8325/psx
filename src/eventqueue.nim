## A queue of events to be run according to a schedule.

import std/heapqueue

type
  Event = tuple[time: uint64, repeat: uint64, action: proc ()]
  EventQueue* = object
    ## A queue of events.
    now: uint64
    minDelta: uint64
    heap: HeapQueue[Event]

func `<`(x, y: Event): bool =
  x.time < y.time

func initEventQueue*: EventQueue =
  ## Return an empty event queue.

  EventQueue(now: 0, minDelta: uint64.high, heap: initHeapQueue[Event]())

proc updateMinDelta(queue: var EventQueue) =
  if queue.heap.len > 0:
    queue.minDelta = queue.heap[0].time - queue.now
  else:
    queue.minDelta = uint64.high

proc after*(queue: var EventQueue, delta: uint64, action: proc ()) =
  ## Execute an event 'delta' timesteps in the future.

  queue.heap.push((time: queue.now + delta, repeat: uint64.high, action: action))
  queue.updateMinDelta

proc every*(queue: var EventQueue, delta: uint64, action: proc ()) =
  ## Execute an event every 'delta' timesteps from now on.

  queue.heap.push((time: queue.now + delta, repeat: delta, action: action))
  queue.updateMinDelta

func time*(queue: EventQueue): uint64 {.inline.} =
  ## How many timesteps have passed until now?

  queue.now

func nextTime*(queue: EventQueue): uint64 {.inline.} =
  ## How many timesteps in the future does the next event happen?

  queue.minDelta

proc fastForward*(queue: var EventQueue, delta: uint64) {.inline.} =
  ## Skip forward 'delta' timesteps.
  ## It is not allowed to skip past the time when the next event should happen.

  assert delta <= queue.minDelta
  queue.now += delta
  queue.minDelta -= delta

proc runNext*(queue: var EventQueue): bool =
  ## Skip forward to the next event and run it.

  if queue.heap.len > 0:
    let event = queue.heap.pop()
    event.action()
    if event.repeat != uint64.high:
      queue.every(event.repeat, event.action)
    queue.updateMinDelta
    return true
  else:
    return false
