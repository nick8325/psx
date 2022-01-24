## A queue of events to be run according to a schedule.

import std/heapqueue

type
  Event = tuple[time: int64, repeat: int64, action: proc ()]
  EventQueue* {.requiresInit.} = ref object
    ## A queue of events.
    now: int64
    minDelta: int64
    heap: HeapQueue[Event]

func `<`(x, y: Event): bool =
  x.time < y.time

func initEventQueue*: EventQueue =
  ## Return an empty event queue.

  EventQueue(now: 0, minDelta: int64.high, heap: initHeapQueue[Event]())

func time*(queue: EventQueue): int64 {.inline.} =
  ## How many timesteps have passed until now?

  queue.now

func nextTime*(queue: EventQueue): int64 {.inline.} =
  ## When does the next event happen?

  queue.now + queue.minDelta

proc updateMinDelta(queue: var EventQueue) =
  ## Update the minDelta field after updating the heap.

  if queue.heap.len > 0:
    queue.minDelta = queue.heap[0].time - queue.now
  else:
    queue.minDelta = int64.high

proc at*(queue: var EventQueue, time: int64, action: proc ()) =
  ## Execute an event at a given time.
  ## The time can be in the past in which case the event happens now.

  if time < 0:
    action()
  else:
    queue.heap.push((time: time, repeat: int64.high, action: action))
    queue.updateMinDelta

proc after*(queue: var EventQueue, delta: int64, action: proc ()) =
  ## Execute an event 'delta' timesteps in the future.
  ## 'delta' can be negative in which case the event happens now.

  queue.at(queue.now + delta, action)

proc every*(queue: var EventQueue, delta: int64, action: proc ()) =
  ## Execute an event every 'delta' timesteps from now on.

  assert delta >= 0
  queue.heap.push((time: queue.now + delta, repeat: delta, action: action))
  queue.updateMinDelta

proc runNext(queue: var EventQueue) =
  ## Run the next event regardless of its time.

  if queue.heap.len > 0:
    var event = queue.heap.pop()
    queue.updateMinDelta
    event.action()
    if event.repeat != int64.high:
      event.time += event.repeat
      queue.heap.push event
      queue.updateMinDelta

proc fastForward*(queue: var EventQueue, delta: int64) {.inline.} =
  ## Skip forward 'delta' timesteps.
  ## Run any events that happen in the meantime.

  queue.now += delta
  queue.minDelta -= delta

  while queue.minDelta < 0:
    queue.runNext()

var
  events*: EventQueue = initEventQueue() ## The queue of events to happen.
