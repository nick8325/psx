## A queue of events to be run according to a schedule.

import std/[heapqueue, strformat]
import utils, savestates

const loggerComponent = logEventQueue

type
  Event = object
    name: string
    time: int64
    id: int64
    repeat: proc(): int64
    action: proc ()
  EventQueue = object
    ## A queue of events.
    now: int64
    id: int64
    minDelta: int64
    heap: HeapQueue[Event]

func `<`(x, y: Event): bool =
  cmp((x.time, x.id), (y.time, y.id)) < 0

func initEventQueue*: EventQueue =
  ## Return an empty event queue.

  EventQueue(now: 0, id: 0, minDelta: int64.high, heap: initHeapQueue[Event]())

func now*(queue: EventQueue): int64 {.inline.} =
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

proc at*(queue: var EventQueue, time: int64, name: string, action: proc ()) =
  ## Execute an event at a given time.
  ## The time can be in the past in which case the event happens now.

  if time < queue.now:
    warn fmt"event '{name}' scheduled in past (delta={time-queue.now})"

  if time <= 0:
    action()
  else:
    let id = queue.id
    queue.id += 1
    queue.heap.push(Event(name: name, time: time, id: id, repeat: nil, action: action))
    queue.updateMinDelta

proc after*(queue: var EventQueue, delta: int64, name: string, action: proc ()) =
  ## Execute an event 'delta' timesteps in the future.
  ## 'delta' can be negative in which case the event happens now.

  queue.at(queue.now + delta, name, action)

proc get(delta: proc(): int64, name: string): int64 {.inline.} =
  result = delta()
  if result <= 0:
    warn fmt"repeating event '{name}' scheduled in past or now (delta={result})"
    result = 1

proc every*(queue: var EventQueue, delta: proc(): int64, name: string, action: proc ()) =
  ## Execute an event every 'delta' timesteps from now on.

  let id = queue.id
  queue.id += 1
  let nowdelta = delta.get(name)
  queue.heap.push(Event(name: name, time: queue.now + nowdelta, id: id, repeat: delta, action: action))
  queue.updateMinDelta

proc runNext(queue: var EventQueue) =
  ## Run the next event regardless of its time.

  if queue.heap.len > 0:
    var event = queue.heap.pop()
    queue.updateMinDelta
    event.action()
    if event.repeat != nil:
      let delta = event.repeat.get(event.name)
      event.time += delta
      queue.heap.push event
      queue.updateMinDelta

proc fastForward*(queue: var EventQueue, delta: int64) {.inline.} =
  ## Skip forward 'delta' timesteps.
  ## Run any events that happen in the meantime.

  assert delta >= 0
  var remaining = delta

  while unlikely(queue.minDelta <= remaining):
    queue.now += queue.minDelta
    remaining -= queue.minDelta
    queue.minDelta = 0
    queue.runNext()

  queue.now += remaining
  queue.minDelta -= remaining

var
  events* {.saved.}: EventQueue = initEventQueue() ## The queue of events to happen.
