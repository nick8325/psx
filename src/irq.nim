## The IRQ chip.

import basics, memory, cpu, utils
import std/[bitops, setutils, strformat]

const loggerComponent = logIRQ

type
  IRQs* = tuple
    stat, mask: word
    # IRQ input pins, used to implement level-triggering
    source: set[0..10]

var
  irqs*: IRQs

proc setCPUIRQ(irqs: var IRQs) =
  ## Update the processor IRQ flags from irq.stat.

  cpu.cpu.setIRQ((irqs.stat and irqs.mask) != 0)

proc set*(irqs: var IRQs, irq: range[0..10], val: bool) =
  ## Change the value of an input IRQ pin.

  # IRQs are edge-triggered
  if val and not (irq in irqs.source):
    debug fmt"Trigger IRQ {irq}, {cpu.cpu}"
    irqs.stat.setBit int(irq)

  irqs.source[irq] = val
  irqs.setCPUIRQ()

proc toggle*(irqs: var IRQs, irq: range[0..10]) =
  ## Toggle the value of an input IRQ pin.

  irqs.set(irq, not (irq in irqs.source))

proc signal*(irqs: var IRQs, irq: range[0..10]) =
  ## Activate an input IRQ pin for an instant.

  debug fmt"Signal IRQ {irq}, {cpu.cpu}"
  irqs.stat.setBit int(irq)
  irqs.setCPUIRQ()

proc handleStatus*(irqs: var IRQs, value: var word, kind: IOKind) =
  case kind
  of Read: value = irqs.stat
  of Write: irqs.stat = irqs.stat and value
  irqs.setCPUIRQ

  trace fmt"IRQ status {irqs.stat:x}"

proc handleMask*(irqs: var IRQs, value: var word, kind: IOKind) =
  case kind
  of Read: value = irqs.mask
  of Write: irqs.mask = value and 0x7ff
  irqs.setCPUIRQ

  trace fmt"IRQ mask {irqs.mask:x}"
