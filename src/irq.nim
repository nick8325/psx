## The IRQ chip.

import machine
import std/bitops

type
  IRQs* = tuple
    stat, mask: word

var
  irqs*: IRQs

proc setCPUIRQ(irqs: var IRQs) =
  ## Update the processor IRQ flags from irq.stat.

  machine.cpu.setIRQ((irqs.stat and irqs.mask) != 0)

proc signal*(irqs: var IRQs, irq: range[0..10]) =
  ## Activate a given IRQ.

  irqs.stat.setBit int(irq)
  irqs.setCPUIRQ()

proc clear*(irq: range[0..10]) =
  ## Clear a given IRQ.

  irqs.stat.clearBit int(irq)
  irqs.setCPUIRQ()

proc handleStatus*(irqs: var IRQs, value: var word, kind: IOKind) =
  case kind
  of Read: value = irqs.stat
  of Write: irqs.stat = irqs.stat and value
proc handleMask*(irqs: var IRQs, value: var word, kind: IOKind) =
  case kind
  of Read: value = irqs.mask
  of Write: irqs.mask = value and 0x7ff

# VBLANK IRQ
events.every(clockRate div refreshRate) do: irqs.signal 0
