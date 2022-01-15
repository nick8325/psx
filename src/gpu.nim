## The GPU.

import machine, utils, irq
import std/[bitops, strformat, logging]

var logger = newLogger("GPU", lvlDebug)

proc gp0*(value: word) =
  logger.debug fmt"GP0 {value:08x}"

proc gp1*(value: word) =
  logger.debug fmt"GP1 {value:08x}"

proc gpuread*: word =
  logger.debug fmt"GPUREAD"

proc gpustat*: word =
  logger.debug fmt"GPUSTAT"
  0x1c00_0000u32

proc gpuWriteDMA*(value: word) =
  logger.debug fmt"GPU DMA write {value:08x}"

proc gpuReadDMA*: word =
  logger.debug "GPU DMA read"
