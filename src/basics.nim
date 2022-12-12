## PSX-specific stuff shared throughout the program.

import savestates

type
  word* = uint32
  iword* = int32

  AccessKind* {.pure.} = enum
    ## Which kind of access a memory reference is.
    Fetch, Load, Store

  MachineErrorKind* {.pure.} = enum
    ## Classes of CPU exceptions.
    Interrupt,
    AddressError,
    BusError,
    SystemCall,
    Breakpoint,
    ReservedInstruction,
    CoprocessorUnusable,
    ArithmeticOverflow,

  MachineError* = ref object of CatchableError
    ## CPU exceptions.
    case error*: MachineErrorKind
    of CoprocessorUnusable:
      cop*: 0..3
    of AddressError, BusError:
      kind*: AccessKind
      # The PSX doesn't report the address for a bus error,
      # but it doesn't hurt to remember it here
      address*: word
    of ReservedInstruction:
      instruction*: word
    else: discard

func isError*(error: MachineErrorKind): bool =
  ## Is a CPU exception an error condition (as opposed to e.g. a syscall)?

  error notin {Interrupt, SystemCall}

# Timing information.

type
  Region* {.pure.} = enum
    ## These numbers must match GPUSTAT.20.
    NTSC = 0,
    PAL = 1

const
  # This many timesteps occur per second.
  clockRate*: int64 = 44100 * 0x300 * 11 # ~372MHz
  # How many timesteps it takes for one clock cycle of each component.
  cpuClock* = 11 # ~33.8MHz
  gpuClock* = 7  # ~53.2MHz
  refreshRate*: array[Region, int] =
    [NTSC: 60, PAL: 50]

  # Width and height of the VRAM, in pixels.
  vramWidth* = 1024
  vramHeight* = 512

var
  region* {.saved.}: Region = PAL ## The game's region. Updated by the BIOS.

type
  MemoryRegion* {.pure.} = enum
    ## Which device a memory region belongs to.
    Scratchpad, RAM, BuiltIn, GPU, MDEC, Serial,
    BIOS, SPU, CDROM, Expansion1, Expansion2, Expansion3

var
  ## Number of clock cycles' delay for reads to each region.
  memoryDelay8* {.saved.}: array[MemoryRegion, int] =
    [Scratchpad: 0, RAM: cpuClock*5, BuiltIn: 0, GPU: cpuClock*2,
     MDEC: cpuClock*2, Serial: cpuClock*2,
     # These ones are set by the BIOS.
     BIOS: 0, SPU: 0, CDROM: 0, Expansion1: 0, Expansion2: 0, Expansion3: 0]
  memoryDelay16* {.saved.}: array[MemoryRegion, int] = memoryDelay8
  memoryDelay32* {.saved.}: array[MemoryRegion, int] = memoryDelay8

var
  ## Is the machine currently paused?
  paused* = false
