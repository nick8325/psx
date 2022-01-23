## PSX-specific stuff shared throughout the program.

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

# Timing information.

type
  Region* {.pure.} = enum
    ## These numbers match GPUSTAT.20.
    NTSC = 0,
    PAL = 1

const
  # This many timesteps occur per second.
  clockRate*: uint64 = 44100 * 0x300 * 11 # ~372MHz
  # How many timesteps it takes for one clock cycle of each component.
  cpuClock* = 11 # ~33.8MHz
  gpuClock* = 7  # ~53.2MHz
  systemClock* = cpuClock * 8 # ~4.23MHz
  refreshRate*: array[Region, int] =
    [NTSC: 60, PAL: 50]

  # Width and height of the VRAM, in pixels.
  vramWidth* = 1024
  vramHeight* = 512

var
  region*: Region = NTSC ## The game's region. Updated by the BIOS.
