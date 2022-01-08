## PSX-specific stuff shared throughout the program.

type
  word* = uint32
  iword* = int32

  AccessKind* {.pure.} = enum Fetch, Load, Store ## \
    ## Which kind of access a memory reference is.

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
