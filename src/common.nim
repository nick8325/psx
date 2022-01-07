## PSX-specific stuff shared throughout the program.

type
  word* = uint32
  iword* = int32

  ## Errors that turn into CPU exceptions.
  MachineError* = object of CatchableError
  MemoryError* = object of MachineError
  InvalidAddressError* = object of MemoryError
  UnalignedAccessError* = object of MemoryError

  DecodingError* = object of MachineError
  UnknownInstructionError* = object of DecodingError

  ExecutionError* = object of CatchableError
  InvalidCOPError* = object of ExecutionError
  ArithmeticOverflowError* = object of ExecutionError
  MemoryProtectionError* = object of ExecutionError
