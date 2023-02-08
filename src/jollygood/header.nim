import nimterop/cimport

cPlugin:
  proc onSymbol*(sym: var Symbol) {.exportc, dynlib.} =
    # Some type names start with underscores
    if sym.name[0] == '_': sym.name = sym.name[1..^1]

    # Skip procedures - they're meant for us to define
    if sym.kind == nskProc: sym.name = ""

cImport("/usr/include/jg/jg.h")

# We'll assume that jg_input_t.defs is always created with allocCStringArray
proc `=destroy`(info: var jg_inputinfo_t) =
  let defs = cast[cstringArray](info.defs)
  deallocCStringArray(defs)
