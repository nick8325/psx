import nimterop/cimport

cPlugin:
  proc onSymbol*(sym: var Symbol) {.exportc, dynlib.} =
    # Some type names start with underscores
    if sym.name[0] == '_': sym.name = sym.name[1..^1]

    # Skip procedures - they're meant for us to define
    if sym.kind == nskProc: sym.name = ""

cOverride:
  # assignment to defs doesn't work otherwise
  type jg_inputinfo_t* {.bycopy, importc: "struct _jg_inputinfo_t".} = object
    typ* {.importc: "type".}: jg_inputtype
    index*: int
    name*: cstring
    fname*: cstring
    defs*: pointer # should be ptr cstring but in C this is const char **
                   # and the compiler complains about dropping of const
    numaxes*: int
    numbuttons*: int

cImport("/usr/include/jg/jg.h")
