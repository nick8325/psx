## Save states (not yet written to file).

import std/[tables, strformat, macros, algorithm, strutils, sugar]
import variant

type
  State* = Table[string, Variant]
  Handler = object
    ## A single field to be saved restored.
    ## It has a name, and methods to get/save into a Variant.
    name: string
    save: () -> Variant
    load: (obj: Variant) -> void

var
  handlers: seq[Handler]

proc saved(getSaveName: proc(node: NimNode): string, node: NimNode): NimNode =
  ## Register a variable to be saved and restored.

  node.expectKind nnkVarSection
  node.expectLen 1
  let def = node[0]
  def.expectKind nnkIdentDefs

  var varName = def[0]
  if varName.kind == nnkPragmaExpr: varName = varName[0]
  let saveName = getSaveName(varName)
  quote do:
    handlers.add Handler(
      name: `saveName`,
      save: () => newVariant(`varName`),
      load: (val: Variant) => (`varName` = get(val, `varName`.typeOf)))

    `node`

proc qualifiedName(node: NimNode): string =
  ## Return a fully-qualified name for a symbol.

  node.expectKind nnkSym
  var path: seq[string]
  var theNode = node
  while theNode != nil:
    path.add(theNode.strVal)
    if theNode.symKind == nskModule:
      break
    theNode = theNode.owner
  path.reverse()
  return path.join(".")

macro saved*(node: typed): untyped =
  ## Register a variable to be saved and restored.

  saved(qualifiedName, node)

macro saved*(name: string, node: typed): untyped =
  ## Register a variable to be saved and restored, giving it a custom name.

  let nameStr = name.strVal
  saved((_: NimNode) => nameStr, node)

proc save*: State =
  ## Save the emulator state.

  for handler in handlers:
    if handler.name in result:
      raise newException(Exception, fmt"Handler {handler.name} already defined")
    result[handler.name] = handler.save()

proc load*(state: State) =
  ## Restore the emulator state.

  for handler in handlers:
    handler.load(state[handler.name])
