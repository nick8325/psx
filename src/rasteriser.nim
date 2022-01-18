## The backend of the GPU - converts drawing commands into a framebuffer.

import std/[options, strformat]

type
  TransparencyMode* {.pure.} = enum
    ## 0-4 must match what the GPU uses.
    Mean = 0,
    Add = 1,
    Subtract = 2,
    AddQuarter = 3,
    Opaque = 4

  TextureColourDepth* {.pure.} = enum
    ## Must match what the GPU uses.
    FourBit = 0,
    EightBit = 1,
    FifteenBit = 2

  Colour* = tuple
    red: uint8
    green: uint8
    blue: uint8

  TextureColourMode* = object
    case depth*: TextureColourDepth
    of FourBit, EightBit: palette*: tuple[x: int, y: int]
    of FifteenBit: discard

  Texture*[N: static int] = object
    blended*: bool
    coords*: array[N, tuple[x: int, y: int]]
    colourMode*: TextureColourMode

  Polygon*[N: static int] = object
    vertices*: array[N, tuple[x: int, y: int, colour: Colour]]
    transparency*: TransparencyMode
    dither*: bool
    texture*: Option[Texture[N]]

  Rectangle* = object
    position*: tuple[x: int, y: int]
    size*: tuple[x: int, y: int]
    colour*: Colour
    texture*: Option[Texture[1]]
    flipX*, flipY*: bool

  Line* = object
    start*: tuple[x: int, y: int, colour: Colour]
    stop*: tuple[x: int, y: int, colour: Colour]
    dither*: bool

func `$`*[N: static int](tex: Texture[N]): string =
  result = if tex.blended: "blended" else: "raw"
  result &= fmt", colour mode=({tex.colourMode.depth})"
  if tex.colourMode.depth != FifteenBit:
    result &= fmt", palette=({tex.colourMode.palette.x}, {tex.colourMode.palette.y})"
  for i, coord in tex.coords:
    if i > 0: result &= "--"
    result &= fmt"({coord.x},{coord.y})"

func `$`*[N: static int](poly: Polygon[N]): string =
  result =
    case N
    of 3: "triangle, "
    of 4: "quadrilateral, "
    else: fmt"{N}-sided polygon, "
  for i, v in poly.vertices:
    if i > 0: result &= "--"
    result &= fmt"({v.x},{v.y})"
  result &= fmt", colours "
  for i, v in poly.vertices:
    if i > 0: result &= "--"
    result &= fmt"#{v.colour.red:02x}{v.colour.green:02x}{v.colour.blue:02x}"
  result &= fmt", {poly.transparency}"
  if poly.dither:
    result &= ", dithered"
  if poly.texture.isSome: result &= fmt", texture {poly.texture.get()}"

proc draw*(x: Polygon[3]) =
  echo fmt"draw {x}"

proc draw*(poly: Polygon[4]) =
  # Split a rectangle into two triangles

  let v = poly.vertices
  var poly1 = Polygon[3](vertices: [v[0], v[1], v[2]],
                         transparency: poly.transparency,
                         dither: poly.dither)
  var poly2 = Polygon[3](vertices: [v[1], v[2], v[3]],
                         transparency: poly.transparency,
                         dither: poly.dither)
  if poly.texture.isSome:
    let t = poly.texture.get()
    let c = t.coords
    poly1.texture = some(
      Texture[3](blended: t.blended, colourMode: t.colourMode,
                 coords: [c[0], c[1], c[2]]))
    poly2.texture = some(
      Texture[3](blended: t.blended, colourMode: t.colourMode,
                 coords: [c[1], c[2], c[3]]))
  poly1.draw
  poly2.draw
