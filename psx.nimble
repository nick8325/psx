# Package

version       = "1"
author        = "Nick Smallbone"
description   = "A PlayStation emulator"
license       = "MIT"
srcDir        = "src"
bin           = @["main"]


# Dependencies

requires "nim >= 2.0.0", "sdl2 >= 2.0", "variant >= 0.3.0", "glm >= 1.0.0", "nimterop >= 0.6.13", "https://github.com/nick8325/imguin"
