# Package

version       = "0.1.0"
author        = "James Kominick"
description   = "advent of code d01"
license       = "MIT"
srcDir        = "src"
bin           = @["d01"]
binDir        = "bin"

task run, "Build and run":
  exec "nim c -r -o:bin/d01 src/d01.nim"

task test, "Build and test":
  exec "nim c -r -o:bin/test tests/test.nim"

# Dependencies

requires "nim >= 0.19.0"
