#!/usr/bin/python

from os import path
from subprocess import Popen
from sys import argv

def getPath(loc: str, ext: str) -> str:
  if not path.isfile(loc):
    print(f"{loc} is not a file")
    exit(1)
  fileExt = path.splitext(loc)[1]
  if fileExt != ext:
    print(f"Expected {ext} extension, instad got: {fileExt or '<no extension>'}")
    exit(1)
  if path.isabs(loc):
    return loc
  return path.join('..', loc)

match argv[1:]:
  case ['compile', file]:
    filePath = getPath(file, '.bt')
    output = path.join('..', path.splitext(path.basename(filePath))[0] + '.bto')

    Popen(["sbt", "-warn", f'run {filePath} {output}'], cwd="./baltc").wait()
  case ['run', file]:
    filePath = getPath(file, '.bto')
    Popen(["cargo", "run", filePath], cwd="./balt-vm").wait()
  case _:
    print("Avaliable commands:")
    print("  compile <file.bt>  - compiles file.bt to file.bto")
    print("  run     <file.bto> - runs file.bto")
