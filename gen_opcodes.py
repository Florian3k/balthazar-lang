#!/usr/bin/python

from os import path

assert(__name__ == "__main__")

base_dir = path.dirname(path.realpath(__file__))
scala_path = path.join(base_dir, "baltc", "src", "main", "scala", "Opcodes.scala")
rust_path = path.join(base_dir, "balt-vm", "src", "bytecode.rs")

assert(path.isfile(scala_path))
assert(path.isfile(rust_path))

opcodes: list[str] = [
  "OpRet",
  "OpConst",
  "OpNull",

  # int64 instructions
  "OpAddI64",
  "OpSubI64",
  "OpMulI64",
  "OpDivI64",

  "OpLeqI64",
  "OpLtI64",
  "OpGeqI64",
  "OpGtI64",

  "OpNegI64",

  # float64 instructions
  "OpAddF64",
  "OpSubF64",
  "OpMulF64",
  "OpDivF64",

  "OpLeqF64",
  "OpLtF64",
  "OpGeqF64",
  "OpGtF64",

  # jumps
  "OpJump",
  "OpJumpTrue",
  "OpJumpFalse",

  # other
  "OpPrint",

  "OpConcat",
  "OpPrintStr",

  "OpEq",
]

scala_lines: list[str] = [
  "/*",
  " * This file was automatically generated by `gen_opcodes.py`",
  " * DO NOT EDIT MANUALLY",
  " */",
  "",
  "package opcode",
  "",
  "transparent trait Range(lower: Int, value: Int, upper: Int):",
  "  assert(lower <= value && value <= upper, \"Operand range overflow!\")",
  "",
  "enum Operand:",
  "  case U8(var value: Int) extends Operand, Range(0, value, 255)",
  "  case U16(var value: Int) extends Operand, Range(0, value, (1 << 16) - 1)",
  "  case S16(var value: Int) extends Operand, Range(Short.MinValue, value, Short.MaxValue)",
  "",
  "enum Opcode:",
  *[f"  case {name}" for name in opcodes],
  "",
]

rust_lines: list[str] = [
  "/*",
  " * This file was automatically generated by `gen_opcodes.py`",
  " * DO NOT EDIT MANUALLY",
  " */",
  "",
  "use serde::{Deserialize, Serialize};",
  "use std::error::Error;",
  "",
  "#[repr(u8)]",
  "#[derive(Debug, Eq, PartialEq, Serialize, Deserialize, Copy, Clone)]",
  "pub enum Op {",
  *[f"    {name} = {i}," for (i, name) in enumerate(opcodes)],
  "}",
  "use Op::*;",
  "",
  "impl TryFrom<u8> for Op {",
  "    type Error = Box<dyn Error>;",
  "",
  "    fn try_from(value: u8) -> Result<Self, Self::Error> {",
  "        Ok(match value {",
  *[f"            {i} => {name}," for (i, name) in enumerate(opcodes)],
  "            x => return Err(format!(\"Unknown opcode: {}\", x).into()),",
  "        })",
  "    }",
  "}",
  "",
]


with open(rust_path, "w") as rust_file:
  rust_file.write("\n".join(rust_lines))

with open(scala_path, "w") as scala_file:
  scala_file.write("\n".join(scala_lines))
