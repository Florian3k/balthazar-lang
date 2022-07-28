/*
 * This file was automatically generated by `gen_opcodes.py`
 * DO NOT EDIT MANUALLY
 */

package opcode

transparent trait Range(lower: Int, value: Int, upper: Int):
  assert(lower <= value && value <= upper, "Operand range overflow!")

enum Operand:
  case U8(var value: Int) extends Operand, Range(0, value, 255)
  case U16(var value: Int) extends Operand, Range(0, value, (1 << 16) - 1)
  case S16(var value: Int) extends Operand, Range(Short.MinValue, value, Short.MaxValue)

enum Opcode:
  case OpRet
  case OpConst
  case OpNull
  case OpAddI64
  case OpSubI64
  case OpMulI64
  case OpDivI64
  case OpLeqI64
  case OpLtI64
  case OpGeqI64
  case OpGtI64
  case OpNegI64
  case OpAddF64
  case OpSubF64
  case OpMulF64
  case OpDivF64
  case OpLeqF64
  case OpLtF64
  case OpGeqF64
  case OpGtF64
  case OpJump
  case OpJumpTrue
  case OpJumpFalse
  case OpPrint
  case OpConcat
  case OpPrintStr
  case OpEq
