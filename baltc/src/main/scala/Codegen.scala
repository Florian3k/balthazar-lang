import ast._
import ast.Expr._
import opcode._
import scala.collection.mutable.ArrayBuffer

class Codegen:
  val consts = ArrayBuffer[Long]()

  def getOrCreateConstant(v: Long): Int =
    if !consts.contains(v) then consts.addOne(v)
    consts.indexOf(v)

  def codegenExpr(expr: Typed[Expr[Typed]]): List[Opcode | Operand] =
    expr.node match
      case NullLiteral() => List(Opcode.OpNull)
      case BoolLiteral(b) =>
        val idx = getOrCreateConstant(if b then 1 else 0)
        List(Opcode.OpConst, Operand.U16(idx))
      case Int64Literal(n) =>
        val idx = getOrCreateConstant(n)
        List(Opcode.OpConst, Operand.U16(idx))
      case Float64Literal(f)  => ???
      case StringLiteral(s)   => ???
      case VariableExpr(name) => ???
      case UnaryExpr(op, expr) =>
        codegenExpr(expr) ++ List(
          op match
            case Unop.Minus => Opcode.OpNegI64
            case Unop.Not   => ???
        )
      case BinaryExpr(lhs, op, rhs) =>
        import Binop._
        import Type._
        val tp = lhs.typ
        codegenExpr(lhs) ++ codegenExpr(rhs) ++ List(
          op match
            case Plus =>
              tp match
                case Int64   => Opcode.OpAddI64
                case Float64 => Opcode.OpAddF64
                case String  => Opcode.OpConcat
                case _       => ???
            case Minus =>
              tp match
                case Int64   => Opcode.OpSubI64
                case Float64 => Opcode.OpSubF64
                case _       => ???
            case Times =>
              tp match
                case Int64   => Opcode.OpMulI64
                case Float64 => Opcode.OpMulF64
                case _       => ???
            case Div =>
              tp match
                case Int64   => Opcode.OpDivI64
                case Float64 => Opcode.OpDivF64
                case _       => ???
            case Modulo     => ???
            case ShiftLeft  => ???
            case ShiftRight => ???
            case BitAnd     => ???
            case BitOr      => ???
            case LessEqual =>
              tp match
                case Int64   => Opcode.OpLeqI64
                case Float64 => Opcode.OpLeqF64
                case _       => ???
            case LessThan =>
              tp match
                case Int64   => Opcode.OpLtI64
                case Float64 => Opcode.OpLtF64
                case _       => ???
            case GreaterEqual =>
              tp match
                case Int64   => Opcode.OpGeqI64
                case Float64 => Opcode.OpGeqF64
                case _       => ???
            case GreaterThan =>
              tp match
                case Int64   => Opcode.OpGtI64
                case Float64 => Opcode.OpGtF64
                case _       => ???
            case Equal    => Opcode.OpEq
            case NotEqual => ???
            case BoolAnd  => ???
            case BoolOr   => ???
        )
      case _ => ???
