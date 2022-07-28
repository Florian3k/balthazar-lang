import ast._
import ast.Expr._
import ast.Statement._
import opcode._
import scala.collection.mutable.ArrayBuffer

class Codegen:
  type Op = Opcode | Operand
  val consts = ArrayBuffer[Long | String]()

  def bytecodeSize(code: List[Op]): Short =
    code
      .map {
        case Operand.U8(_)  => 1
        case Operand.U16(_) => 2
        case Operand.S16(_) => 2
        case _: Opcode      => 1
      }
      .sum
      .toShort

  def getOrCreateConstant(v: Long | String): Int =
    if !consts.contains(v) then consts.addOne(v)
    consts.indexOf(v)

  def codegenStmt(stmt: Statement[Typed]): List[Op] =
    stmt match
      case fd: FunctionDecl[Typed]  => codegenStmt(fd)
      case is: IfStatement[Typed]   => codegenStmt(is)
      case es: ExprStatement[Typed] => codegenStmt(es)
      case _ =>
        throw Exception(s"Codegen for ${stmt.getClass} is not supported")

  def codegenStmt(fd: FunctionDecl[Typed]): List[Op] =
    val FunctionDecl(name, params, retType, body) = fd
    if params.length > 0 then
      throw Exception("Function with params is not supported")
    if retType.isDefined then
      throw Exception("Function with return type is not supported")
    body.flatMap(codegenStmt)

  def codegenStmt(is: IfStatement[Typed]): List[Op] =
    /*
      [codeCond]
       OpJumpFalse(1)
      [codeIfTrue]
       OpJump(2)
      [codeIfFalse] (1)
                    (2)
     */
    val IfStatement(cond, ifTrue, ifFalse) = is

    val codeCond = codegenExpr(cond)
    val codeIfTrue = ifTrue.flatMap(codegenStmt)
    val codeIfFalse = ifFalse.flatMap(codegenStmt)

    val secondJump =
      List[Op](Opcode.OpJump, Operand.S16(bytecodeSize(codeIfFalse)))
    val firstJump = List[Op](
      Opcode.OpJumpFalse,
      Operand.S16(bytecodeSize(codeIfTrue ++ secondJump)),
    )

    codeCond ++ firstJump ++ codeIfTrue ++ secondJump ++ codeIfFalse

  def codegenStmt(es: ExprStatement[Typed]): List[Op] =
    codegenExpr(es.expr)
      // Printing after every ExprStatement is temporary, to be removed after implementing functions
      .appended(
        if es.expr.typ == Type.String
        then Opcode.OpPrintStr
        else Opcode.OpPrint
      )

  def codegenExpr(expr: Typed[Expr[Typed]]): List[Op] =
    expr.node match
      case NullLiteral() => List(Opcode.OpNull)
      case BoolLiteral(b) =>
        val idx = getOrCreateConstant(if b then 1 else 0)
        List(Opcode.OpConst, Operand.U16(idx))
      case Int64Literal(n) =>
        val idx = getOrCreateConstant(n)
        List(Opcode.OpConst, Operand.U16(idx))
      case Float64Literal(f) => ???
      case StringLiteral(s) =>
        val idx = getOrCreateConstant(s)
        List(Opcode.OpConst, Operand.U16(idx))
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
