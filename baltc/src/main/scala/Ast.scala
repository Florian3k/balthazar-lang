package ast

type Untyped = [X] =>> X
case class Typed[E](typ: Type, node: E)

object Statement:
  type ForInitStatement[Typ[_]] = VarDecl[Typ] | ExprOrAssign[Typ]
  type ExprOrAssign[Typ[_]] =
    ExprStatement[Typ] | VarAssign[Typ] | FieldAssign[Typ]

enum Statement[Typ[_]]:
  case ClassDecl[Typ[_]](
      name: String,
      fields: List[(String, Type)],
      methods: List[FunctionDecl[Typ]],
  ) extends Statement[Typ]
  case FunctionDecl[Typ[_]](
      name: String,
      params: List[(String, Type)],
      retType: Option[Type],
      body: List[Statement[Typ]],
  ) extends Statement[Typ]
  case VarDecl[Typ[_]](name: String, typ: Option[Type], expr: Typ[Expr[Typ]])
      extends Statement[Typ]

  case IfStatement[Typ[_]](
      cond: Typ[Expr[Typ]],
      ifTrue: List[Statement[Typ]],
      ifFalse: List[Statement[Typ]],
  ) extends Statement[Typ]
  case WhileStatement[Typ[_]](cond: Typ[Expr[Typ]], body: List[Statement[Typ]])
      extends Statement[Typ]
  case ForStatement[Typ[_]](
      init: Option[Statement.ForInitStatement[Typ]],
      cond: Option[Typ[Expr[Typ]]],
      inc: Option[Statement.ExprOrAssign[Typ]],
      body: List[Statement[Typ]],
  ) extends Statement[Typ]
  case BreakStatement[Typ[_]]() extends Statement[Typ]
  case ContinueStatement[Typ[_]]() extends Statement[Typ]
  case ReturnStatement[Typ[_]](expr: Option[Typ[Expr[Typ]]])
      extends Statement[Typ]

  case VarAssign[Typ[_]](name: String, expr: Typ[Expr[Typ]])
      extends Statement[Typ]
  case FieldAssign[Typ[_]](
      obj: Typ[Expr[Typ]],
      field: String,
      expr: Typ[Expr[Typ]],
  ) extends Statement[Typ]

  case ExprStatement[Typ[_]](expr: Typ[Expr[Typ]]) extends Statement[Typ]

enum Type:
  case Bool
  case Int64
  case Float64
  case String
  case Typename(name: String)
  case Nullable(typ: Type)
  case Null

enum Expr[Typ[_]]:
  case NullLiteral[Typ[_]]() extends Expr[Typ]
  case BoolLiteral[Typ[_]](value: Boolean) extends Expr[Typ]
  case Int64Literal[Typ[_]](value: Long) extends Expr[Typ]
  case Float64Literal[Typ[_]](value: Double) extends Expr[Typ]
  case StringLiteral[Typ[_]](value: String) extends Expr[Typ]

  case VariableExpr[Typ[_]](value: String) extends Expr[Typ]

  case UnaryExpr[Typ[_]](op: Unop, rhs: Typ[Expr[Typ]]) extends Expr[Typ]
  case BinaryExpr[Typ[_]](lhs: Typ[Expr[Typ]], op: Binop, rhs: Typ[Expr[Typ]])
      extends Expr[Typ]

  case FuncCallExpr[Typ[_]](func: Typ[Expr[Typ]], args: List[Typ[Expr[Typ]]])
      extends Expr[Typ]
  case ObjAccessExpr[Typ[_]](obj: Typ[Expr[Typ]], field: String)
      extends Expr[Typ]

enum Binop:
  case Plus
  case Minus
  case Times
  case Div
  case Modulo
  case ShiftLeft
  case ShiftRight
  case BitAnd
  case BitOr
  case LessEqual
  case LessThan
  case GreaterEqual
  case GreaterThan
  case Equal
  case NotEqual
  case BoolAnd
  case BoolOr

enum Unop:
  case Minus
  case Not
