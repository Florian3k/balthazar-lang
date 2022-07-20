package ast

object Statement:
  type ForInitStatement = VarDecl | ExprOrAssign
  type ExprOrAssign = ExprStatement | VarAssign | FieldAssign

enum Statement:
  case ClassDecl(
      name: String,
      fields: List[(String, Type)],
      methods: List[FunctionDecl],
  )
  case FunctionDecl(
      name: String,
      params: List[(String, Type)],
      retType: Option[Type],
      body: List[Statement],
  )
  case VarDecl(name: String, typ: Option[Type], expr: Expr)

  case IfStatement(
      cond: Expr,
      ifTrue: List[Statement],
      ifFalse: List[Statement],
  )
  case WhileStatement(cond: Expr, body: List[Statement])
  case ForStatement(
      init: Option[Statement.ForInitStatement],
      cond: Option[Expr],
      inc: Option[Statement.ExprOrAssign],
      body: List[Statement],
  )
  case BreakStatement()
  case ContinueStatement()
  case ReturnStatement(expr: Option[Expr])

  case VarAssign(name: String, expr: Expr)
  case FieldAssign(obj: Expr, field: String, expr: Expr)

  case ExprStatement(expr: Expr)

enum Type:
  case Bool
  case Int64
  case Float64
  case String
  case Typename(name: String)
  case Nullable(typ: Type)

enum Expr:
  case NullLiteral
  case BoolLiteral(value: Boolean)
  case Int64Literal(value: Long)
  case Float64Literal(value: Double)
  case StringLiteral(value: String)

  case VariableExpr(value: String)

  case UnaryExpr(op: Unop, rhs: Expr)
  case BinaryExpr(lhs: Expr, op: Binop, rhs: Expr)

  case FuncCallExpr(func: Expr, args: List[Expr])
  case MethodCallExpr(obj: Expr, method: String, args: List[Expr])
  case ObjAccessExpr(obj: Expr, field: String)

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
