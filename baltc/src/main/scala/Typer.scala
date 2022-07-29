package typer

import ast._
import Statement._
import Expr._

// scalafmt: { maxColumn = 100 }
private def prettyName(stmt: Statement[?]) =
  stmt match
    case ClassDecl(_, _, _)       => "class declaration"
    case FunctionDecl(_, _, _, _) => "function declaration"
    case VarDecl(_, _, _)         => "variable declaration"
    case IfStatement(_, _, _)     => "if statement"
    case WhileStatement(_, _)     => "while statement"
    case ForStatement(_, _, _, _) => "for statement"
    case BreakStatement()         => "break statement"
    case ContinueStatement()      => "continue statement"
    case ReturnStatement(_)       => "return statement"
    case VarAssign(_, _)          => "variable assignment"
    case FieldAssign(_, _, _)     => "field assignment"
    case ExprStatement(_)         => "expression"

private def isSubtypeOf(sub: Type, sup: Type): Boolean =
  import Type._
  (sub, sup) match
    case _ if sub == sup              => true
    case (Nullable(t1), Nullable(t2)) => isSubtypeOf(t1, t2)
    case (Null, Nullable(_))          => true
    case _                            => false

object Typer:
  case class Context(
      val classes: List[ClassDecl[?]],
      val functions: List[FunctionDecl[?]],
      val vars: List[(String, Type)],
      val fnRetType: Option[Type],
      val isLoop: Boolean,
      val notNull: Set[String],
  ):
    def withNotNull(names: Set[String]) = copy(notNull = notNull | names)
    def withoutNotNull(name: String) = copy(notNull = notNull - name)
    def withVar(name: String, typ: Type) = copy(vars = (name, typ) :: vars)
    def withRetType(rt: Option[Type]) = copy(fnRetType = rt)
    def withLoop = copy(isLoop = true)
  object Context:
    def empty: Context = Context(List(), List(), List(), None, false, Set())
  def ctx(using ctx: Context): Context = ctx

  def typecheckProgram(
      program: List[Statement[Untyped]]
  ): List[Statement[Typed]] =
    for stmt <- program do
      stmt match
        case ClassDecl(_, _, _)       => ()
        case FunctionDecl(_, _, _, _) => ()
        case _ =>
          throw Exception(
            s"${prettyName(stmt).capitalize} is not allowed in global scope"
          )

    val classes = program.collect { case cd @ ClassDecl(_, _, _) => cd }
    val functions = program.collect { case fd @ FunctionDecl(_, _, _, _) => fd }

    given Context = Context(classes, functions, List(), None, false, Set())

    classes.map(typecheck) ++ functions.map(typecheck)

    // TODO - verify main signature

  def typecheckStmt(stmt: Statement[Untyped])(using Context): Statement[Typed] =
    stmt match
      case cd @ ClassDecl(_, _, _)       => typecheck(cd)
      case fd @ FunctionDecl(_, _, _, _) => typecheck(fd)
      case vd @ VarDecl(_, _, _)         => typecheck(vd)
      case is @ IfStatement(_, _, _)     => typecheck(is)
      case ws @ WhileStatement(_, _)     => typecheck(ws)
      case fs @ ForStatement(_, _, _, _) => typecheck(fs)
      case bs @ BreakStatement()         => typecheck(bs)
      case cs @ ContinueStatement()      => typecheck(cs)
      case rs @ ReturnStatement(_)       => typecheck(rs)
      case va @ VarAssign(_, _)          => typecheck(va)
      case fa @ FieldAssign(_, _, _)     => typecheck(fa)
      case es @ ExprStatement(_)         => typecheck(es)

  def typecheck(cd: ClassDecl[Untyped])(using Context): ClassDecl[Typed] = ???

  def typecheck(fd: FunctionDecl[Untyped])(using Context): FunctionDecl[Typed] =
    val FunctionDecl(name, params, retType, body) = fd
    val newCtx = params
      .foldLeft(ctx) { case (ct, (n, t)) => ct.withVar(n, t) }
      .withRetType(retType)
    val tBody = typecheckBlock(body)(using newCtx)
    FunctionDecl(name, params, retType, tBody)

  def typecheck(vd: VarDecl[Untyped])(using Context): VarDecl[Typed] =
    val VarDecl(name, typ, expr) = vd
    val tExpr = typecheckExpr(expr)
    if typ != None && typ.get != tExpr.typ then
      throw Exception("Variable declaration type mismatch")
    VarDecl(name, typ.orElse(Some(tExpr.typ)), tExpr)

  def typecheck(is: IfStatement[Untyped])(using Context): IfStatement[Typed] =
    val IfStatement(cond, ifTrue, ifFalse) = is
    val tCond = typecheckExpr(cond)
    if tCond.typ != Type.Bool then throw Exception("if condition must be Bool")

    val notNull = getNotNullAsserts(tCond)
    val tIfTrue = typecheckBlock(ifTrue)(using ctx.withNotNull(notNull))
    val tIfFalse = typecheckBlock(ifFalse)
    IfStatement(tCond, tIfTrue, tIfFalse)

  def typecheck(ws: WhileStatement[Untyped])(using Context): WhileStatement[Typed] =
    val WhileStatement(cond, body) = ws
    val tCond = typecheckExpr(cond)
    if tCond.typ != Type.Bool then throw Exception("while condition must be Bool")
    WhileStatement(tCond, typecheckBlock(body)(using ctx.withLoop))

  def typecheck(fs: ForStatement[Untyped])(using Context): ForStatement[Typed] = ???

  def typecheck(bs: BreakStatement[Untyped])(using Context): BreakStatement[Typed] =
    if !ctx.isLoop then throw Exception("break must be inside loop")
    else BreakStatement()

  def typecheck(cs: ContinueStatement[Untyped])(using Context): ContinueStatement[Typed] =
    if !ctx.isLoop then throw Exception("continue must be inside loop")
    else ContinueStatement()

  def typecheck(rs: ReturnStatement[Untyped])(using Context): ReturnStatement[Typed] = ???

  def typecheck(va: VarAssign[Untyped])(using Context): VarAssign[Typed] =
    val VarAssign(name, expr) = va
    val (_, typ) = ctx.vars
      .find { (n, typ) => n == name }
      .getOrElse(throw Exception(s"Variable $name is not defined"))
    val tExpr = typecheckExpr(expr)
    if !isSubtypeOf(tExpr.typ, typ) then throw Exception("Variable assignment type mismatch")
    VarAssign(name, tExpr)

  def typecheck(fa: FieldAssign[Untyped])(using Context): FieldAssign[Typed] = ???

  def typecheck(es: ExprStatement[Untyped])(using Context): ExprStatement[Typed] =
    ExprStatement(typecheckExpr(es.expr))

  def typecheckBlock(block: List[Statement[Untyped]])(using Context): List[Statement[Typed]] =
    block match
      case stmt :: rest =>
        val tStmt = typecheckStmt(stmt)
        val newCtx = tStmt match
          case VarDecl(name, typ, _) => ctx.withVar(name, typ.get)
          case _                     => ctx
        val tRest = typecheckBlock(rest)(using newCtx)
        tStmt :: tRest
      case Nil => Nil

  def typecheckExpr(expr: Expr[Untyped])(using Context): Typed[Expr[Typed]] =
    import Type._
    expr match
      case NullLiteral()     => Typed(Null, NullLiteral())
      case BoolLiteral(v)    => Typed(Bool, BoolLiteral(v))
      case Int64Literal(v)   => Typed(Int64, Int64Literal(v))
      case Float64Literal(v) => Typed(Float64, Float64Literal(v))
      case StringLiteral(v)  => Typed(String, StringLiteral(v))
      case VariableExpr(name) =>
        ctx.vars
          .find { (n, typ) => n == name }
          .map { (_, typ) => Typed(typ, VariableExpr(name)) }
          .getOrElse(throw Exception(s"Variable $name is not defined"))
      case UnaryExpr(op, expr) =>
        import Unop._
        val tExpr = typecheckExpr(expr)
        (tExpr.typ, op) match
          case (Int64 | Float64, Minus) => Typed(tExpr.typ, UnaryExpr(Minus, tExpr))
          case (Bool, Not)              => Typed(Bool, UnaryExpr(Not, tExpr))
          case (tp, op) => throw Exception(s"Cannot use ${op} unary operator on ${tp} type")
      case BinaryExpr(lhs, op, rhs) =>
        import Binop._
        val tLhs = typecheckExpr(lhs)
        val tRhs = typecheckExpr(rhs)
        if tLhs.typ != tRhs.typ then throw Exception("Type mismatch in binop")

        (tLhs.typ, op) match
          case (tp @ (Int64 | Float64), op @ (LessThan | LessEqual | GreaterThan | GreaterEqual)) =>
            Typed(Bool, BinaryExpr(tLhs, op, tRhs))
          case (tp, op @ (Equal | NotEqual)) =>
            Typed(Bool, BinaryExpr(tLhs, op, tRhs))
          case (Int64, op @ (ShiftLeft | ShiftRight | BitAnd | BitOr | Modulo)) =>
            Typed(Int64, BinaryExpr(tLhs, op, tRhs))
          case (tp @ (Int64 | Float64), op @ (Plus | Minus | Times | Div)) =>
            Typed(tp, BinaryExpr(tLhs, op, tRhs))
          case (String, Plus) =>
            Typed(String, BinaryExpr(tLhs, Plus, tRhs))
          case (Bool, op @ (BoolAnd | BoolOr)) =>
            Typed(Bool, BinaryExpr(tLhs, op, tRhs))
          case (tp, op) =>
            throw Exception(s"Cannot use $op binary operator on $tp type values")
      case FuncCallExpr(_, _)        => ???
      case ObjAccessExpr(obj, field) => ???

  def getNotNullAsserts(expr: Typed[Expr[Typed]]): Set[String] = Set()
