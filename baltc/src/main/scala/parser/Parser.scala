package parser

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{breakable, break}
import ast._
import ast.Statement._
import ast.Expr._
import parser.TokenType._

class Parser(var input: List[Token]):
  private def consumeToken(): Token =
    input match
      case token :: rest =>
        input = rest
        token
      case Nil =>
        throw new Exception("Unexpected end of input")

  private def consumeToken(tt: TokenType): Token =
    input match
      case token :: rest if token.typ == tt =>
        input = rest
        token
      case token :: _ =>
        throw new Exception(s"Expected $tt, instead got $token")
      case Nil =>
        throw new Exception(s"Expected $tt")

  private def consumeIdentifier() =
    consumeToken(Identifier).content

  private def peek(idx: Int = 0): Token = input(idx)

  private def peekType: TokenType = peek().typ

  def parseProgram(): List[Statement[Untyped]] =
    val stmts = ListBuffer[Statement[Untyped]]()
    while input.nonEmpty do stmts.addOne(parseStatement())
    stmts.toList

  def parseStatement(): Statement[Untyped] =
    peekType match
      case ClassKeyword    => parseClassDecl()
      case DefKeyword      => parseFunctionDecl()
      case VarKeyword      => parseVarDecl()
      case IfKeyword       => parseIfStatement()
      case WhileKeyword    => parseWhileStatement()
      case ForKeyword      => parseForStatement()
      case BreakKeyword    => parseBreakStatement()
      case ContinueKeyword => parseContinueStatement()
      case ReturnKeyword   => parseReturnStatement()
      case _               => parseExprOrAssign()

  def parseClassDecl(): ClassDecl[Untyped] =
    consumeToken(ClassKeyword)
    val className = consumeIdentifier()
    consumeToken(LeftBrace)

    val fields = ListBuffer[(String, Type)]()
    while peekType == VarKeyword do
      consumeToken(VarKeyword)
      val name = consumeIdentifier()
      consumeToken(Colon)
      val typ = parseType()
      consumeToken(Semicolon)
      fields.addOne((name, typ))

    val methods = ListBuffer[FunctionDecl[Untyped]]()
    while peekType == DefKeyword do methods.addOne(parseFunctionDecl())

    consumeToken(RightBrace)
    ClassDecl(className, fields.toList, methods.toList)

  def parseFunctionDecl(): FunctionDecl[Untyped] =
    consumeToken(DefKeyword)
    val fnName = consumeIdentifier()

    val params = ListBuffer[(String, Type)]()
    consumeToken(LeftParen)
    while peekType != RightParen do
      val pName = consumeIdentifier()
      consumeToken(Colon)
      val pType = parseType()
      if peekType != RightParen then consumeToken(Comma)
      params.addOne((pName, pType))
    consumeToken(RightParen)

    val retType =
      if peekType == Colon then
        consumeToken(Colon)
        Some(parseType())
      else None

    val body = parseBlock()

    FunctionDecl(fnName, params.toList, retType, body)

  def parseVarDecl(): VarDecl[Untyped] =
    consumeToken(VarKeyword)
    val varName = consumeIdentifier()

    val typ =
      if peekType == Colon then
        consumeToken(Colon)
        Some(parseType())
      else None

    consumeToken(Assign)
    var expr = parseExpr()
    consumeToken(Semicolon)

    VarDecl(varName, typ, expr)

  def parseIfStatement(): IfStatement[Untyped] =
    consumeToken(IfKeyword)
    consumeToken(LeftParen)
    var cond = parseExpr()
    consumeToken(RightParen)

    var ifTrue = parseBlock()

    var ifFalse =
      if peekType == ElseKeyword then
        consumeToken(ElseKeyword)
        parseBlock()
      else List()

    IfStatement(cond, ifTrue, ifFalse)

  def parseWhileStatement(): WhileStatement[Untyped] =
    consumeToken(WhileKeyword)
    consumeToken(LeftParen)
    var cond = parseExpr()
    consumeToken(RightParen)
    WhileStatement(cond, parseBlock())

  def parseForStatement(): ForStatement[Untyped] =
    consumeToken(ForKeyword)
    consumeToken(LeftParen)

    val init = peekType match
      case Semicolon  => None
      case VarKeyword => Some(parseVarDecl())
      case _          => Some(parseExprOrAssign())

    val cond = if peekType == Semicolon then None else Some(parseExpr())
    consumeToken(Semicolon)

    val inc =
      if peekType == RightParen then None
      else Some(parseExprOrAssign(consumeSemicolon = false))

    consumeToken(RightParen)

    ForStatement(init, cond, inc, parseBlock())

  def parseBreakStatement(): Statement[Untyped] =
    consumeToken(BreakKeyword)
    consumeToken(Semicolon)
    BreakStatement()

  def parseContinueStatement(): ContinueStatement[Untyped] =
    consumeToken(ContinueKeyword)
    consumeToken(Semicolon)
    ContinueStatement()

  def parseReturnStatement(): ReturnStatement[Untyped] =
    consumeToken(ReturnKeyword)
    val expr = peekType match
      case Semicolon => None
      case _         => Some(parseExpr())
    consumeToken(Semicolon)
    ReturnStatement(expr)

  def parseBlock(): List[Statement[Untyped]] =
    consumeToken(LeftBrace)
    val stmts = ListBuffer[Statement[Untyped]]()
    while peekType != RightBrace do stmts.addOne(parseStatement())
    consumeToken(RightBrace)
    stmts.toList

  def parseExprOrAssign(
      consumeSemicolon: Boolean = true
  ): ExprOrAssign[Untyped] =
    parseExpr() match
      case VariableExpr(name) if peekType == Assign =>
        consumeToken(Assign)
        val expr = parseExpr()
        if consumeSemicolon then consumeToken(Semicolon)
        VarAssign(name, expr)
      case ObjAccessExpr(obj, field) if peekType == Assign =>
        consumeToken(Assign)
        val expr = parseExpr()
        if consumeSemicolon then consumeToken(Semicolon)
        FieldAssign(obj, field, expr)
      case e =>
        if consumeSemicolon then consumeToken(Semicolon)
        ExprStatement(e)

  def parseType(): Type =
    val token = peek()
    def consume() = consumeToken(token.typ)
    val typ = token.typ match
      case BoolTypeKeyword =>
        consume()
        Type.Bool
      case Int64TypeKeyword =>
        consume()
        Type.Int64
      case Float64TypeKeyword =>
        consume()
        Type.Float64
      case StringTypeKeyword =>
        consume()
        Type.String
      case Identifier =>
        Type.Typename(consume().content)
      case _ =>
        throw new Exception(s"Expected type, instead got: ${token.content}")

    if peekType == QuestionMark then
      consumeToken(QuestionMark)
      Type.Nullable(typ)
    else typ

  def tokenToBinop(tt: TokenType): Binop =
    tt match
      case Plus         => Binop.Plus
      case Minus        => Binop.Minus
      case Times        => Binop.Times
      case Div          => Binop.Div
      case Modulo       => Binop.Modulo
      case ShiftLeft    => Binop.ShiftLeft
      case ShiftRight   => Binop.ShiftRight
      case BitAnd       => Binop.BitAnd
      case BitOr        => Binop.BitOr
      case LessEqual    => Binop.LessEqual
      case LessThan     => Binop.LessThan
      case GreaterEqual => Binop.GreaterEqual
      case GreaterThan  => Binop.GreaterThan
      case Equal        => Binop.Equal
      case NotEqual     => Binop.NotEqual
      case BoolAnd      => Binop.BoolAnd
      case BoolOr       => Binop.BoolOr
      case _            => ???

  def parseExpr(): Expr[Untyped] =
    parseBinopExpr(
      List(
        Right(List(BoolOr)),
        Right(List(BoolAnd)),
        Left(List(Equal, NotEqual)),
        Left(List(LessThan, LessEqual, GreaterThan, GreaterEqual)),
        Left(List(BitOr)),
        Left(List(BitAnd)),
        Left(List(ShiftLeft, ShiftRight)),
        Left(List(Plus, Minus)),
        Left(List(Times, Div, Modulo)),
      )
    )

  def parseBinopExpr(
      table: List[Either[List[TokenType], List[TokenType]]]
  ): Expr[Untyped] =
    table match
      case Left(tokens) :: rest =>
        var expr = parseBinopExpr(rest)
        while tokens.contains(peekType) do
          val tt = peekType
          consumeToken(tt)
          expr = BinaryExpr(expr, tokenToBinop(tt), parseBinopExpr(rest))
        expr
      case Right(tokens) :: rest =>
        val expr = parseBinopExpr(rest)
        if tokens.contains(peekType) then
          val tt = peekType
          consumeToken(tt)
          BinaryExpr(expr, tokenToBinop(tt), parseBinopExpr(table))
        else expr
      case Nil =>
        parseUnaryExpr()

  def parseUnaryExpr(): Expr[Untyped] =
    peekType match
      case Minus =>
        consumeToken(Minus)
        UnaryExpr(Unop.Minus, parseUnaryExpr())
      case Not =>
        consumeToken(Not)
        UnaryExpr(Unop.Not, parseUnaryExpr())
      case _ =>
        parseCallAndAccessExpr()

  def parseCallAndAccessExpr(): Expr[Untyped] =
    def parseCallArgs(): List[Expr[Untyped]] =
      consumeToken(LeftParen)
      val args = ListBuffer[Expr[Untyped]]()
      while peekType != RightParen do
        args.addOne(parseExpr())
        if peekType != RightParen then consumeToken(Comma)
      consumeToken(RightParen)
      args.toList

    var expr = parseBasicExpr()
    breakable {
      while true do
        peekType match
          case LeftParen =>
            expr = FuncCallExpr(expr, parseCallArgs())
          case Period =>
            consumeToken(Period)
            val field = consumeIdentifier()
            expr =
              if peekType == LeftParen then
                MethodCallExpr(expr, field, parseCallArgs())
              else ObjAccessExpr(expr, field)
          case t =>
            break
    }
    expr

  def parseBasicExpr(): Expr[Untyped] =
    val token = consumeToken()
    token.typ match
      case NullKeyword  => NullLiteral()
      case TrueKeyword  => BoolLiteral(true)
      case FalseKeyword => BoolLiteral(false)
      case IntLiteral =>
        val intVal = token.content match
          case s"0x$rest" => java.lang.Long.valueOf(rest, 16)
          case s"0b$rest" => java.lang.Long.valueOf(rest, 2)
          case s"0o$rest" => java.lang.Long.valueOf(rest, 8)
          case rest       => java.lang.Long.valueOf(rest)
        Int64Literal(intVal)
      case FloatLiteral => Float64Literal(token.content.toDouble)
      case TokenType.StringLiteral =>
        Expr.StringLiteral(token.content.drop(1).dropRight(1))
      case CharLiteral => ???
      case Identifier  => VariableExpr(token.content)
      case LeftParen =>
        val expr = parseExpr()
        consumeToken(RightParen)
        expr
      case _ =>
        throw new Exception(
          s"Unexpected token: ${token.content} [${token.loc}]"
        )

end Parser
