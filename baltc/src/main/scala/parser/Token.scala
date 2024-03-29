package parser

case class Location(filename: String, line: Int, char: Int)

case class Token(typ: TokenType, content: String, loc: Location)

enum TokenType:
  // keywords
  case DefKeyword
  case ReturnKeyword
  case VarKeyword
  case ClassKeyword
  case IfKeyword
  case ElseKeyword
  case WhileKeyword
  case ForKeyword
  case BreakKeyword
  case ContinueKeyword

  case BoolTypeKeyword
  case Int64TypeKeyword
  case Float64TypeKeyword
  case StringTypeKeyword

  case TrueKeyword
  case FalseKeyword
  case NullKeyword

  case Identifier
  case IntLiteral
  case FloatLiteral
  case StringLiteral
  case CharLiteral

  // symbols
  case LeftParen
  case RightParen
  case LeftBracket
  case RightBracket
  case LeftBrace
  case RightBrace
  case Period
  case Comma
  case QuestionMark
  case Colon
  case Semicolon

  // operators
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
  case Assign
  case Not
