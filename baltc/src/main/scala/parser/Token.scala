package parser

case class Location(filename: String, line: Int, char: Int)

case class Token(typ: TokenType, content: String, loc: Location)

enum TokenType:
  // keywords
  case DefKeyword
  case ClassKeyword
  case IfKeyword
  case ElseKeyword
  case WhileKeyword
  case ForKeyword

  case TrueKeyword
  case FalseKeyword

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
  case Semicolon

  // operators
  case Plus
  case Minus
  case Times
  case Div
  case Modulo
  case Or
  case And
  case LessEqual
  case LessThan
  case GreaterEqual
  case GreaterThan
  case Equal
  case Assign
  case NotEqual
  case Not
  case ShiftLeft
  case ShiftRight
