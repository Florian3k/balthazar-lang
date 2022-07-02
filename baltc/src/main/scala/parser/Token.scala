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

  // symbols
  case LeftParen
  case RightParen
  case LeftBracket
  case RightBracket
  case LeftBrace
  case RightBrace
  case Semicolon
  case SingleQuote
  case DoubleQuote

  // operators
  case Plus
  case Minus
  case Times
  case Div
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
