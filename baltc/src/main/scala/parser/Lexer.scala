package parser

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

import TokenType._

val regexes: List[(Regex, TokenType)] = List(
  ("^def\\b".r, DefKeyword),
  ("^class\\b".r, ClassKeyword),
  ("^if\\b".r, IfKeyword),
  ("^else\\b".r, ElseKeyword),
  ("^while\\b".r, WhileKeyword),
  ("^for\\b".r, ForKeyword),
  ("^true\\b".r, TrueKeyword),
  ("^false\\b".r, FalseKeyword),
  ("^([A-Za-z][A-Za-z0-9_]+)\\b".r, Identifier),
  ("^(\\+|-)?(0x|0b|0o)?([0-9]+)\\b".r, IntLiteral),
  ("^(\\+|-)?([0-9]+\\.[0-9]+)\\b".r, FloatLiteral),
  ("^\\(".r, LeftParen),
  ("^\\)".r, RightParen),
  ("^\\[".r, LeftBracket),
  ("^\\]".r, RightBracket),
  ("^\\{".r, LeftBrace),
  ("^\\}".r, RightBrace),
  ("^;".r, Semicolon),
  ("^'".r, SingleQuote),
  ("^\"".r, DoubleQuote),
  ("^\\+".r, Plus),
  ("^-".r, Minus),
  ("^\\*".r, Times),
  ("^/".r, Div),
  ("^\\|".r, Or),
  ("^&".r, And),
  ("^\\<=".r, LessEqual),
  ("^\\<".r, LessThan),
  ("^\\>=".r, GreaterEqual),
  ("^\\>".r, GreaterThan),
  ("^==".r, Equal),
  ("^=".r, Assign),
  ("^!=".r, NotEqual),
  ("^!".r, Not),
)

def lexer(initialContent: String, filename: String): List[Token] =
  val res = ListBuffer[Token]()
  var content = initialContent
  var lineIdx = 1
  var charIdx = 1

  def loc = Location(filename, lineIdx, charIdx)

  while content.nonEmpty do
    val char = content.head
    if char == ' ' || char == '\t' then
      charIdx += 1
      content = content.tail
    else if char == '\n' then
      charIdx = 1
      lineIdx += 1
      content = content.tail
    else if content.startsWith("//") then
      while content.nonEmpty && content.head != '\n' do
        charIdx += 1
        content = content.tail
    else
      regexes.find((re, tt) => re.findFirstIn(content).isDefined) match
        case Some((re, SingleQuote)) => ???
        case Some((re, DoubleQuote)) => ???
        case Some((re, tt @ (Identifier | IntLiteral | FloatLiteral))) =>
          val res = re.findFirstIn(content)
          ???
        case Some((re, tt)) =>
          val str = re.findFirstIn(content).get
          print(str)
          res.append(Token(tt, str, loc))
          charIdx += str.length
          content = content.drop(str.length)
        case None => ???

  return res.toList
