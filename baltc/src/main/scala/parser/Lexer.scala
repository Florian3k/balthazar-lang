package parser

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

import TokenType._

val regexes: List[(Regex, TokenType)] = List(
  ("^def\\b".r, DefKeyword),
  ("^return\\b".r, ReturnKeyword),
  ("^var\\b".r, VarKeyword),
  ("^class\\b".r, ClassKeyword),
  ("^if\\b".r, IfKeyword),
  ("^else\\b".r, ElseKeyword),
  ("^while\\b".r, WhileKeyword),
  ("^for\\b".r, ForKeyword),
  ("^break\\b".r, BreakKeyword),
  ("^continue\\b".r, ContinueKeyword),
  ("^bool\\b".r, BoolTypeKeyword),
  ("^int64\\b".r, Int64TypeKeyword),
  ("^float64\\b".r, Float64TypeKeyword),
  ("^String\\b".r, StringTypeKeyword),
  ("^true\\b".r, TrueKeyword),
  ("^false\\b".r, FalseKeyword),
  ("^([A-Za-z][A-Za-z0-9_]*)\\b".r, Identifier),
  ("^(\\+|-)?([0-9]+\\.[0-9]+)\\b".r, FloatLiteral),  // floats first, to prevent int eating the integer part of float
  ("^(\\+|-)?(0x|0b|0o)?([0-9a-fA-F]+)\\b".r, IntLiteral),
  ("^\\(".r, LeftParen),
  ("^\\)".r, RightParen),
  ("^\\[".r, LeftBracket),
  ("^\\]".r, RightBracket),
  ("^\\{".r, LeftBrace),
  ("^\\}".r, RightBrace),
  ("^\\.".r, Period),
  ("^\\,".r, Comma),
  ("^\\?".r, QuestionMark),
  ("^\\:".r, Colon),
  ("^;".r, Semicolon),
  ("^\\+".r, Plus),
  ("^-".r, Minus),
  ("^\\*".r, Times),
  ("^/".r, Div),
  ("^%".r, Modulo),
  ("^\\|".r, Or),
  ("^&".r, And),
  ("^<<".r, ShiftLeft),
  ("^>>".r, ShiftRight),
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
    else if char == '"' then 
      val strRe = "^(\".*?\")".r
      strRe.findFirstMatchIn(content) match
        case Some(m) => 
          val str = m.group(0)
          res.append(Token(StringLiteral, str, loc))
          charIdx += str.length
          content = content.drop(str.length)
        case _ => ??? //  TODO lexer error
    else if char == '\'' then ??? // TODO parse char literal
    else
      regexes.find((re, tt) => re.findFirstIn(content).isDefined) match
        case Some((re, tt)) =>
          val str = re.findFirstIn(content).get
          res.append(Token(tt, str, loc))
          charIdx += str.length
          content = content.drop(str.length)
        case None => throw new Exception(s"Unexpected character: $char [$loc]") // TODO handle lexer error

  return res.toList
