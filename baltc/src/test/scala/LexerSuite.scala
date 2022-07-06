import parser._
import TokenType._

class LexerSuite extends munit.FunSuite:
  test("simple lexer test") {
    val res = lexer(" def+  // asdf", "file1")
    val expected = List(
      Token(DefKeyword, "def", Location("file1", 1, 2)),
      Token(Plus, "+", Location("file1", 1, 5)),
    )
    assertEquals(res, expected)
  }

  test("recognize shift operators") {
    val res = lexer(">> <<", "file1")
    val expected = List(
      Token(ShiftRight, ">>", Location("file1", 1, 1)),
      Token(ShiftLeft,  "<<", Location("file1", 1, 4))
    )
    assertEquals(res, expected)
  }

  test("differentiate between comparissons and shifts") {
    val res = lexer("> > >> << < << <", "file1")
    val expected = List (
      Token(GreaterThan, ">", Location("file1", 1, 1)),
      Token(GreaterThan, ">", Location("file1", 1, 3)),
      Token(ShiftRight, ">>", Location("file1", 1, 5)),
      Token(ShiftLeft, "<<", Location("file1", 1, 8)),
      Token(LessThan, "<", Location("file1", 1, 11)),
      Token(ShiftLeft, "<<", Location("file1", 1, 13)),
      Token(LessThan, "<", Location("file1", 1, 16)),
    )
    assertEquals(res, expected)
  }

  test("lex modulo operator") {
    val res = lexer("%", "file1")
    val expected = List (
      Token(Modulo, "%", Location("file1", 1, 1))
    )
    assertEquals(res, expected)
  }

  test("differentiate between keywords and identifiers"){
    val res = lexer("def foo bar if else while for true baz", "file1")
    val expected = List(
      Token(DefKeyword, "def", Location("file1", 1, 1)),
      Token(Identifier, "foo", Location("file1", 1, 5)),
      Token(Identifier, "bar", Location("file1", 1, 9)),
      Token(IfKeyword, "if", Location("file1", 1, 13)),
      Token(ElseKeyword, "else", Location("file1", 1, 16)),
      Token(WhileKeyword, "while", Location("file1", 1, 21)),
      Token(ForKeyword, "for", Location("file1", 1, 27)),
      Token(TrueKeyword, "true", Location("file1", 1, 31)),
      Token(Identifier, "baz", Location("file1", 1, 36)),
    )
    assertEquals(res, expected)
  }

  test("lex int literals") {
    val res = lexer("1234 -0xdeadc0de 0b1010101", "file1")
    val expected = List(
      Token(IntLiteral, "1234", Location("file1", 1, 1)),
      Token(IntLiteral, "-0xdeadc0de", Location("file1", 1, 6)),
      Token(IntLiteral, "0b1010101", Location("file1", 1, 18)),
    )
    assertEquals(res, expected)
  }

  test("lex float literals") {
    val res = lexer("0.1 00.1234 -12.4", "file1")
    val expected = List(
      Token(FloatLiteral, "0.1", Location("file1", 1, 1)),
      Token(FloatLiteral, "00.1234", Location("file1", 1, 5)),
      Token(FloatLiteral, "-12.4", Location("file1", 1, 13)),
    )
    assertEquals(res, expected)
  }

  test("basic string literals") {
    val res = lexer("10 \"foo\" bar", "file1")
    val expected = List(
      Token(IntLiteral, "10", Location("file1", 1, 1)),
      Token(StringLiteral, "\"foo\"", Location("file1",1, 4)),
      Token(Identifier, "bar", Location("file1", 1, 10)),
    )
    assertEquals(res, expected)
  }
