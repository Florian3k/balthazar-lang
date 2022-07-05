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
