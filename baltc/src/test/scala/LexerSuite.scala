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
