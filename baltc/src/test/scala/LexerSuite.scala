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
      Token(ShiftLeft, "<<", Location("file1", 1, 4)),
    )
    assertEquals(res, expected)
  }

  test("differentiate between comparissons and shifts") {
    val res = lexer("> > >> << < << <", "file1")
    val expected = List(
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
    val expected = List(
      Token(Modulo, "%", Location("file1", 1, 1))
    )
    assertEquals(res, expected)
  }

  test("differentiate between keywords and identifiers") {
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
      Token(StringLiteral, "\"foo\"", Location("file1", 1, 4)),
      Token(Identifier, "bar", Location("file1", 1, 10)),
    )
    assertEquals(res, expected)
  }

  test("function definition with complicated body") {
    val res = lexer(
      //          10        20        30
      // 1234567890123456789012345678901234567890
      """def fun1(x: int64, y: String?): int64 {
        |  for (var i = 0; i < 7; i += 1) {
        |    if (x % 2 == 0x42) {
        |      obj.method(x);
        |      continue;
        |    }
        |    break;
        |  }
        |  -1.abs();
        |  - 1.abs();
        |  return 5;
        |}
        """.stripMargin,
      "file1",
    )
    def loc(line: Int, char: Int) = Location("file1", line, char)
    val expected = List(
      // line 1
      Token(DefKeyword, "def", loc(1, 1)),
      Token(Identifier, "fun1", loc(1, 5)),
      Token(LeftParen, "(", loc(1, 9)),
      Token(Identifier, "x", loc(1, 10)),
      Token(Colon, ":", loc(1, 11)),
      Token(Int64TypeKeyword, "int64", loc(1, 13)),
      Token(Comma, ",", loc(1, 18)),
      Token(Identifier, "y", loc(1, 20)),
      Token(Colon, ":", loc(1, 21)),
      Token(StringTypeKeyword, "String", loc(1, 23)),
      Token(QuestionMark, "?", loc(1, 29)),
      Token(RightParen, ")", loc(1, 30)),
      Token(Colon, ":", loc(1, 31)),
      Token(Int64TypeKeyword, "int64", loc(1, 33)),
      Token(LeftBrace, "{", loc(1, 39)),
      // line 2
      Token(ForKeyword, "for", loc(2, 3)),
      Token(LeftParen, "(", loc(2, 7)),
      Token(VarKeyword, "var", loc(2, 8)),
      Token(Identifier, "i", loc(2, 12)),
      Token(Assign, "=", loc(2, 14)),
      Token(IntLiteral, "0", loc(2, 16)),
      Token(Semicolon, ";", loc(2, 17)),
      Token(Identifier, "i", loc(2, 19)),
      Token(LessThan, "<", loc(2, 21)),
      Token(IntLiteral, "7", loc(2, 23)),
      Token(Semicolon, ";", loc(2, 24)),
      Token(Identifier, "i", loc(2, 26)),
      Token(Plus, "+", loc(2, 28)),
      Token(Assign, "=", loc(2, 29)),
      Token(IntLiteral, "1", loc(2, 31)),
      Token(RightParen, ")", loc(2, 32)),
      Token(LeftBrace, "{", loc(2, 34)),
      // line 3
      Token(IfKeyword, "if", loc(3, 5)),
      Token(LeftParen, "(", loc(3, 8)),
      Token(Identifier, "x", loc(3, 9)),
      Token(Modulo, "%", loc(3, 11)),
      Token(IntLiteral, "2", loc(3, 13)),
      Token(Equal, "==", loc(3, 15)),
      Token(IntLiteral, "0x42", loc(3, 18)),
      Token(RightParen, ")", loc(3, 22)),
      Token(LeftBrace, "{", loc(3, 24)),
      // line 4
      Token(Identifier, "obj", loc(4, 7)),
      Token(Period, ".", loc(4, 10)),
      Token(Identifier, "method", loc(4, 11)),
      Token(LeftParen, "(", loc(4, 17)),
      Token(Identifier, "x", loc(4, 18)),
      Token(RightParen, ")", loc(4, 19)),
      Token(Semicolon, ";", loc(4, 20)),
      // line 5
      Token(ContinueKeyword, "continue", loc(5, 7)),
      Token(Semicolon, ";", loc(5, 15)),
      // line 6
      Token(RightBrace, "}", loc(6, 5)),
      // line 7
      Token(BreakKeyword, "break", loc(7, 5)),
      Token(Semicolon, ";", loc(7, 10)),
      // line 8
      Token(RightBrace, "}", loc(8, 3)),
      // line 9
      Token(IntLiteral, "-1", loc(9, 3)),
      Token(Period, ".", loc(9, 5)),
      Token(Identifier, "abs", loc(9, 6)),
      Token(LeftParen, "(", loc(9, 9)),
      Token(RightParen, ")", loc(9, 10)),
      Token(Semicolon, ";", loc(9, 11)),
      // line 10
      Token(Minus, "-", loc(10, 3)),
      Token(IntLiteral, "1", loc(10, 5)),
      Token(Period, ".", loc(10, 6)),
      Token(Identifier, "abs", loc(10, 7)),
      Token(LeftParen, "(", loc(10, 10)),
      Token(RightParen, ")", loc(10, 11)),
      Token(Semicolon, ";", loc(10, 12)),
      // line 11
      Token(ReturnKeyword, "return", loc(11, 3)),
      Token(IntLiteral, "5", loc(11, 10)),
      Token(Semicolon, ";", loc(11, 11)),
      // line 12
      Token(RightBrace, "}", loc(12, 1)),
    )
    assertEquals(res, expected)
  }
