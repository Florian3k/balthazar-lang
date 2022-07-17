import parser._
import ast._
import ast.Expr._
import ast.Statement._
import ast.Type._

class ParserSuite extends munit.FunSuite:
  test("function definition with complicated body") {
    val tokens = lexer(
      // 123456789012345678901234567890
      """def fun1(x: int64, y: String?): int64 {
        |  for (var i = 0; i < 7; i = i + 1) {
        |    if (x % 2 == 0x42) {
        |      obj.method(null);
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
    val tree = Parser(tokens).parseProgram()
    // scalafmt: { maxColumn = 120 }
    assertEquals(
      tree,
      List(
        FunctionDecl(
          name = "fun1",
          params = List(("x", Int64), ("y", Nullable(String))),
          retType = Some(Int64),
          body = List(
            ForStatement(
              init = Some(VarDecl("i", None, Int64Literal(0))),
              cond = Some(BinaryExpr(VariableExpr("i"), Binop.LessThan, Int64Literal(7))),
              inc = Some(VarAssign("i", BinaryExpr(VariableExpr("i"), Binop.Plus, Int64Literal(1)))),
              body = List(
                IfStatement(
                  cond = BinaryExpr(
                    BinaryExpr(VariableExpr("x"), Binop.Modulo, Int64Literal(2)),
                    Binop.Equal,
                    Int64Literal(66),
                  ),
                  ifTrue = List(
                    ExprStatement(MethodCallExpr(VariableExpr("obj"), "method", List(NullLiteral))),
                    ContinueStatement(),
                  ),
                  ifFalse = List(),
                ),
                BreakStatement(),
              ),
            ),
            ExprStatement(MethodCallExpr(Int64Literal(-1), "abs", List())),
            ExprStatement(UnaryExpr(Unop.Minus, MethodCallExpr(Int64Literal(1), "abs", List()))),
            ReturnStatement(Some(Int64Literal(5))),
          ),
        )
      ),
    )
  }
