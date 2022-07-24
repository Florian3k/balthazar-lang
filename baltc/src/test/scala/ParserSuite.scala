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
        FunctionDecl[Untyped](
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
                    ExprStatement(FuncCallExpr(ObjAccessExpr(VariableExpr("obj"), "method"), List(NullLiteral()))),
                    ContinueStatement(),
                  ),
                  ifFalse = List(),
                ),
                BreakStatement(),
              ),
            ),
            ReturnStatement(Some(Int64Literal(5))),
          ),
        )
      ),
    )
  }

  test("class definition and boolean operators") {
    val tokens = lexer(
      """class Foo {
        |  var field1: int64;
        |  var field2: float64;
        |
        |  def Foo() {
        |    obj.call(1, 2).field.other(3)(4, 5);
        |  }
        |
        |  def bar(): bool {
        |    return a || b || c == d && e && f;
        |  }
        |}
        """.stripMargin,
      "file1",
    )
    val tree = Parser(tokens).parseProgram()
    assertEquals(
      tree,
      List(
        ClassDecl[Untyped](
          "Foo",
          List(("field1", Int64), ("field2", Float64)),
          List(
            FunctionDecl(
              "Foo",
              List(),
              None,
              List(
                ExprStatement(
                  FuncCallExpr(
                    FuncCallExpr(
                      ObjAccessExpr(
                        ObjAccessExpr(
                          FuncCallExpr(
                            ObjAccessExpr(VariableExpr("obj"), "call"),
                            List(Int64Literal(1), Int64Literal(2)),
                          ),
                          "field",
                        ),
                        "other",
                      ),
                      List(Int64Literal(3)),
                    ),
                    List(Int64Literal(4), Int64Literal(5)),
                  )
                )
              ),
            ),
            FunctionDecl(
              "bar",
              List(),
              Some(Bool),
              List(
                ReturnStatement(
                  Some(
                    BinaryExpr(
                      VariableExpr("a"),
                      Binop.BoolOr,
                      BinaryExpr(
                        VariableExpr("b"),
                        Binop.BoolOr,
                        BinaryExpr(
                          BinaryExpr(VariableExpr("c"), Binop.Equal, VariableExpr("d")),
                          Binop.BoolAnd,
                          BinaryExpr(VariableExpr("e"), Binop.BoolAnd, VariableExpr("f")),
                        ),
                      ),
                    )
                  )
                )
              ),
            ),
          ),
        )
      ),
    )
  }
