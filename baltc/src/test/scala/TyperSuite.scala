package parser

import ast._
import ast.Statement._
import ast.Expr._
import ast.Type._
import typer.Typer

class TyperSuite extends munit.FunSuite:
  test("should typecheck simple binary expr") {
    val expr: Expr[Untyped] = Parser(lexer("2 + 2 > 3;", "file1")).parseExpr()
    val tExpr = Typer.typecheckExpr(expr)(using Typer.Context.empty)
    assertEquals(
      tExpr,
      Typed(
        Bool,
        BinaryExpr(
          Typed(
            Int64,
            BinaryExpr(
              Typed(Int64, Int64Literal(2)),
              Binop.Plus,
              Typed(Int64, Int64Literal(2)),
            ),
          ),
          Binop.GreaterThan,
          Typed(Int64, Int64Literal(3)),
        ),
      ),
    )
  }

  test("should typecheck simple main function declaration") {
    val program =
      Parser(lexer("def main() { 2 + 2 > 3; }", "file1")).parseProgram()
    val tProgram = Typer.typecheckProgram(program)
    assertEquals(
      tProgram,
      List(
        FunctionDecl(
          "main",
          List(),
          None,
          List(
            ExprStatement(
              Typed(
                Bool,
                BinaryExpr(
                  Typed(
                    Int64,
                    BinaryExpr(
                      Typed(Int64, Int64Literal(2)),
                      Binop.Plus,
                      Typed(Int64, Int64Literal(2)),
                    ),
                  ),
                  Binop.GreaterThan,
                  Typed(Int64, Int64Literal(3)),
                ),
              )
            )
          ),
        )
      ),
    )
  }
