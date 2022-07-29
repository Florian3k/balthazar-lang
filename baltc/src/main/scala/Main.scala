import java.io.PrintWriter
import scala.io.Source

import ast.Statement.{FunctionDecl, ExprStatement}
import ast.Type
import opcode.{Opcode, Operand}
import parser.{lexer, Parser}
import typer.Typer

@main def main(inFile: String, outFile: String): Unit =
  val contents = Source.fromFile(inFile).mkString
  val program = Parser(lexer(contents, inFile)).parseProgram()
  val typed = Typer.typecheckProgram(program)

  val mainFn = typed
    .collect { case mainFn @ FunctionDecl("main", _, _, _) => mainFn }
    .headOption
    .getOrElse(throw Exception("Main function missing"))

  val codegen = Codegen()
  val bytecode = codegen.codegenStmt(mainFn)

  val json = ujson.Obj(
    "code" -> bytecode.appended(Opcode.OpRet).flatMap {
      case Operand.U8(i) => List(ujson.Num(i))
      case Operand.U16(i) =>
        List(ujson.Num(i & 0xff), ujson.Num((i & 0xff_00) >> 8))
      case Operand.S16(i) =>
        List(ujson.Num(i & 0xff), ujson.Num((i & 0xff_00) >> 8))
      case op: Opcode => List(ujson.Str(op.toString))
    },
    "constants" -> codegen.consts.map {
      case s: String => ujson.Obj("str" -> ujson.Str(s))
      case n: Long   => ujson.Str(n.toString)
    },
    "lines" -> ujson.Arr(),
  )

  val res = ujson.write(json, 4)
  new PrintWriter(outFile) {
    write(res)
    close
  }
