package br.unb.cic.wlang.cfg

import org.scalatest.funsuite.AnyFunSuite
import CFGToDot._
import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.parser.{ResourceHandle, WhileProgramParser}

class CFGToDOTTest extends AnyFunSuite {

  test("Simple test to the exportDot function") {
    val cfg = Set((1,2), (2,3), (3,4))

    val expected = "digraph CFG { \n  1 -> 2\n  2 -> 3\n  3 -> 4\n}"

    assert(expected == exportDot(cfg))
  }

  test("Test for the DOT notation of the factorial program") {
    val content = ResourceHandle.getContent("Factorial.wp")

    assert(content != null)

    val p: WhileProgramParser = new WhileProgramParser()

    val wp: WhileProgram = p.parse(p.whileProgram, content) match {
      case p.Success(program: WhileProgram, _) => program
      case p.Failure(msg, _) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }

    val cfg = CFGBuilder.flow(wp)

    println(exportDot(cfg))
  }
}
