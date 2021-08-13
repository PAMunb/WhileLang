package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

class CFGBuilderBuilderTest extends AnyFunSuite {

  test("Test simple CFG") {
    val stmt = Assignment("x", Const(4), 1)
    val program = WhileProgram(stmt)
    val resultCFG = CFGBuilder.build(program)

    val expectedCFG = Set.empty

    assert(expectedCFG == resultCFG)
  }

  test("Test factorial CFG") {
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(GT(Var("y"), Const(1)), Sequence(d3, d4), 3)
    val d5 = Assignment("y", Const(0), 6)

    val program = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val resultCFG = CFGBuilder.build(program)

    val expectedCFG =
      Set((d1.label, d2.label)
         ,(d2.label, w1.label)
         ,(w1.label, d3.label)
         ,(d3.label, d4.label)
         ,(d4.label, w1.label)
         ,(w1.label, d5.label))

    assert(expectedCFG == resultCFG)
  }

}
