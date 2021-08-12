package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

class CFGBuilderBuilderTest extends AnyFunSuite {

  test("Test simple CFG") {
    val stmt = Assignment("x", Const(4), 1)
    val program = WhileProgram(stmt)
    val sn = Node(stmt)
    val g = CFGBuilder.build(program)

    val expected = Set()

    assert(expected == g)
  }

  test("Test factorial CFG") {
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(GT(Var("y"), Const(1)), Sequence(d3, d4), 3)
    val d5 = Assignment("y", Const(0), 6)

    val p = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val g = CFGBuilder.build(p)

    val expected: Set[(Node, Node)] =
      Set((Node(d1), Node(d2))
         ,(Node(d2), Node(w1))
         ,(Node(w1), Node(d3))
         ,(Node(d3), Node(d4))
         ,(Node(d4), Node(w1))
         ,(Node(w1), Node(d5)))

    assert(expected == g)
  }

}
