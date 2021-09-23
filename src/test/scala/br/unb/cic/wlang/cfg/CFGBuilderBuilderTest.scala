package br.unb.cic.wlang.cfg

import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class CFGBuilderBuilderTest extends AnyFunSuite {

  test("Test simple CFG") {
    val stmt = Assignment("x", Const(4), 1)
    val program = WhileProgram(List(), stmt)

    val g = CFGBuilder.flow(program)

    val expected = Set()

    assert(expected == g)
  }

  test("Test factorial CFG") {
    val d1 = Assignment("y", Variable("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Variable("z"), Variable("y")), 4)
    val d4 = Assignment("y", Sub(Variable("y"), Const(1)), 5)
    val w1 = While(Condition(GT(Variable("y"), Const(1)), 3), Sequence(d3, d4))
    val d5 = Assignment("y", Const(0), 6)

    val p = WhileProgram(List(), Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val g = CFGBuilder.flow(p)

    val expected: Set[(Int, Int)] =
      Set((1, 2)
         ,(2, 3)
         ,(3, 4)
         ,(4, 5)
         ,(5, 3)
         ,(3, 6))

    assert(expected == g)
  }

  test("Test for interprocedural fibonacci CFG") {
    val program = WhileProgram(List(
      Procedure("fib", List(FormalArgument("z",ByValue), FormalArgument("u",ByValue), FormalArgument("v",ByResult)),1,
        IfThenElse(Condition(LT(Variable("z"),Const(3)),2),
          Assignment("v",Add(Variable("u"),Const(1)),3),
          Sequence(Call("fib",List(Sub(Variable("z"),Const(1)), Variable("u"), Variable("v")),4,5),
                   Call("fib",List(Sub(Variable("z"),Const(2)), Variable("v"), Variable("v")),6,7))),8)),
      Call("fib",List(Const(10), Const(0), Variable("y")),9,10)
    )

    val g = CFGBuilder.flow(program)

    val expected = Set(
      (1,2), (2,3), (3,8),
      (2,4), (4,1), (8,5), (5,6), (6,1), (8,7), (7,8),
      (9, 1), (8,10)
    )

    assert(g == expected)
  }

}
