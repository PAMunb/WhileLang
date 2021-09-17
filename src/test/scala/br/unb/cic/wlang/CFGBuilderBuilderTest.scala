package br.unb.cic.wlang

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
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(Condition(GT(Var("y"), Const(1)), 3), Sequence(d3, d4))
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
  
  test("Test flow com interprocedural") {
    //Call(name: String, args: List[AExp], lc: Label, lr: Label)

    val d1 = Skip(1)

    val d3 = Assignment("v", Add(Var("u"), Const(1)), 3)

    val d4 = Call("fib", List(Sub(Var("z"), Const(1)), Var("u"), Var("v")), 4, 5)

    val d6 = Call("fib", List(Sub(Var("z"), Const(2)), Var("v"), Var("v")), 6, 7)

    val d2 = IfThenElse(Condition(GT(Var("z"), Const(3)), 2), d3, Sequence(d4, d6))

    val d8 = Skip(8)

    val d9 = Call("fib", List(Var("x"), Const(0), Var("y")), 9, 10)

    //Procedure(name: String, formalArgs: List[FormalArgument], ln: Label, stmt: Stmt, lx: Label)
    val proc = Procedure("fib", List(FormalArgument("z",ByValue), FormalArgument("u",ByValue), FormalArgument("v",ByResult)), 1, Sequence(d1, Sequence(d2, d8)), 8)

    val p = WhileProgram(List(proc), Sequence(d9, d1))

    val g = CFGBuilder.flow(p)

    val expected: Set[(Int, Int)] =
      Set((1, 2)
        ,(2, 3)
        ,(3, 8)
        ,(2, 4)
        ,(4, 1)
        ,(8, 5)
        ,(5, 6)
        ,(6, 1)
        ,(8, 7)
        ,(7, 8)
        ,(9, 1)
        ,(8, 10))

    assert(expected == g)
  }
}
