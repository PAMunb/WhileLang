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
    val d3 = Assignment("z", OpArith("Mul", Var("z"), Var("y")), 4)
    val d4 = Assignment("y", OpArith("Sub", Var("y"), Const(1)), 5)
    val w1 = While(Condition(OpRelat("GT",Var("y"), Const(1)), 3), Sequence(d3, d4))
    val d5 = Assignment("y", Const(0), 6)

    val program = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val resultCFG = CFGBuilder.build(program)

    val expectedCFG =
      Set((d1.label, d2.label)
        , (d2.label, w1.condition.label)
        , (w1.condition.label, d3.label)
        , (d3.label, d4.label)
        , (d4.label, w1.condition.label)
        , (w1.condition.label, d5.label))

    assert(expectedCFG == resultCFG)
  }
  test("Test AE book CFG") {
    val exprASumB = OpArith("Sum", Var("a"), Var("b"))
    val exprAMulB = OpArith("Mul", Var("a"), Var("b"))
    val exprASum1 = OpArith("Sum", Var("a"), Const(1))

    val d1 = Assignment("x", exprASumB, 1)
    val d2 = Assignment("y", exprAMulB, 2)
    val d4 = Assignment("a", exprASum1, 4)
    val d5 = Assignment("x", exprASum1, 5)
    val w3 = While(Condition(OpRelat("GT", Var("y"), exprASumB), 3), Sequence(d4, d5))

    val program = WhileProgram(Sequence(d1, Sequence(d2, w3)))

    val resultCFG = CFGBuilder.build(program)

    val expectedCFG =
      Set((d1.label, d2.label)
        , (d2.label, w3.condition.label)
        , (w3.condition.label, d4.label)
        , (d4.label, d5.label)
        , (d5.label, w3.condition.label))

    assert(expectedCFG == resultCFG)
  }
  test("Build reverse CFG example 2.8") {
    val exprAGTB = OpRelat("GT", Var("a"), Var("b"))
    val exprBMinusA = OpArith("Minus",Var("b"), Var("a"))
    val exprAMinusB = OpArith("Minus",Var("a"), Var("b"))

    val program =
      WhileProgram(
        IfThenElse(
          Condition(exprAGTB,1),
          Sequence(Assignment("x", exprBMinusA, 2), Assignment("y", exprAMinusB, 3)),
          Sequence(Assignment("y", exprBMinusA,4), Assignment("x", exprAMinusB, 5))
        )
      )


    val resultCFG = CFGBuilder.buildR(program)

    val expectedCFG =
      Set(
        (5, 4)
        ,(3, 2)
        ,(2, 1)
        ,(4, 1)
      )

    assert(expectedCFG == resultCFG)
  }

}
