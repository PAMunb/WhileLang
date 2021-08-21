package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

class AvailableExpressionTest extends AnyFunSuite {
  test("example 2.4") {
    val exprASumB = OpArith("Sum", Var("a"), Var("b"))
    val exprAMulB = OpArith("Mul", Var("a"), Var("b"))
    val exprASum1 = OpArith("Sum", Var("a"), Const(1))

    val d1 = Assignment("x", exprASumB, 1)
    val d2 = Assignment("y", exprAMulB, 2)
    val d4 = Assignment("a", exprASum1, 4)
    val d5 = Assignment("x", exprASumB, 5)
    val w3 = While(Condition(OpRelat("GT", Var("y"), exprASumB), 3), Sequence(d4, d5))

    val program = WhileProgram(Sequence(d1, Sequence(d2, w3)))
    val entry: Map[Int, Set[AExp]] = Map(
      1 -> Set.empty,
      2 -> Set(exprASumB),
      3 -> Set(exprASumB),
      4 -> Set(exprASumB),
      5 -> Set.empty
    )

    val exit: Map[Int, Set[AExp]] = Map(
      1 -> Set(exprASumB),
      2 -> Set(exprASumB, exprAMulB),
      3 -> Set(exprASumB),
      4 -> Set.empty,
      5 -> Set(exprASumB)
    )
    val expectedAbstraction = AvailableExpression.Abstraction(entry, exit)
    val abstraction = AvailableExpression.process(program)

    assert(abstraction == expectedAbstraction)
  }
}