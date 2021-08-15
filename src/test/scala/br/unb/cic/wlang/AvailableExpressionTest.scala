package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

class AvailableExpressionTest extends AnyFunSuite {

  /*
 * x := a+b;
 * y := a*b;
 * while y > a+b do
 *     a := a+l;
 *     x := a+b;
*/

  val s1 = Assignment("x", Add(Var("a"), Var("b")), 1)
  val s2 = Assignment("y", Mult(Var("a"), Var("b")), 2)
  val s4 = Assignment("a", Add("a", Const(1)), 4)
  val s5 = Assignment("x", Add("a", "b"), 5)
  val s3 = While(GT(Var("y"), Add("a", "b")), s2, 4)
  val p = WhileProgram(Sequence(s1, Sequence(s2, Sequence(s3, Sequence(s4, s5)))

}
