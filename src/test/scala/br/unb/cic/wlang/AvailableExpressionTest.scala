package br.unb.cic.wlang
import br.unb.cic.wlang.df.AvailableExpression
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
  val s4 = Assignment("a", Add(Var("a"), Const(1)), 4)
  val s5 = Assignment("x", Add(Var("a"), Var("b")), 5)
  val s3 =
    While(Condition(GT(Var("y"), Add(Var("a"), Var("b"))), 3), Sequence(s4, s5))

  val p = WhileProgram(List(),
    Sequence(s1, Sequence(s2, Sequence(s3, Sequence(s4, s5))))
  )

  test("Test case for Available Expression") {
    val (in, out) = AvailableExpression.execute(p)

    assert(in(1) == Set.empty)
    assert(out(1) == Set(Add(Var("a"), Var("b"))))

    assert(in(2) == Set(Add(Var("a"), Var("b"))))
    assert(out(2) == Set(Add(Var("a"), Var("b")), Mult(Var("a"), Var("b"))))

    assert(in(3) == Set(Add(Var("a"), Var("b"))))
    assert(out(3) == Set(Add(Var("a"), Var("b"))))

    assert(in(4) == Set(Add(Var("a"), Var("b"))))
    assert(out(4) == Set.empty)

    assert(in(5) == Set.empty)
    assert(out(5) == Set(Add(Var("a"), Var("b"))))

  }

}
