package br.unb.cic.wlang.df

import br.unb.cic.wlang.df.framework.{ AvailableExpression => MFPAE }
import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class AvailableExpressionTest extends AnyFunSuite {
  /*
   * x := a+b;
   * y := a*b;
   * while y > a+b do
   *     a := a+l;
   *     x := a+b;
   */
  val s1 = Assignment("x", Add(Variable("a"), Variable("b")), 1)
  val s2 = Assignment("y", Mult(Variable("a"), Variable("b")), 2)
  val s4 = Assignment("a", Add(Variable("a"), Const(1)), 4)
  val s5 = Assignment("x", Add(Variable("a"), Variable("b")), 5)
  val s3 =
    While(Condition(GT(Variable("y"), Add(Variable("a"), Variable("b"))), 3), Sequence(s4, s5))

  val p = WhileProgram(List(),
    Sequence(s1, Sequence(s2, Sequence(s3, Sequence(s4, s5))))
  )

  test("Test case for Available Expression") {
    val (in, out) = AvailableExpression.execute(p)

    assert(in(1) == Set.empty)
    assert(out(1) == Set(Add(Variable("a"), Variable("b"))))

    assert(in(2) == Set(Add(Variable("a"), Variable("b"))))
    assert(out(2) == Set(Add(Variable("a"), Variable("b")), Mult(Variable("a"), Variable("b"))))

    assert(in(3) == Set(Add(Variable("a"), Variable("b"))))
    assert(out(3) == Set(Add(Variable("a"), Variable("b"))))

    assert(in(4) == Set(Add(Variable("a"), Variable("b"))))
    assert(out(4) == Set.empty)

    assert(in(5) == Set.empty)
    assert(out(5) == Set(Add(Variable("a"), Variable("b"))))

  }

  test("Test case for the MFP implementation of Available Expressions") {

    val mfp = new MFPAE(p)

    val (mfp1, mfp2) = mfp.execute()

    assert(mfp1 != null)
    assert(mfp2 != null)

    print(mfp2)

    assert(mfp1(1) == Set.empty)
    assert(mfp2(1) == Set(Add(Variable("a"), Variable("b"))))

    assert(mfp1(2) == Set(Add(Variable("a"), Variable("b"))))
    assert(mfp2(2) == Set(Add(Variable("a"), Variable("b")), Mult(Variable("a"), Variable("b"))))

    assert(mfp1(3) == Set(Add(Variable("a"), Variable("b"))))
    assert(mfp2(3) == Set(Add(Variable("a"), Variable("b"))))

    assert(mfp1(4) == Set(Add(Variable("a"), Variable("b"))))
    assert(mfp2(4) == Set.empty)

    assert(mfp1(5) == Set.empty)
    assert(mfp2(5) == Set(Add(Variable("a"), Variable("b"))))

  }


}
