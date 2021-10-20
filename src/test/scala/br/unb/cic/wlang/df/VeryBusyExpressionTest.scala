package br.unb.cic.wlang.df

import br.unb.cic.wlang.df.framework.{ VeryBusyExpression => MFPVBE, VeryBusyExpressionMOP => MOPVBE}
import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class VeryBusyExpressionTest extends AnyFunSuite {
/* if a>b then
     x := b-a
     y := a-b
   else
     y := b-a
     x := a-b  
 */  
  val s2 = Assignment("x", Sub(Variable("b"), Variable("a")), 2)
  val s3 = Assignment("y", Sub(Variable("a"), Variable("b")), 3)
  val s4 = Assignment("y", Sub(Variable("b"), Variable("a")), 4)
  val s5 = Assignment("x", Sub(Variable("a"), Variable("b")), 5)
  val s1 = IfThenElse(Condition(GT(Variable("a"), Variable("b")), 1), Sequence(s2, s3), Sequence(s4, s5))

  val p = WhileProgram(List(), Sequence(s1, s1))  // to define this program as a sequence of statements, s1 must come after s1
  // val p = WhileProgram(s1)

  test("Test case for Very Busy Expression") {
    val (in, out) = VeryBusyExpression.execute(p)

    assert(in(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(out(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))

    assert(in(2) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(out(2) == Set(Sub(Variable("a"), Variable("b"))))

    assert(in(3) == Set(Sub(Variable("a"), Variable("b"))))
    assert(out(3) == Set.empty)

    assert(in(4) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(out(4) == Set(Sub(Variable("a"), Variable("b"))))

    assert(in(5) == Set(Sub(Variable("a"), Variable("b"))))
    assert(out(5) == Set.empty)

  }

  test("Test case for the MFP implementation of Very Busy Expressions") {

    val mfp = new MFPVBE(p)

    val (mfp1, mfp2) = mfp.execute()

    assert(mfp1 != null)
    assert(mfp2 != null)

    assert(mfp2(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mfp1(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))

    assert(mfp2(2) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mfp1(2) == Set(Sub(Variable("a"), Variable("b"))))

    assert(mfp2(3) == Set(Sub(Variable("a"), Variable("b"))))
    assert(mfp1(3) == Set.empty)

    assert(mfp2(4) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mfp1(4) == Set(Sub(Variable("a"), Variable("b"))))

    assert(mfp2(5) == Set(Sub(Variable("a"), Variable("b"))))
    assert(mfp1(5) == Set.empty)
  }

    test("Test case for the MOP implementation of Very Busy Expressions") {

    val mop = new MOPVBE(p)

    val (mop1, mop2) = mop.execute()

    assert(mop1 != null)
    assert(mop2 != null)

    assert(mop2(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mop1(1) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))

    assert(mop2(2) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mop1(2) == Set(Sub(Variable("a"), Variable("b"))))

    assert(mop2(3) == Set(Sub(Variable("a"), Variable("b"))))
    assert(mop1(3) == Set.empty)

    assert(mop2(4) == Set(Sub(Variable("a"), Variable("b")), Sub(Variable("b"), Variable("a"))))
    assert(mop1(4) == Set(Sub(Variable("a"), Variable("b"))))

    assert(mop2(5) == Set(Sub(Variable("a"), Variable("b"))))
    assert(mop1(5) == Set.empty)
  }
}
