package br.unb.cic.wlang
import br.unb.cic.wlang.df.VeryBusyExpression
import org.scalatest.funsuite.AnyFunSuite

class VeryBusyExpressionTest extends AnyFunSuite {
/* if a>b then
     x := b-a
     y := a-b
   else
     y := b-a
     x := a-b  
 */  
  val s2 = Assignment("x", Sub(Var("b"), Var("a")), 2)
  val s3 = Assignment("y", Sub(Var("a"), Var("b")), 3)
  val s4 = Assignment("y", Sub(Var("b"), Var("a")), 4)
  val s5 = Assignment("x", Sub(Var("a"), Var("b")), 5)
  val s1 = IfThenElse(Condition(GT(Var("a"), Var("b")), 1), Sequence(s2, s3), Sequence(s4, s5))

  val p = WhileProgram(List(), Sequence(s1, s1))  // to define this program as a sequence of statements, s1 must come after s1
  // val p = WhileProgram(s1)

  test("Test case for Very Busy Expression") {
    val (in, out) = VeryBusyExpression.execute(p)

    assert(in(1) == Set(Sub(Var("a"), Var("b")), Sub(Var("b"), Var("a"))))
    assert(out(1) == Set(Sub(Var("a"), Var("b")), Sub(Var("b"), Var("a"))))

    assert(in(2) == Set(Sub(Var("a"), Var("b")), Sub(Var("b"), Var("a"))))
    assert(out(2) == Set(Sub(Var("a"), Var("b"))))

    assert(in(3) == Set(Sub(Var("a"), Var("b"))))
    assert(out(3) == Set.empty)

    assert(in(4) == Set(Sub(Var("a"), Var("b")), Sub(Var("b"), Var("a"))))
    assert(out(4) == Set(Sub(Var("a"), Var("b"))))

    assert(in(5) == Set(Sub(Var("a"), Var("b"))))
    assert(out(5) == Set.empty)

  }

}
