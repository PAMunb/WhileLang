package br.unb.cic.wlang.df

import br.unb.cic.wlang.df.framework.{ LiveVariable => MFPLV }
import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class LiveVariableTest extends  AnyFunSuite {
/*  
 x := 2; 
 y := 4;
 x := 1;
 if y>x then
    z := y
 else
    z := y * y;
 x := z; 
 */
    val s1 = Assignment("x", Const(2), 1)
    val s2 = Assignment("y", Const(4), 2)
    val s3 = Assignment("x", Const(1), 3)
    val s5 = Assignment("z", Variable("y"), 5)
    val s6 = Assignment("z", Mult(Variable("y"), Variable("y")), 6)
    val s4 = IfThenElse(Condition(GT(Variable("y"), Variable("x")), 4), s5, s6)
    val s7 = Assignment("x", Variable("z"), 7)

    val p = WhileProgram(List(),
        Sequence(s1, Sequence(s2, Sequence(s3, Sequence(s4, s7))))
    )

  test("Test case for Live Variable") {
    val (in, out) = LiveVariable.execute(p)

    assert(in(1) == Set.empty)
    assert(out(1) == Set.empty)

    assert(in(2) == Set.empty)
    assert(out(2) == Set("y"))

    assert(in(3) == Set("y"))
    assert(out(3) == Set("x", "y")) //== Set("x") union Set("y")) 

    assert(in(4) == Set("x", "y")) //== Set("x") union Set("y")) 
    assert(out(4) == Set("y"))

    assert(in(5) == Set("y"))
    assert(out(5) == Set("z"))

    assert(in(6) == Set("y"))
    assert(out(6) == Set("z"))

    assert(in(7) == Set("z"))
    assert(out(7) == Set.empty)

  }

   test("Test case for the MFP implementation of Live Variables") {

    val mfp = new MFPLV(p)

    val (mfp1, mfp2) = mfp.execute()

    assert(mfp1 != null)
    assert(mfp2 != null)

    assert(mfp2(1) == Set.empty)
    assert(mfp1(1) == Set.empty)

    assert(mfp2(2) == Set.empty)
    assert(mfp1(2) == Set("y"))

    assert(mfp2(3) == Set("y"))
    assert(mfp1(3) == Set("x", "y")) //== Set("x") union Set("y")) 

    assert(mfp2(4) == Set("x", "y")) //== Set("x") union Set("y")) 
    assert(mfp1(4) == Set("y"))

    assert(mfp2(5) == Set("y"))
    assert(mfp1(5) == Set("z"))

    assert(mfp2(6) == Set("y"))
    assert(mfp1(6) == Set("z"))

    assert(mfp2(7) == Set("z"))
    assert(mfp1(7) == Set.empty)

 }



}
