package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

//class ReachDefinitionTest extends  AnyFunSuite {

//  val d1 = Assignment("y", Var("x"), 1)
//  val d2 = Assignment("z", Const(1), 2)
//  val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
//  val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
//  val w1 = While(GT(Var("y"), Const(1)), Sequence(d3, d4), 3)
//  val d5 = Assignment("y", Const(0), 6)
//
//  val p = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))
//
//  test("Test case for init universe") {
//    val expected = Set(
//        ("y", d1),
//        ("z", d2),
//        ("z", d3),
//        ("y", d4),
//        ("y", d5))
//    assert(expected == ReachDefinition.initUniversalSet(p.stmt))
//  }
//
//  test("Test case for reach definition") {
//    val (in, out) = ReachDefinition.execute(p)
//
//    assert(in(d1) == Set.empty)
//    assert(out(d1) == Set(("y", d1)))
//
//    assert(in(d2) == Set(("y", d1)))
//    assert(out(d2) == Set(("y", d1), ("z", d2)))
//
//    assert(in(w1) == Set(("y", d1), ("z", d2), ("z", d3), ("y", d4)))
//    assert(out(w1) == Set(("y", d1), ("z", d2), ("z", d3), ("y", d4)))
//
//    assert(in(d3) == Set(("y", d1), ("z", d2), ("z", d3), ("y", d4)))
//    assert(out(d3) == Set(("y", d1), ("z", d3), ("y", d4)))
//
//    assert(in(d4) == Set(("y", d1), ("z", d3), ("y", d4)))
//    assert(out(d4) == Set(("z", d3), ("y", d4)))
//
//    assert(in(d5) == Set(("y", d1), ("z", d2), ("z", d3), ("y", d4)))
//    assert(out(d5) == Set(("z", d2), ("z", d3), ("y", d5)))

//  }
//
//}
