package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite
import scalax.collection.GraphEdge
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef.EdgeAssoc


class CFGBuilderBuilderTest extends AnyFunSuite {

  test("Test simple CFG") {
    val stmt = Assignment("x", Const(4), 1)
    val program = WhileProgram(stmt)
    val sn = SimpleNode(stmt)
    val g = CFGBuilder.build(program)

    val expected = Graph[GraphNode, GraphEdge.DiEdge]()

    expected += StartNode ~> sn
    expected += sn ~> EndNode

    assert(expected == g)
  }

  test("Test factorial CFG") {
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(GT(Var("y"), Const(1)), Sequence(d3, d4), 3)
    val d5 = Assignment("y", Const(0), 6)

    val p = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val g = CFGBuilder.build(p)

    val expected = Graph[GraphNode, GraphEdge.DiEdge]()

    expected += StartNode ~> SimpleNode(d1)
    expected += SimpleNode(d1) ~> SimpleNode(d2)
    expected += SimpleNode(d2) ~> SimpleNode(w1)
    expected += SimpleNode(w1) ~> SimpleNode(d3)
    expected += SimpleNode(d3) ~> SimpleNode(d4)
    expected += SimpleNode(d4) ~> SimpleNode(w1)
    expected += SimpleNode(w1) ~> SimpleNode(d5)
    expected += SimpleNode(d5) ~> EndNode

    assert(8 == g.nodes.size)
    assert(8 == g.edges.size)
    assert(expected == g)
  }

}
