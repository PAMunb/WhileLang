package br.unb.cic.wlang

import scalax.collection.mutable.Graph
import scalax.collection.GraphEdge
import scalax.collection.GraphPredef.EdgeAssoc

/**
 * The root of the hierarchy of the
 * control-flow graph nodes.
 */
abstract class GraphNode

/**
 * Represents the start node of a control-flow
 * graph.
 */
case object StartNode extends GraphNode

/**
 * Represents a real node from the function or program statements
 *
 * @param stmt the statement of a program or function
 */
case class SimpleNode(stmt: Stmt) extends GraphNode

/**
 * Represents an end node of a control-flow
 * graph.
 */
case object EndNode extends GraphNode

/**
 * An Scala object responsible for building control
 * flow graphs from a While program.
 */
object CFGBuilder {
  /**
   * Builds a control flow graph from a given While program.
   *
   * @param program a While program
   *
   * @return The control-flow graph of the While program
   */
  def build(program: WhileProgram): Graph[GraphNode, GraphEdge.DiEdge] = {
    val g = Graph[GraphNode, GraphEdge.DiEdge]()
    build(g, program.stmt, StartNode, EndNode)
  }

  /*
   * The "core" of the algorithm for building
   * control-flow graphs. Here we use pattern
   * matching over the different statements.
   */
  private def build(g:Graph[GraphNode, GraphEdge.DiEdge], stmt: Stmt, currentNode: GraphNode, targetNode: GraphNode): Graph[GraphNode, GraphEdge.DiEdge] = {
    stmt match {
      case Assignment(_, _, _) =>
        val newNode = SimpleNode(stmt)
        g += currentNode ~> newNode
        g += newNode ~> targetNode
      case IfThenElse(_, s1, s2, _) =>
        val newNode = SimpleNode(stmt)
        g += currentNode ~> newNode
        build(build(g, s1, newNode, targetNode), s2, newNode, targetNode)
      case While(_, s, _) =>
        val newNode = SimpleNode(stmt)
        g += currentNode ~> newNode
        g += newNode ~> targetNode
        build(g, s, newNode, newNode)
      case Skip(_) =>
        val newNode = SimpleNode(stmt)
        g += currentNode ~> newNode
        g += newNode ~> targetNode
      case Sequence(s1, s2) =>
        build(build(g, s1, currentNode, first(s2)), s2, last(s1), targetNode)
    }
  }

  /*
   * Returns the first statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def first(stmt: Stmt) : GraphNode = stmt match {
    case Sequence(s1, _) => first(s1)
    case _ => SimpleNode(stmt)
  }

  /*
   * Returns the last statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def last(stmt: Stmt) : GraphNode = stmt match {
    case Sequence(_, s2) => last(s2)
    case _ => SimpleNode(stmt)
  }
}
