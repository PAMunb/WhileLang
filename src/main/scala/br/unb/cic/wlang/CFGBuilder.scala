package br.unb.cic.wlang


/**
 * The root of the hierarchy of the
 * control-flow graph nodes.
 */
abstract class GraphNode


/**
 * Represents a real node from the function or program statements
 *
 * @param stmt the statement of a program or function
 */
case class Node(stmt: Stmt) extends GraphNode


/**
 * An Scala object responsible for building control
 * flow graphs from a While program.
 */
object CFGBuilder {
  type CFG = Set[(GraphNode, GraphNode)]
  /**
   * Builds a control flow graph from a given While program.
   *
   * @param program a While program
   *
   * @return The control-flow graph of the While program
   */
  def build(program: WhileProgram): CFG = flow(program.stmt)

  /*
   * The "core" of the algorithm for building
   * control-flow graphs. Here we use pattern
   * matching over the different statements.
   *
   * (for { from <- finalStmt(s1) } yield (from, initStatement(s2)))
   */
  private def flow(stmt: Stmt): CFG = {
    stmt match {
      case Assignment(_, _, _) => Set.empty
      case Skip(_) => Set.empty
      case Sequence(s1, s2) => flow(s1) union flow(s2) union finalStmt(s1).map(from => (from, initStmt(s2)))
      case IfThenElse(_, s1, s2, _) => flow(s1) union flow(s2) union Set((Node(stmt), initStmt(s1)), (Node(stmt), initStmt(s2)))
      case While(_, s, _) => flow(s) union Set((Node(stmt), initStmt(s))) union finalStmt(s).map(from => (from, Node(stmt)))
    }
  }

  /*
   * Returns the first statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def initStmt(stmt: Stmt) : GraphNode = stmt match {
    case Sequence(s1, _) => initStmt(s1)
    case _ => Node(stmt)
  }

  /*
   * Returns the last statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def finalStmt(stmt: Stmt) : Set[GraphNode] = stmt match {
    case Sequence(_, s2) => finalStmt(s2)
    case IfThenElse(_, thenStmt, elseStmt, _) => finalStmt(thenStmt) union finalStmt(elseStmt)
    case _ => Set(Node(stmt))
  }
}
