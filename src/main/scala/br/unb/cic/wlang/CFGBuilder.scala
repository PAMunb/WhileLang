package br.unb.cic.wlang

import WhileProgram._
/**
 * An Scala object responsible for building control
 * flow graphs from a While program.
 */
object CFGBuilder {
  type CFG = Set[(Int, Int)]

  /**
   * Builds a control flow graph from a given While program.
   *
   * @param program a While program
   *
   * @return The control-flow graph of the While program
   */
  def flow(program: WhileProgram): CFG = flow(program.stmt)

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
      case Sequence(s1, s2) => flow(s1) union flow(s2) union finalLabels(s1).map(from => (from, initLabel(s2)))
      case IfThenElse(Condition(_, label), s1, s2) =>
        flow(s1) union flow(s2) union Set((label, initLabel(s1)), (label, initLabel(s2)))
      case While(Condition(_, label), s) =>
        flow(s) union Set((label, initLabel(s))) union finalLabels(s).map(from => (from, label))
    }
  }

}
