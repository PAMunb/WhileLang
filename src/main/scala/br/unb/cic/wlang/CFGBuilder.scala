package br.unb.cic.wlang

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
    * @return The control-flow graph of the While program
    */
  def build(program: WhileProgram): CFG = flow(program.stmt)

  /*
   * Returns the first label of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  def initLabel(stmt: Stmt): Int = stmt match {
    case Assignment(_, _, label) => label
    case Skip(label) => label
    case Sequence(s1, _) => initLabel(s1)
    case IfThenElse(Condition(_, label), _, _) => label
    case While(Condition(_, label), _) => label
  }

  /*
   * Returns the set of last labels of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  def finalLabel(stmt: Stmt): Set[Int] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(_, s2) => finalLabel(s2)
    case IfThenElse(_, s1, s2) => finalLabel(s1) union finalLabel(s2)
    case While(Condition(_, label), _) => Set(label)
  }

  /*
   * The "core" of the algorithm for building
   * control-flow graphs. Here we use pattern
   * matching over the different statements.
   *
   * (for { from <- finalStmt(s1) } yield (from, initStatement(s2)))
   */
  def flow(stmt: Stmt): CFG =
    stmt match {
      case Assignment(_, _, _) => Set.empty
      case Skip(_) => Set.empty
      case Sequence(s1, s2) =>
        flow(s1) union flow(s2) union finalLabel(s1).map(from => (from, initLabel(s2)))
      case IfThenElse(Condition(_, l), s1, s2) =>
        flow(s1) union flow(s2) union Set((l, initLabel(s1)), (l, initLabel(s2)))
      case While(Condition(_, l), s) =>
        flow(s) union Set((l, initLabel(s))) union finalLabel(s).map(from => (from, l))
    }
}
