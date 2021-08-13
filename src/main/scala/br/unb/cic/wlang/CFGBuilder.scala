package br.unb.cic.wlang


/**
 * An Scala object responsible for building control
 * flow graphs from a While program.
 */
object CFGBuilder {
  type Label = Int
  type Block = (Stmt, Label)
  type CFG = Set[(Label, Label)]
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
  private def flow(stmt: Stmt): CFG =
    stmt match {
      case Assignment(_, _, _) => Set.empty
      case Skip(_) => Set.empty
      case Sequence(s1, s2) => 
        flow(s1) union flow(s2) union finalLabel(s1).map(from => (from, initLabel(s2)))
      case IfThenElse(_, s1, s2, l) => 
        flow(s1) union flow(s2) union Set((l, initLabel(s1)), (l, initLabel(s2)))
      case While(_, s, l) => 
        flow(s) union Set((l, initLabel(s))) union finalLabel(s).map(from => (from, l))
    }
  

  /*
   * Returns the first label of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def initLabel(stmt: Stmt) : Label = stmt match {
    case Assignment(_,_, label) => label
    case Skip(label) => label
    case Sequence(s1, _) => initLabel(s1)
    case IfThenElse(_,_, _, label) => label
    case While(_,_, label) => label
  }

  /*
   * Returns the set of last labels of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  private def finalLabel(stmt: Stmt) : Set[Label] = stmt match {
    case Assignment(_,_, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(_, s2) => finalLabel(s2)
    case IfThenElse(_,s1, s2, _) => finalLabel(s1) union finalLabel(s2)
    case While(_,_, label) => Set(label)
  }
  

  /**
    * Returns the set of elementary blocks given a statement
    * 
    * @see Section 2.1 of Principles of Program Analysis
    */
  private def blocks(stmt: Stmt) : Set[Block] = stmt match {
    case Assignment(name, exp, label) => Set((stmt, label))
    case Skip(label) => Set((stmt, label))
    case Sequence(s1, s2) => blocks(s1) union blocks(s2)
    case IfThenElse(condition, thenStmt, elseStmt, label) => 
      Set((stmt, label)) union blocks(thenStmt) union blocks(elseStmt)
    case While(condition, stmt, label) => Set((stmt, label)) union blocks(stmt)
  }

  /**
    * Returns the set of labels occurring in a program
    * 
    * @see Section 2.1 of Principles of Program Analysis
    */
  private def labels(stmt: Stmt): Set[Label] = blocks(stmt).map(_._2)
}
