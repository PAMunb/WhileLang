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
   *
   * @return The control-flow graph of the While program
   */
  def build(program: WhileProgram): CFG = flow(program.stmt)




def blocks(stmt: Stmt) : Set[Block] = stmt match {
  case Assignment(v, e, label) => Set(Assignment(v, e, label))
  case Skip(label) => Set(Skip(label))
  case Sequence(s1, s2) => blocks(s1) union blocks(s2)
  case IfThenElse(c, s1, s2) => blocks(s1) union blocks(s2) union Set(c):Set[Block]
  case While(c, s) => blocks(s) union Set(c)
}

  // def labels(stmt: Stmt): Set[Int] = blocks(stmt).map(b => b.label)

  def labels(stmt: Stmt): Set[Int] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(s1, s2) => labels(s1) union labels(s2)
    case IfThenElse(c, s1, s2) => Set(c.label) union labels(s1) union labels(s2)
    case While(c, s) => Set(c.label) union labels(s)
  }


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

  /*
   * Returns the first statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
   def initLabel(stmt: Stmt) : Int = stmt match {
    case Assignment(_, _, label) => label
    case Skip(label) => label
    case Sequence(s1, _) => initLabel(s1)
    case IfThenElse(Condition(_, label), _, _)  => label
    case While(Condition(_, label),_) => label
  }

  /*
   * Returns the last statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
   def finalLabels(stmt: Stmt) : Set[Int] = stmt match {
     case Assignment(_, _, label) => Set(label)
     case Skip(label) => Set(label)
     case Sequence(_, s2) => finalLabels(s2)
     case IfThenElse(_, s1, s2) => finalLabels(s1) union finalLabels(s2)
     case While(Condition(_, label), _) => Set(label)
   }
}
