package br.unb.cic.wlang



/**
 * An Scala object responsible for building control
 * flow graphs from a While program.
 */
object CFGBuilder {
  type CFG = Set[(Stmt, Stmt)]
  /**
   * Builds a control flow graph from a given While program.
   *
   * @param program a While program
   *
   * @return The control-flow graph of the While program
   */
  def build(program: WhileProgram): CFG = flow(program.stmt)

  def stmts(program: WhileProgram): Set[Stmt] = stmts(program.stmt)
  def stmts(stmt: Stmt): Set[Stmt] = stmt match {
    case Assignment(_, _, _) => Set(stmt)
    case Skip(_) => Set(stmt)
    case While(_, s, _) => Set(stmt) union stmts(s)
    case IfThenElse(_, s1, s2, _) => Set(stmt) union (stmts(s1) union stmts(s2))
    case Sequence(s1, s2) => stmts(s1) union stmts(s2)
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
      case Sequence(s1, s2) => flow(s1) union flow(s2) union finalStmt(s1).map(from => (from, initStmt(s2)))
      case IfThenElse(_, s1, s2, _) => flow(s1) union flow(s2) union Set((stmt, initStmt(s1)), (stmt, initStmt(s2)))
      case While(_, s, _) => flow(s) union Set((stmt, initStmt(s))) union finalStmt(s).map(from => (from, stmt))
    }
  }

  /*
   * Returns the first statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
   def initStmt(stmt: Stmt) : Stmt = stmt match {
    case Sequence(s1, _) => initStmt(s1)
    case _ => stmt
  }

  /*
   * Returns the last statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
   def finalStmt(stmt: Stmt) : Set[Stmt] = stmt match {
    case Sequence(_, s2) => finalStmt(s2)
    case IfThenElse(_, thenStmt, elseStmt, _) => finalStmt(thenStmt) union finalStmt(elseStmt)
    case _ => Set(stmt)
  }
}
