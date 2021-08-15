package br.unb.cic.wlang
import scala.collection.mutable

/** Implementation of the Available Expression algorithm.
  */

object AvailableExpression {
//   TODO getNonTrivialExpressionSet
//   TODO setInitialValues = Emptyset
//   TODO Naive Chaotic Iteration on AvailableExpression.execute
  type Abstraction = Set[(String, Stmt)]
  type DS = mutable.HashMap[Stmt, Abstraction]

  val empty: Abstraction = Set.empty

  def execute(program: WhileProgram): (DS, DS) = {
    val cfg = CFGBuilder.build(program)
    val stmts = CFGBuilder.stmts(program)

    // initialization
    val universalSet = initUniversalSet(program.stmt)
    val out = initOutSet(stmts)
    val in = new DS()

    var fixedPoint = false

    // while (!fixedPoint) {} //TODO
    (in, out)       //é o return (DS, DS)
  }

  def initOutSet(stmts: Set[Stmt]): DS = { //inicializa os sets dos stmts como vazio? ok!
    val out = new DS()
    stmts.foreach(s => out += s -> empty)
    out
  }

  def initUniversalSet(stmt: Stmt): Abstraction = stmt match {
    case Assignment(v, _, _) => Set((v, stmt))
    case Skip(_)             => Set.empty
    case IfThenElse(_, s1, s2, _) =>
      initUniversalSet(s1) union initUniversalSet(s2)
    case While(_, s, _)   => initUniversalSet(s)
    case Sequence(s1, s2) => initUniversalSet(s1) union initUniversalSet(s2)
  }

  def kill(stmt: Stmt, universalSet: Abstraction): Abstraction = stmt match {
    case Assignment(v, _, _) => universalSet.filter(t => v == t._1)
    case _                   => Set.empty
  }

  def gen(stmt: Stmt): Abstraction = stmt match {
    case Assignment(v, _, _) => Set((v, stmt))
    case _                   => Set.empty
  }

}
