package br.unb.cic.wlang

import scala.collection.mutable

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object ReachDefinition {

  type Abstraction = Set[(String, Int)]
  type DS = mutable.HashMap[Stmt, Abstraction]

  val empty: Abstraction = Set.empty

  def execute(program: WhileProgram): (DS, DS) = ???

  def assignments(stmt: Stmt): Set[(String, Int)] = stmt match {
    case Assignment(v, _, label) => Set((v, label))
    case Skip(_) => Set()
    case Sequence(s1, s2) => assignments(s1) union assignments(s2)
    case IfThenElse(_, s1, s2) => assignments(s1) union assignments(s2)
    case While(_, s) => assignments(s)
  }

  def kill(block: Block, stmt: Stmt): Abstraction = block match {
    case Assignment(v, _, _) => assignments(stmt).filter(tuple => tuple._1 == v)
    case _ => Set.empty
  }

  def gen(block: Block): Abstraction = block match {
    case Assignment(v, _, label) => Set((v,label))
    case _ => Set.empty
  }

}
