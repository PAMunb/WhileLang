package br.unb.cic.wlang.df.instances

import br.unb.cic.wlang.WhileProgram.{Label, block, fv}
import br.unb.cic.wlang.df.framework._
import br.unb.cic.wlang.{Assignment, Condition, Skip, WhileProgram}


abstract class LiveVariable(wp: WhileProgram) extends GenericFramework[String](wp) {
  override def lattice(): Lattice[String] = Lattice(Union, Set[String]())
  override def direction(): AnalysisDirection = BackwardAnalysis
  override def extremeValues(): Set[String] = Set.empty

  override def kill(label: Label): Set[String] = block(label, wp).get match {
    case Assignment(x, _, _) => Set(x)
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  override def gen(label: Label): Set[String] = block(label, wp).get match {
    case Assignment(_, a, _) => fv(a)
    case Skip(_) => Set.empty
    case Condition(b, _) => fv(b) 
  }

}
