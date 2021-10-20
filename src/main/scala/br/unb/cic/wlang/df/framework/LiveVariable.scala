package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.{Assignment, Condition, Skip, WhileProgram}
import br.unb.cic.wlang.WhileProgram.{Label, block, fv}


class LiveVariable(wp: WhileProgram) extends MFP[String](wp) {

  override def lattice(): Lattice[String] = Lattice(Union, Set[String]())
  override def direction(): AnalysisDirection = BackwardAnalysis
  override def extremeValues(): Set[String] = Set.empty

  override def kill(label: Label): Set[String] = block(label, wp).get match {
    case Assignment(x, exp, label) => Set(x)
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  override def gen(label: Label): Set[String] = block(label, wp).get match {
    case Assignment(x, a, label) => fv(a)
    case Skip(_) => Set.empty
    case Condition(b, _) => fv(b) 
  }

}
