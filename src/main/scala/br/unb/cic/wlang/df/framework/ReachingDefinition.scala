package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.{Assignment, Condition, Skip, WhileProgram}
import br.unb.cic.wlang.WhileProgram.{Label, assignments, block, fv}
import br.unb.cic.wlang.df.ReachingDefinition.undef

class ReachingDefinition(wp: WhileProgram) extends MFP[(String, Label)](wp) {

  override def lattice(): Lattice[(String, Label)] = Lattice(Union, Set[(String, Label)]())
  override def direction(): AnalysisDirection = ForwardAnalysis
  override def extremeValues(): Set[(String, Label)] = fv(wp.stmt).map(v => (v, undef))

  override def kill(label: Label): Set[(String, Label)] = block(label, wp).get match {
      case Assignment(x, _, label) => Set((x, undef)) union assignments(wp).filter({ case (v, l) => v == x && l != label })
      case Skip(_) => Set.empty
      case Condition(_, _) => Set.empty
    }

  override def gen(label: Label): Set[(String, Label)] =
    block(label, wp).get match {
      case Assignment(x, _, label) => Set((x, label))
      case Skip(_) => Set.empty
      case Condition(_, _) => Set.empty
    }
}
