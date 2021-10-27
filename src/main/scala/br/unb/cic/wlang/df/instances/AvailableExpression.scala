package br.unb.cic.wlang.df.instances

import br.unb.cic.wlang.WhileProgram.{Label, block, expHasVariable, nonTrivialExpression}
import br.unb.cic.wlang.df.framework._
import br.unb.cic.wlang._


abstract class AvailableExpression(wp: WhileProgram) extends GenericFramework[Exp](wp) {
  override def lattice(): Lattice[Exp] = Lattice(Intersection, nonTrivialExpression(wp))
  override def direction(): AnalysisDirection = ForwardAnalysis
  override def extremeValues(): Set[Exp] = Set.empty

  override def kill(label: Label): Set[Exp] = block(label, wp).get match {
    case Assignment(x, _, _) => nonTrivialExpression(wp).filter(exp => expHasVariable(x, exp))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  override def gen(label: Label): Set[(Exp)] =
    block(label, wp).get match {
      case Assignment(x, exp, _) => nonTrivialExpression(exp).filterNot(exp => expHasVariable(x, exp))
      case Skip(_) => Set.empty
      case Condition(b, _) => nonTrivialExpression(b) 
  }

}
