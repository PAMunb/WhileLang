package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.{Assignment, Condition, Skip, WhileProgram, Exp}
import br.unb.cic.wlang.WhileProgram.{Label, block, nonTrivialExpression, expHasVariable}


class VeryBusyExpressionMOP(wp: WhileProgram) extends MFP[(Exp)](wp) {

  override def lattice(): Lattice[(Exp)] = Lattice(Intersection, nonTrivialExpression(wp))
  override def direction(): AnalysisDirection = BackwardAnalysis
  override def extremeValues(): Set[(Exp)] = Set.empty

  override def kill(label: Label): Set[(Exp)] = block(label, wp).get match {
    case Assignment(x, exp, label) => nonTrivialExpression(wp).filter(exp => expHasVariable(x, exp)) 
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  override def gen(label: Label): Set[(Exp)] = block(label, wp).get match {
      case Assignment(x, a, label) => nonTrivialExpression(a) 
      case Skip(_) => Set.empty
      case Condition(b, _) => nonTrivialExpression(b)
  }

}
