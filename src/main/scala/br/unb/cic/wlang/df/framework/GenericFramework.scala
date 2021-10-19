package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.WhileProgram.{Label, finalLabels, initLabel}
import br.unb.cic.wlang.cfg.CFGBuilder
import br.unb.cic.wlang.cfg.CFGBuilder.CFG

import scala.collection.mutable

trait AnalysisDirection

case object ForwardAnalysis extends AnalysisDirection
case object BackwardAnalysis extends AnalysisDirection

abstract class GenericFramework[Abstraction](wp: WhileProgram) {

  type Result = mutable.Map[Label, Set[Abstraction]]

  def execute(): (Result, Result)

  def transferFunction(analysis: Set[Abstraction], label: Label): Set[Abstraction] =
    (analysis diff kill(label)) union gen(label)

  def buildControlFlowGraph(): CFG = direction() match {
    case ForwardAnalysis => CFGBuilder.flow(wp)
    case BackwardAnalysis => CFGBuilder.flowR(wp)
  }

  def findExtremeLabels() : Set[Label] = direction() match {
    case ForwardAnalysis => Set(initLabel(wp))
    case BackwardAnalysis => finalLabels(wp)
  }

  /* these abstract definitions correspond to the 'hot spots' of our framework */
  def kill(label: Label): Set[Abstraction]
  def gen(label: Label): Set[Abstraction]

  def lattice(): Lattice[Abstraction]
  def direction(): AnalysisDirection
  def extremeValues(): Set[Abstraction]

}
