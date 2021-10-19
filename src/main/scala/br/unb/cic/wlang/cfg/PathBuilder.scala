package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder.CFG

object PathBuilder {

  type Path = List[Label]

  def paths(target: Label, extremeLabels: Set[Label], flow: CFG): Set[Path] =
    extremeLabels.flatMap(from => path(from, target, flow))

  def path(from: Label, target: Label, flow: CFG): Set[Path] = path(from, target, flow, List(), List())

  def path(from: Label, target: Label, flow: CFG, visiting: List[Label], finished: List[Label]): Set[Path] = {
    var res: Set[Path] = if(from == target) Set(List(from)) else Set()

    val newVisiting = if(!visiting.contains(from)) from :: visiting else visiting
    val newFinished = if(visiting.contains(from)) from:: finished else finished

    for((n, t) <- flow if (n == from) && !newFinished.contains(t)) {
      res = res ++ path(t, target, flow, newVisiting, newFinished).map(path => from :: path)
    }
    res
  }


}
