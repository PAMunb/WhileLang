package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder.{CFG, InterCFG}

object PathBuilder {
  type Path = List[Label]

  def paths(target: Label, extremeLabels: Set[Label], flow: CFG): Set[Path] =
    extremeLabels.flatMap(from => path(from, target, flow))

  def path(from: Label, target: Label, flow: CFG): Set[Path] = path(from, target, flow, List())

  def path(from: Label, target: Label, flow: CFG, visited: List[Label], limit: Int = 3): Set[Path] = {
    var res: Set[Path] = if(from == target) Set(List(from)) else Set()

    val newVisited = from :: visited

    for((n, t) <- flow if (n == from) && (newVisited.filter(p => p == t).size < limit)) {
      res = res ++ path(t, target, flow, newVisited).map(path => from :: path)
    }
    res
  }


}
