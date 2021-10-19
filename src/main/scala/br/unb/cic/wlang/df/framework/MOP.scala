package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.PathBuilder.Path
import br.unb.cic.wlang.cfg.PathBuilder.paths

import scala.collection.mutable

abstract class MOP[Abstraction](wp: WhileProgram) extends GenericFramework[Abstraction](wp) {

  override def execute(): (Result, Result) = {
    val flow  = buildControlFlowGraph()
    val nodes: Set[Label] = flow.flatMap({ case (a,b) => List(a, b) } )
    val allPaths: Map[Label, Set[Path]] = nodes.map(label => (label, paths(label, findExtremeLabels(), flow))).toMap

    var mop2 : Result = new mutable.HashMap()

    for((label, paths) <- allPaths) {
      var res : Set[Abstraction] = lattice().bottom
      for(p <- paths) {
        res = lattice().meetOperator(res, transferFunctionOverPath(extremeValues(), p))
      }
      mop2 += label -> res
    }

    (mop2, mop2)
  }

  def transferFunctionOverPath(analysis: Set[Abstraction], path: List[Label]): Set[Abstraction] = {
    var res: Set[Abstraction] = analysis
    for (label <- path) {
      res = transferFunction(res, label)
    }
    res
  }

}
