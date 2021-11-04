package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder.{CFG, InterCFG, flowR}
import br.unb.cic.wlang.cfg.PathBuilder.path

import scala.collection.mutable

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

  /**
   * We expect a valid path for the control flow graph `flow` and a
   * valid interFlow.
   */
  def completePath(aPath: Path, flow: CFG, interFlow: InterCFG): Boolean = {
    val first = aPath.head
    val last = aPath.last
    assert(path(first, last, flow).contains(aPath))
    completePath(aPath, flow, interFlow, new mutable.Stack[Label]())
  }

  def completePath(aPath: Path, flow: CFG, interFlow: InterCFG, stack: mutable.Stack[Label]): Boolean =
    aPath match {
      case List() if stack.isEmpty => true // base case + succeeded to recognize the path
      case List() if !stack.isEmpty => false // base case + failed to recognize the path
      case _ => { // the recursive case
        val callEdge = interFlow.find({ case (lc, _, _, _) => lc == aPath.head })
        val returnEdge = interFlow.find({ case (_, _, _, lr) => lr == aPath.head })
        if (callEdge.isDefined) { // if this is a call edge, we must "push" into the stack.
          val (_, _, _, lr) = callEdge.get
          return completePath(aPath.tail, flow, interFlow, stack.push(lr))
        }
        else if (returnEdge.isDefined) { // if this is a return edge, we must "pop" from the stack (if lr == stack.top)
          val (_, _, _, lr) = returnEdge.get
          if (lr == stack.top) {
            stack.pop() // removes the top element
            return completePath(aPath.tail, flow, interFlow, stack)
          } //if lr != top, we found an incomplete path.
          else return false
        } // otherwise, we just continue checking if a path is valid or not.
        else return completePath(aPath.tail, flow, interFlow, stack)
      }
    }
}
