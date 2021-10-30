package br.unb.cic.wlang.cfg

import br.unb.cic.wlang.WhileProgram.Label
import br.unb.cic.wlang.cfg.CFGBuilder.{CFG, InterCFG, flowR}

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

  def completePath(path: Path, flow: CFG, interFlow: InterCFG): Boolean =
    completePath(path, flow, interFlow, new mutable.Stack[Label]())

  def completePath(path: Path, flow: CFG, interFlow: InterCFG, stack: mutable.Stack[Label]): Boolean = {
    // this is the base cases of the recursion.
    // in this case, we have recognized a path as being complete
    if(path.size == 0 && stack.isEmpty)        return true
    else if (path.size == 0 && !stack.isEmpty) return false
    else {
      // here we check if the label is either a call or a return, making
      // use of the interflow.
      val callEdge = interFlow.find({case (lc, _, _, _) => lc == path.head })
      val returnEdge = interFlow.find({case (_, _, _, lr) => lr == path.head })
      if(callEdge.isDefined) { // if this is a call edge, we must "push" into the stack.
        val (_, _, _, lr) = callEdge.get
        return completePath(path.tail, flow, interFlow, stack.push(lr))
      }
      else if(returnEdge.isDefined) { // if this is a return edge, we must "pop" from the stack (if lr == stack.top)
        val (_, _, _, lr) = returnEdge.get
        if(lr == stack.top) {
          stack.pop() // removes the top element
          return completePath(path.tail, flow, interFlow, stack)
        } //if lr != top, we found an incomplete path.
        else return false
      } // otherwise, we just continue checking if a path is valid or not.
      else return completePath(path.tail, flow, interFlow, stack)
    }
  }


}
