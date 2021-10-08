package br.unb.cic.wlang.df

import br.unb.cic.wlang.cfg.CFGBuilder.flow
import br.unb.cic.wlang.WhileProgram._
import br.unb.cic.wlang._

import scala.collection.mutable

/*
 Implementation of the Available Expression algorithm.
*/
object AvailableExpression {

  type Abstraction = Set[(Exp)] //for AE the Abstraction is not (String,Label) like RD, it is AExp according to Page 38 of the PPA Book.
  type DS = mutable.HashMap[Int, Abstraction] //(label, Set[Exp])

  val empty: Abstraction = Set.empty

  def execute(program: WhileProgram): (DS, DS) = {
    var fixed = false

    // writing entry and exits as functions would be a possible
    // solution. nonetheless, since one depends on each other,
    // and the CFG might include cycles, a table-based implementation
    // avoids infinite loops.
    val entry: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
    val exit: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
    val nonTrivialExpressionSet: Set[Exp] = nonTrivialExpression(program)

    val bottom = nonTrivialExpressionSet

    // we need to initialize exit..., since we have
    // to first compute entry[l] from exit[l]. after
    // that, we recompute exit[l] from entry[l].
    for (label <- labels(program)) {
      exit(label) =
        bottom //for AE the meet operator is intersection so we initialize exit(label) with all non-trivial expressions of the program as largest solution
    }

    do {
      val entryOld = entry.clone()
      val exitOld = exit.clone()

      for (label <- labels(program)) {
        //println("(label): (" + label + ")")
        entry(label) =
          if (label == initLabel(program.stmt))
            empty //return empty para entry(1)
          else {
            // ⋂ { exit(from) | (from, to) <- flow(program) and to == label}
            // according to Table 2.1 on page 38 of the PPA book
            var res = bottom
            for ((from, to) <- flow(program) if to == label) { // | (ℓ',ℓ) ∈ flow(S*), sendo S* os stmt do program
              res = exit(from) intersect res // ⋂ { AExit(ℓ') } -> remember: {exit(from)} == entry(to)
              // println("(from,to) - exit(from) => res: (" + from + "," + to + ") - " + exit(from) + " => " + res)
            }
            res
          }
        val b = block(label, program) // block with a given label *label*
        exit(label) = (entry(label) diff kill(b.get, program)) union gen(b.get)
      }
      fixed = (entryOld, exitOld) == (entry, exit)
    } while (!fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.1 of the PPA book */
  // def kill(block: Block, program: WhileProgram): Set[Exp] = block match {
  def kill(block: Block, program: WhileProgram): Abstraction = block match {
    case Assignment(x, exp, label) => nonTrivialExpression(program).filter(exp => expHasVariable(x, exp)) //killAE({X := a}ℓ) = {a' ∈ AExp*, | x ∈ FV(a')}
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.1 of the PPA book */
  // def gen(block: Block): Set[Exp] = block match {
  def gen(block: Block): Abstraction = block match {
    case Assignment(x, exp, label) => nonTrivialExpression(exp).filterNot(exp => expHasVariable(x, exp)) //genAE({X := a}ℓ) = {a' ∈ AExp(a), | x ∉ FV(a')}
    case Skip(_) => Set.empty
    case Condition(b, _) => nonTrivialExpression(b) //genAE({b}ℓ) = AExp(b)
  }
}
