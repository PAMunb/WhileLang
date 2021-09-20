package br.unb.cic.wlang.df

import br.unb.cic.wlang.CFGBuilder.flowR
import br.unb.cic.wlang.WhileProgram.{block, finalLabels, fv, labels}
import br.unb.cic.wlang._

import scala.collection.mutable

/*
 Implementation of the Live Variable algorithm.
*/
object LiveVariable {
  type Abstraction = Set[String] //for LV the Abstraction is a set of String,Label such that String is the variable of interest
  type DS = mutable.HashMap[Int, Abstraction]

  val bottom: Abstraction = Set.empty
  //   val variables: Abstraction = Set.empty

  def execute(program: WhileProgram): (DS, DS) = {
    var fixed = false

    // writing entry and exits as functions would be a possible
    // solution. nonetheless, since one depends on each other,
    // and the CFG might include cycles, a table-based implementation
    // avoids infinite loops.
    val entry: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
    val exit: mutable.HashMap[Int, Abstraction] = mutable.HashMap()

    // we need to initialize exit..., since we have
    // to first compute entry[l] from exit[l]. after
    // that, we recompute exit[l] from entry[l].
    for (label <- labels(program)) {
      entry(label) = bottom //for LV the meet operator is union so we initialize entry(label) with all empty set as smallest solution
    }

    do {
      val entryOld = entry.clone()
      val exitOld = exit.clone()

      for (label <- labels(program)) {
        exit(label) = {
          if (finalLabels(program.stmt).contains(label))
            Set.empty
          else {
            var res = bottom
            for ((from, to) <- flowR(program) if to == label) { // | (ℓ',ℓ) ∈ flowR(S*), such that S* are all statements of program
              res = entry(from) union res // ⋃ { LVexit(ℓ') } -> remember: {exit(from)} == entry(to)
            }
            res
          }
        }
        entry(label) = {
          val b = block(label, program) // block with a given label *label*
          // ⋃ { exit(from) | (from, to) <- flowR(program) and to == label}
          // according to Table 2.4 on page 48 of the PPA book
          (exit(label) diff kill(b.get, program)) union gen(b.get)
        }
      }
      fixed = (entryOld, exitOld) == (entry, exit)
    } while (!fixed)

    (entry, exit)
  }

  /* kill definition according to Table 2.4 of the PPA book */
  def kill(block: Block, program: WhileProgram): Abstraction = block match {
    case Assignment(x, exp, label) => Set(x) //killLV({X := a}ℓ) = {x}
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.4 of the PPA book */
  def gen(block: Block): Abstraction = block match {
    case Assignment(x, a, label) => fv(a) //genLV({X := a}ℓ) = FV(a)
    case Skip(_) => Set.empty
    case Condition(b, _) => fv(b) //genLV({b}ℓ) = FV(b)
  }


}
