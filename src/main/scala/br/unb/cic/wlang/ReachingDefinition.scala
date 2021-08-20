package br.unb.cic.wlang

import scala.collection.mutable
import CFGBuilder.flow
import WhileProgram.{Label, labels, block, initLabel, fv, assignments}

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object ReachingDefinition {

  type Abstraction = Set[(String, Label)]
  type DS = mutable.HashMap[Int, Abstraction]

  val bottom: Abstraction = Set.empty

  val undef = -1   // this is the equivalent to the undef label in the book (?)

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
    for(label <- labels(program)) {
      exit(label) = bottom
    }

    do {
      val entryOld = entry.clone()
      val exitOld = exit.clone()

      for(label <- labels(program)) {
        entry(label) =
          if (label == initLabel(program.stmt))
            fv(program.stmt).map(v => (v, undef)) // { (x, ?) | x in fv(s) }
          else {
            // U { exit(from) | (from, to) <- flow(program) and to == label}
            // we could have implemented this using foldl, though I hope this
            // solution here is easier to understand.
            var res = bottom
            for((from, to) <- flow(program) if to == label) {
              res = exit(from) union res
            }
            res
          }
        val b = block(label, program)  // block with a given label *label*
        exit(label) = (entry(label) diff kill(b.get, program)) union gen(b.get)
      }
      fixed = (entryOld, exitOld) == (entry, exit)
    }
    while(! fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.2 of the ppl book */
  def kill(block: Block, program: WhileProgram): Set[(String, Label)] = block match {
    case Assignment(x, _, label) =>
      Set((x, undef)) union assignments(program).filter( { case (v, l) => v == x && l != label } )
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.2 of the PPL book */
  def gen(block: Block): Set[(String, Label)] = block match {
    case Assignment(x, _, label) => Set((x,label))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }
}
