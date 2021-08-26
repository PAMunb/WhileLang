package br.unb.cic.wlang
import scala.collection.mutable
import CFGBuilder.{flow, flowR}
import WhileProgram.{Label, labels, block, initLabel, finalLabels, fv, nonTrivialExpression, expHasVariable}

/* Implementation of the Available Expression algorithm.
 */
object VeryBusyExpression {
  type Abstraction = Set[(Exp)] //for VB the Abstraction is AExp according to Page 45 of the PPA Book.
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
    //   exit(label) = 
      entry(label) =   
        bottom //for VB the meet operator is intersection so we initialize exit(label) with all non-trivial expressions of the program as largest solution
    }

    do {
      val entryOld = entry.clone()
      val exitOld = exit.clone()

      for (label <- labels(program)) {
        //println("(label): (" + label + ")")
        exit(label) = {
          if (finalLabels(program.stmt).contains(label))
            empty
          else {
            var res = bottom
            for ((from, to) <- flowR(program) if to == label) { // | (ℓ',ℓ) ∈ flow(S*), such that S* are all statements of program
              res = entry(from) intersect res // ⋂ { VBexit(ℓ') } -> remember: {exit(from)} == entry(to)
            }
            res
          }
        }
        entry(label) = {
          val b = block(label, program) // block with a given label *label*
          // ⋂ { exit(from) | (from, to) <- flow(program) and to == label}
          // according to Table 2.3 on page 44 of the PPA book
          (exit(label) diff kill(b.get, program)) union gen(b.get)
        }
      }

      fixed = (entryOld, exitOld) == (entry, exit)
    } while (!fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.3 of the PPA book */
  def kill(block: Block, program: WhileProgram): Set[Exp] = block match {
    case Assignment(x, exp, label) => nonTrivialExpression(program).filter(exp => expHasVariable(x, exp)) //killVB({X := a}ℓ) = {a' ∈ AExp*, | x ∈ FV(a')}
    case Skip(_)                   => Set.empty
    case Condition(_, _)           => Set.empty
  }

  /* gen definition according to Table 2.3 of the PPA book */
  def gen(block: Block): Set[Exp] = block match {
    case Assignment(x, a, label) => nonTrivialExpression(a) //genVB({X := a}ℓ) = AExp(a)
    case Skip(_)                   => Set.empty
    case Condition(b, _)           => nonTrivialExpression(b) //genVB({b}ℓ) = AExp(b)
  }
}
