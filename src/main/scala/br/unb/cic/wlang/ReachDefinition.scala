package br.unb.cic.wlang

import scala.collection.mutable
import CFGBuilder.{flow, initLabel, labels, blocks}

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object ReachDefinition {

  type Abstraction = Set[(String, Int)]
  type DS = mutable.HashMap[Int, Abstraction]

  val empty: Abstraction = Set.empty

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
    for(label <- labels(program.stmt)) {
      exit(label) = empty
    }

    do {
      val inOld = entry.clone()
      val outOld = exit.clone()

      for(label <- labels(program.stmt)) {
        entry(label) =
          if(label == initLabel(program.stmt)) fv(program.stmt).map(v => (v, undef)) // { (x, ?) | x in fv(s) }
          else {
            // U { exit(from) | (from, to) <- flow(program) and to == label}
            // we could have implemented this using foldl, though I hope this
            // solution here is easier to understand.
            var res = empty
            for((from, to) <- flow(program.stmt) if to == label) {
              res = exit(from) union res
            }
            res
          }
        val b = block(label, program.stmt)  // block with a given label label
        exit(label) = entry(label) diff kill(b.get, program.stmt) union gen(b.get)
      }
      fixed = (inOld, outOld) == (entry, exit)
    }
    while(! fixed)
    (entry, exit)
  }

  def fv(stmt: Stmt): Set[String] = blocks(stmt).flatMap(b => fv(b))

  def fv(block: Block): Set[String] = block match {
    case Assignment(_, exp, _) => fv(exp)
    case Skip(_) => Set()
    case Condition(_, _) => Set()
  }

  def fv(exp: AExp): Set[String] = exp match {
    case Var(name) => Set(name)
    case Const(_) => Set()
    case Add(l, r) => fv(l) union fv(r)
    case Sub(l, r) => fv(l) union fv(r)
    case Mult(l, r) => fv(l) union fv(r)
  }

  def block(label: Int, program: Stmt): Option[Block] = blocks(program).find(b => b.label == label)


  def assignments(stmt: Stmt): Set[(String, Int)] = stmt match {
    case Assignment(v, _, label) => Set((v, label))
    case Skip(_) => Set()
    case Sequence(s1, s2) => assignments(s1) union assignments(s2)
    case IfThenElse(_, s1, s2) => assignments(s1) union assignments(s2)
    case While(_, s) => assignments(s)
  }

  /* kill definition accorting to Table 2.2 of the ppl book */
  def kill(block: Block, stmt: Stmt): Set[(String, Int)] = block match {
    case Assignment(x, _, _) => Set((x, undef)) union assignments(stmt).filter(tuple => tuple._1 == x)
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.2 of the PPL book */
  def gen(block: Block): Set[(String, Int)] = block match {
    case Assignment(v, _, label) => Set((v,label))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }
}
