package br.unb.cic.wlang

import br.unb.cic.wlang.CFGBuilder._
import br.unb.cic.wlang.WhileProgram._

object AvailableExpression {
  def process(program: WhileProgram): Abstraction = {
    val bottom = nonTrivialExpressions(program)
    val initialExit =
      labels(program).foldLeft(Map[Int, Set[AExp]]()) { (m, label) => m + (label -> bottom) }

    val abstraction = Abstraction(Map.empty, initialExit)
    process(program, abstraction)
  }

  private def process(program: WhileProgram, abstraction: Abstraction): Abstraction = {
    val (newAbstraction, continue) = iterate(labels(program), program, abstraction)
    if (continue) {
      process(program, newAbstraction)
    } else {
      newAbstraction
    }
  }

  private def iterate(labels: Set[Int], program: WhileProgram, abstraction: Abstraction): (Abstraction, Boolean) = {
    val newAbstraction = labels.foldLeft(abstraction) { (abstraction, label) => {
        val newAbstractionForLabel = generateNewAbstractionForLabel(label, program,abstraction)
        val newEntry = newAbstractionForLabel._1
        val newExit = newAbstractionForLabel._2
        Abstraction(abstraction.entry + newEntry, abstraction.exit + newExit)
      }
    }
    (newAbstraction, abstraction != newAbstraction)
  }
  private def generateNewAbstractionForLabel(label: Int, program: WhileProgram, abstraction: Abstraction): ((Int, Set[AExp]), (Int, Set[AExp])) = {
    val entry = generateEntry(label, program, abstraction)
    val exit = generateExit(label, program, Map(label -> entry))
    (label -> entry, label -> exit)
  }

  private def kill(block: Block, nonTrivialExpressions: Set[AExp]): Set[AExp] = block match {
    case Assignment(name, _, _) => nonTrivialExpressions.filter(exp => expHasVariable(name, exp))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  private def gen(block: Block): Set[AExp] = block match {
    case Assignment(name, exp, _) => nonTrivialExpressions(exp).filterNot(exp => expHasVariable(name, exp))
    case Skip(_) => Set.empty
    case Condition(exp, _) => nonTrivialExpressions(exp)
  }

  private def expHasVariable(name: String, exp: AExp): Boolean = exp match {
    case Var(n) => n == name
    case Const(_) => false
    case OpArith(_, a1, a2) => expHasVariable(name, a1) || expHasVariable(name, a2)
  }

  private def generateExit(label: Int, program: WhileProgram, entry: Map[Int, Set[AExp]]) = {
    val calculatedBlock = block(label, program.stmt).get
    val nonTrivialExpressions = WhileProgram.nonTrivialExpressions(program)
    val exit =
      (entry(label) diff kill(calculatedBlock, nonTrivialExpressions)) union gen(calculatedBlock)
    exit
  }

  private def generateEntry(label: Int, program: WhileProgram, abstraction: Abstraction): Set[AExp] = {

    if (label == initLabel(program.stmt)) {
      Set.empty
    } else {
      val sets = (for {
        (from, to) <- flow(program.stmt)
        if to == label
      } yield abstraction.exit(from)).toList
      sets.foldLeft[Set[AExp]](nonTrivialExpressions(program))((s1,s2) => s1 intersect s2)
    }

  }

  case class Abstraction(entry: Map[Int, Set[AExp]], exit: Map[Int, Set[AExp]])
}