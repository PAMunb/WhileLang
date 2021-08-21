package br.unb.cic.wlang

/**
  * Abstract representation of a While Program.
  *
  * @param stmt The program main statement.
  */
case class WhileProgram(stmt: Stmt)

object WhileProgram {

  def nonTrivialExpressions(program: WhileProgram): Set[AExp] =
    blocks(program.stmt).flatMap(block => nonTrivialExpressions(block))

  def nonTrivialExpressions(block: Block): Set[AExp] = block match {
    case Assignment(_, exp, _) => nonTrivialExpressions(exp)
    case Skip(_) => Set.empty
    case Condition(exp, _) => nonTrivialExpressions(exp)
  }

  def nonTrivialExpressions(exp: Exp): Set[AExp] = exp match {
    case aExp: AExp => aExp match {
      case Var(_) => Set.empty
      case Const(_) => Set.empty
      case OpArith(op, a1, a2) => Set(aExp) union nonTrivialExpressions(a1) union nonTrivialExpressions(a2)
    }
    case bExp: BExp => bExp match {
      case True => Set.empty
      case False => Set.empty
      case Not(exp) => nonTrivialExpressions(exp)
      case OpBool(_, b1, b2) => nonTrivialExpressions(b1) union nonTrivialExpressions(b2)
      case OpRelat(_, a1, a2) => nonTrivialExpressions(a1) union nonTrivialExpressions(a2)
    }
  }

  def labels(program: WhileProgram): Set[Int] = labels(program.stmt)

  def block(label: Int, stmt: Stmt): Option[Block] = blocks(stmt).find(_.label == label)

  private def blocks(stmt: Stmt): Set[Block] = stmt match {
    case a@Assignment(_, _, _) => Set(a)
    case s@Skip(_) => Set(s)
    case Sequence(s1, s2) => blocks(s1) union blocks(s2)
    case IfThenElse(condition, thenStmt, elseStmt) =>
      blocks(thenStmt) union blocks(elseStmt) union Set(condition)
    case While(condition, stmt) => blocks(stmt) union Set(condition)
  }

  private def labels(stmt: Stmt): Set[Int] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(s1, s2) => labels(s1) union labels(s2)
    case IfThenElse(c, s1, s2) => Set(c.label) union labels(s1) union labels(s2)
    case While(c, s) => Set(c.label) union labels(s)
  }
}

/* The abstract classes
 *   - AExp: Arithmetic expressions
 *   - BExp: Binary expressions
 *   - Stmt: Statements
 */
sealed trait Exp

sealed trait AExp extends Exp

sealed trait BExp extends Exp

sealed trait Stmt

sealed trait Block {
  def label: Int
}

/* Concrete implementations of AExp */
case class Var(name: String) extends AExp // variables

case class Const(value: Int) extends AExp // integer constants

case class OpArith(op: String, a1: AExp, a2: AExp) extends AExp

/* Concrete implementations of BExp */
case object True extends BExp

case object False extends BExp

case class Not(exp: BExp) extends BExp

case class OpBool(op: String, b1: BExp, b2: BExp) extends BExp

case class OpRelat(op: String, a1: AExp, a2: AExp) extends BExp

/* Concrete implementations of Statements */
case class Condition(exp: BExp, label: Int) extends Block

case class Assignment(name: String, exp: AExp, label: Int) extends Stmt with Block

case class Sequence(s1: Stmt, s2: Stmt) extends Stmt

case class IfThenElse(condition: Condition, thenStmt: Stmt, elseStmt: Stmt) extends Stmt

case class While(condition: Condition, stmt: Stmt) extends Stmt

case class Skip(label: Int) extends Stmt with Block
