package br.unb.cic.wlang

/**
 * Abstract representation of a While Program.
 *
 * @param stmt The program main statement.
 */
case class WhileProgram(stmt: Stmt)

object WhileProgram {
  type Label = Int

  def nonTrivialExpression(program: WhileProgram): Set[Exp] = blocks(program.stmt).flatMap(b => nonTrivialExpression(b))

  def nonTrivialExpression(block: Block): Set[Exp] = block match {
    case Assignment(_, exp, _) => nonTrivialExpression(exp)
    case Skip(_) => Set.empty
    case Condition(exp, _) => nonTrivialExpression(exp)
  }

  def nonTrivialExpression(exp: Exp): Set[Exp] = exp match {
    case Var(_) => Set.empty
    case Const(_) => Set.empty
    case Add(left, right) => Set(exp) union nonTrivialExpression(left) union nonTrivialExpression(right)
    case Sub(left, right) => Set(exp) union nonTrivialExpression(left) union nonTrivialExpression(right)
    case Mult(left, right) => Set(exp) union nonTrivialExpression(left) union nonTrivialExpression(right)
    case Not(exp) => nonTrivialExpression(exp)
    case And(left, right) => nonTrivialExpression(left) union nonTrivialExpression(right)
    case Or(left, right) =>  nonTrivialExpression(left) union nonTrivialExpression(right)
    case GT(left, right) => nonTrivialExpression(left) union nonTrivialExpression(right)
    case Eq(left, right) =>  nonTrivialExpression(left) union nonTrivialExpression(right)
  }

  def labels(program: WhileProgram): Set[Label] = labels(program.stmt)

  private def labels(stmt: Stmt): Set[Label] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(s1, s2) => labels(s1) union labels(s2)
    case IfThenElse(c, s1, s2) => Set(c.label) union labels(s1) union labels(s2)
    case While(c, s) => Set(c.label) union labels(s)
  }

  def blocks(stmt: Stmt) : Set[Block] = stmt match {
    case Assignment(v, e, label) => Set(Assignment(v, e, label))
    case Skip(label) => Set(Skip(label))
    case Sequence(s1, s2) => blocks(s1) union blocks(s2)
    case IfThenElse(c, s1, s2) => blocks(s1) union blocks(s2) union Set(c):Set[Block]
    case While(c, s) => blocks(s) union Set(c)
  }

  /* finds a specific block with a label within the statement stmt */
  def block(label: Label, program: WhileProgram): Option[Block] = blocks(program.stmt).find(b => b.label == label)

  /*
   * Returns the first statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  def initLabel(stmt: Stmt) : Label = stmt match {
    case Assignment(_, _, label) => label
    case Skip(label) => label
    case Sequence(s1, _) => initLabel(s1)
    case IfThenElse(Condition(_, label), _, _)  => label
    case While(Condition(_, label),_) => label
  }

  /*
   * Returns the last statement of a given statement.
   *
   * @see Section 2.1 of Principles of Program Analysis
   */
  def finalLabels(stmt: Stmt) : Set[Label] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(_, s2) => finalLabels(s2)
    case IfThenElse(_, s1, s2) => finalLabels(s1) union finalLabels(s2)
    case While(Condition(_, label), _) => Set(label)
  }

  /* computes the free variables of a statement */
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

  def assignments(program: WhileProgram): Set[(String, Label)] = assignments(program.stmt)

  private def assignments(stmt: Stmt): Set[(String, Label)] = stmt match {
    case Assignment(v, _, label) => Set((v, label))
    case Skip(_) => Set()
    case Sequence(s1, s2) => assignments(s1) union assignments(s2)
    case IfThenElse(_, s1, s2) => assignments(s1) union assignments(s2)
    case While(_, s) => assignments(s)
  }

  def expHasVariable(x: String, exp: Exp): Boolean = exp match {
    case Var(v) => v == x
    case Const(_) => false
    case Add(left, right) => expHasVariable(x, left) || expHasVariable(x, right)
    case Sub(left, right) => expHasVariable(x, left) || expHasVariable(x, right)
    case Mult(left, right) => expHasVariable(x, left) || expHasVariable(x, right)
  }
}

/* The abstract classes
 *   - AExp: Arithmetic expressions
 *   - BExp: Binary expressions
 *   - Stmt: Statements
 */

abstract class Exp
abstract class AExp extends Exp
abstract class BExp extends Exp

/* Concrete implementations of AExp */
case class Var(name: String) extends AExp                // variables
case class Const(value: Int) extends AExp                // integer constants
case class Add(left: AExp, right: AExp) extends AExp     // Add arithmetic operation
case class Sub(left: AExp, right: AExp) extends AExp     // Sub arithmetic operation
case class Mult(lef: AExp, right: AExp) extends AExp     // Mult arithmetic operation

/* Concrete implementations of BExp */
case object True extends BExp
case object False extends BExp

case class Not(exp: BExp) extends BExp
case class And(left: BExp, right: BExp) extends BExp
case class Or(Left: BExp, right: BExp) extends BExp
case class Eq(left: AExp, right: AExp) extends BExp
case class GT(left: AExp, right: AExp) extends BExp

import WhileProgram.Label

trait Block {
  def label: Label
}

abstract class Stmt
abstract class ElementaryStmt extends Stmt with Block
abstract class CompositeStmt extends Stmt

/* Concrete implementations of Statements */
case class Condition(exp: BExp, label: Label) extends Block

case class Assignment(name: String, exp: AExp, label: Label) extends ElementaryStmt
case class Sequence(s1: Stmt, s2: Stmt) extends CompositeStmt   // s1;s2
case class IfThenElse(condition: Condition, thenStmt: Stmt, elseStmt: Stmt) extends CompositeStmt
case class While(condition: Condition, stmt: Stmt) extends CompositeStmt
case class Skip(label: Label) extends ElementaryStmt
