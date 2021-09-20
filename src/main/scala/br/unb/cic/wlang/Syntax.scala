package br.unb.cic.wlang

import WhileProgram._

/**
 * Abstract representation of a While Program.
 *
 * @param stmt The program main statement.
 */
case class WhileProgram(declarations: List[Procedure], stmt: Stmt)

case class Procedure(name: String, formalArgs: List[FormalArgument], ln: Label, stmt: Stmt, lx: Label)

trait ParameterType

case object ByValue extends ParameterType
case object ByResult extends ParameterType

case class FormalArgument(name: String, parameterType: ParameterType)

object WhileProgram {
  type Label = Int

  def findProcedure(name: String, program: WhileProgram): Procedure = findProcedure(name, program.declarations)

  def findProcedure(name: String, declarations: List[Procedure]): Procedure = declarations.find(p => p.name == name).get

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

  def labels(wp: WhileProgram): Set[Label] =
    wp.declarations.map(p => labels(p)).foldLeft(Set[Label]())(_ union _) union labels(wp.stmt)

  def labels(p: Procedure): Set[Label] = Set[Label](p.ln, p.lx) union labels(p.stmt)

  private def labels(stmt: Stmt): Set[Label] = stmt match {
    case Assignment(_, _, label) => Set(label)
    case Skip(label) => Set(label)
    case Sequence(s1, s2) => labels(s1) union labels(s2)
    case IfThenElse(c, s1, s2) => Set(c.label) union labels(s1) union labels(s2)
    case Call(_, _, lc, lr) => Set(lc, lr)
    case While(c, s) => Set(c.label) union labels(s)
  }

//  def blocks(stmt: Stmt) : Set[Block] = stmt match {
//    case Assignment(_, _, _) => Set(stmt)
//    case Skip(_) => Set(stmt)
//    case Sequence(s1, s2) => blocks(s1) union blocks(s2)
//    case IfThenElse(c, s1, s2) => blocks(s1) union blocks(s2) union Set(c):Set[Block]
//    case Call(_, _, _, _) => Set(stmt)
//    case While(c, s) => blocks(s) union Set(c)
//  }

  def blocks(wp: WhileProgram): Set[Block] =
    wp.declarations.map(p => blocks(p)).foldLeft(Set[Block]())(_ union _ ) union blocks(wp.stmt)

  def blocks(p: Procedure): Set[Block] = Set[Block](Entry(p.ln), Entry(p.lx)) union blocks(p.stmt)

  def blocks(stmt: Stmt) : Set[Block] = stmt match {
    case Sequence(s1, s2) => blocks(s1) union blocks(s2)
    case IfThenElse(c, s1, s2) => blocks(s1) union blocks(s2) union Set(c):Set[Block]
    case While(c, s) => blocks(s) union Set(c)
    case aBlock : Block => Set(aBlock)
  }

  /* finds a specific block with a label within the statement stmt */
  def block(label: Label, program: WhileProgram): Option[Block] = blocks(program.stmt).find(b => b match {
    case Skip(l) => l == label
    case Assignment(_, _, l) => l == label
    case Call(_, _, lc, lr) => ???     // TODO: Verificar se eh essa a regra no livro.
                                       //   talvez seja o entry da procedure.
    case Condition(_, l) => l == label
  })

  def initLabel(wp: WhileProgram): Label = initLabel(wp.stmt)

  def initLabel(proc: Procedure) : Label = proc.ln

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
    case Call(_, _, lc, _) => lc
    case While(Condition(_, label),_) => label
  }

  def finalLabels(wp: WhileProgram): Set[Label] = finalLabels(wp.stmt)

  def finalLabels(proc: Procedure) : Set[Label] = Set(proc.lx)

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
    case Call(_, _, _, lr) => Set(lr)
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
    case Const(_) => Set() // == Set.empty
    case Add(l, r) => fv(l) union fv(r)
    case Sub(l, r) => fv(l) union fv(r)
    case Mult(l, r) => fv(l) union fv(r)
  }

  def fv(exp: BExp): Set[String] = exp match {
    case Not(v) => fv(v)
    case And(vl, vr) => fv(vl) union fv(vr)
    case Or(vl, vr) => fv(vl) union fv(vr)
    case Eq(vl, vr) => fv(vl) union fv(vr)
    case GT(vl, vr) => fv(vl) union fv(vr)
  } 
 
  def assignments(program: WhileProgram): Set[(String, Label)] = assignments(program.stmt)

  private def assignments(stmt: Stmt): Set[(String, Label)] = stmt match {
    case Assignment(v, _, label) => Set((v, label))
    case Skip(_) => Set()
    case Sequence(s1, s2) => assignments(s1) union assignments(s2)
    case IfThenElse(_, s1, s2) => assignments(s1) union assignments(s2)
    case While(_, s) => assignments(s)
  }

  def variables(program: WhileProgram): Set[String] = variables(program.stmt)

  private def variables(stmt: Stmt): Set[String] = stmt match {
    case Assignment(v, _, label) => Set(v)
    case Skip(_) => Set.empty
    case Sequence(s1, s2) => variables(s1) union variables(s2)
    case IfThenElse(_, s1, s2) => variables(s1) union variables(s2)
    case While(_, s) => variables(s)
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
case class Div(lef: AExp, right: AExp) extends AExp      // Divide arithmetic expression

/* Concrete implementations of BExp */
case object True extends BExp
case object False extends BExp

case class Not(exp: BExp) extends BExp
case class And(left: BExp, right: BExp) extends BExp
case class Or(Left: BExp, right: BExp) extends BExp
case class Eq(left: AExp, right: AExp) extends BExp
case class NEq(left: AExp, right: AExp) extends BExp
case class GT(left: AExp, right: AExp) extends BExp
case class LT(left: AExp, right: AExp) extends BExp

trait Block

case class Entry(label: Label) extends Block
case class Exit(label: Label) extends Block

abstract class Stmt
abstract class ElementaryStmt extends Stmt with Block
abstract class CompositeStmt extends Stmt

/* Concrete implementations of Statements */
case class Condition(exp: BExp, label: Label) extends Block

case class Assignment(name: String, exp: AExp, label: Label) extends ElementaryStmt
case class Sequence(s1: Stmt, s2: Stmt) extends CompositeStmt   // s1;s2
case class IfThenElse(condition: Condition, thenStmt: Stmt, elseStmt: Stmt) extends CompositeStmt
case class While(condition: Condition, stmt: Stmt) extends CompositeStmt
case class Call(name: String, args: List[AExp], lc: Label, lr: Label) extends ElementaryStmt
case class Skip(label: Label) extends ElementaryStmt
