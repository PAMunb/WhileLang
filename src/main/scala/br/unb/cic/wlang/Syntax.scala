package br.unb.cic.wlang

/**
 * Abstract representation of a While Program.
 *
 * @param stmt The program main statement.
 */
case class WhileProgram(stmt: Stmt)

/* The abstract classes
 *   - AExp: Arithmetic expressions
 *   - BExp: Binary expressions
 *   - Stmt: Statements
 */
abstract class AExp
abstract class BExp
abstract class Stmt

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

/* Concrete implementations of Statements */

case class Assignment(name: String, exp: AExp, label: Int) extends Stmt
case class Sequence(s1: Stmt, s2: Stmt) extends Stmt   // s1;s2
case class IfThenElse(condition: BExp, thenStmt: Stmt, elseStmt: Stmt, label: Int) extends Stmt
case class While(condition: BExp, stmt: Stmt, label: Int) extends Stmt
case class Skip(label: Int) extends Stmt
