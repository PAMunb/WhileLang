package br.unb.cic.wlang

import scala.collection.immutable.HashMap

package object StructuralSemantics {
  type State = HashMap[String, Int]

  /** An abstract configuration. I would actually name it
   * as computation, since it corresponds to the result
   * of a computation */
  trait AbsConfiguration

  /*
   * A computation might be a simple computation (SC) or a
   * terminal computation (TC)
   */
  case class TC(env : State) extends AbsConfiguration
  case class SC(stmt: Stmt, env: State) extends AbsConfiguration

  /**
   * The function A: AExp -> (State -> Z)
   *
   * Trying to keep the same structure of the book.
   * A different approach could be:
   *
   * aEval(exp: AExp, s: State): Int
   *
   * Similar to the book, aEval is a function that
   * takes an exp as input and returns a function from
   * (State -> Int)
   */
  def aEval(exp: AExp): State => Int = exp match {
    case Const(v)          => _ => v
    case Var(x)            => e => e(x)
    case Add(left, right)  => e => aEval(left)(e) + aEval(right)(e)
    case Sub(left, right)  => e => aEval(left)(e) - aEval(right)(e)
    case Mult(left, right) => e => aEval(left)(e) * aEval(right)(e)
    case Div(left, right)  => e => aEval(left)(e) / aEval(right)(e)
  }

  /**
   * The function B: BExp -> (State -> T)
   *
   */
  def bEval(exp: BExp): State => Boolean = exp match {
    case True   => _ => true
    case False  => _ => false
    case Not(e) => s => ! bEval(e)(s)
    case And(left, right) => s => bEval(left)(s) && bEval(right)(s)
    case Or(left, right)  => s => bEval(left)(s) || bEval(right)(s)
    case Eq(left, right)  => s => aEval(left)(s) == aEval(right)(s)
    case NEq(left, right) => s => aEval(left)(s) != aEval(right)(s)
    case GT(left, right)  => s => aEval(left)(s) > aEval(right)(s)
    case LT(left, right)  => s => aEval(left)(s) < aEval(right)(s)
  }

  /**
   * Operation semantics of the while language.
   *
   * @param stmt the statement we are interpreting
   * @param s the current state (remember, just a mapping from var -> Int
   * @return an AbsConfiguration (either a terminal one (TC) or a simple configuration (SC)
   */
  def interpret(stmt: Stmt, s: State): AbsConfiguration = stmt match {
    case Skip(_) => TC(s)                                 // just returns the current state

    case Assignment(x, a, _) => TC(s + (x -> aEval(a)(s)))  // updates the state, assigning x -> A[x]s

    case IfThenElse(c, thenStmt, elseStmt) =>
      if(bEval(c.exp)(s)) interpret(thenStmt, s)          // if the condition is true, executes thenStmt
      else                interpret(elseStmt, s)          // otherwise, executes the elseStmt

    case While(c, ws) =>
      if(bEval(c.exp)(s))  interpret(Sequence(ws, stmt), s)  // beautiful recursion here.
      else TC(s)   // exit the while without changing the state

    case Sequence(stmt1, stmt2) => interpret(stmt1, s) match {    // runs the first statement
      case TC(newState) => interpret(stmt2, newState)
      case SC(newStmt, newState) => interpret(Sequence(newStmt, stmt2), newState)
    }
  }


}
