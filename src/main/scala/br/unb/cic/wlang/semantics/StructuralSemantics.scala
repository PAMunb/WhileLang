package br.unb.cic.wlang.semantics

import br.unb.cic.wlang.{AExp, Add, And, Assignment, BExp, ByResult, ByValue, Call, Const, Div, Eq, False, FormalArgument, GT, IfThenElse, LT, Mult, NEq, Not, Or, Procedure, Sequence, Skip, Stmt, Sub, True, Variable, While, WhileProgram}
import br.unb.cic.wlang.semantics.Environment._

class StructuralSemantics {

  val undef: Z = 0

  var currentLocation = 0

  var wp: WhileProgram = _

  // we modeled environment as a mutable field here, just
  // to look more like the book specification. Another possible
  // approach would be changing the computation classes, in order
  // to return not only a store, but also the environment.
  // var env = new Env()

  // to determine the value of a variable x, we first determines
  // its location 'e = env(x)' and then the value stored in that
  // location 'p = store(e)'...
  def state(env: Env, store: Store, x: Var): Z = store(env(x))

  /**
   * The function A: AExp -> (Env, Store -> Z)
   *
   * Trying to keep the same structure of the book.
   * A different approach could be:
   *
   * aEval(exp: AExp, e: Env, s: Store): Int
   *
   */
  def aEval(exp: AExp): Env => Store => Int = exp match {
    case Const(v)          => _ => _ => v
    case Variable(x)       => e => s => s(e(x))
    case Add(left, right)  => e => s => aEval(left)(e)(s) + aEval(right)(e)(s)
    case Sub(left, right)  => e => s => aEval(left)(e)(s) - aEval(right)(e)(s)
    case Mult(left, right) => e => s => aEval(left)(e)(s) * aEval(right)(e)(s)
    case Div(left, right)  => e => s => aEval(left)(e)(s) / aEval(right)(e)(s)
  }

  /**
   * The function B: BExp -> (State -> T)
   */
  def bEval(exp: BExp): Env => Store => Boolean = exp match {
    case True   => _ => _ => true
    case False  => _ =>_ => false
    case Not(exp) => e => s => ! bEval(exp)(e)(s)
    case And(left, right) => e => s => bEval(left)(e)(s) && bEval(right)(e)(s)
    case Or(left, right)  => e => s => bEval(left)(e)(s) || bEval(right)(e)(s)
    case Eq(left, right)  => e => s => aEval(left)(e)(s) == aEval(right)(e)(s)
    case NEq(left, right) => e => s => aEval(left)(e)(s) != aEval(right)(e)(s)
    case GT(left, right)  => e => s => aEval(left)(e)(s) > aEval(right)(e)(s)
    case LT(left, right)  => e => s => aEval(left)(e)(s) < aEval(right)(e)(s)
  }

  def run(p: WhileProgram): AbsConfiguration = {
    this.wp = p
    interpret(wp.stmt, new Store())(new Env())
  }

  // returns a new environment with a new variable/
  def createLocationForNewVariable(env: Env, x: String): Env = env + (x -> allocation())

  // creates a new location.
  private def allocation(): Loc = {
    currentLocation += 1
    currentLocation
  }

  /**
   * Operational semantics of the while language.
   *
   * @param stmt the statement we are interpreting
   * @param store the current state (remember, just a mapping from var -> Int
   * @return an AbsConfiguration (either a terminal one (TC) or a simple configuration (SC)
   */
  def interpret(stmt: Stmt, store: Store)(e: Env): AbsConfiguration = stmt match {
    case Skip(_) => TC(e, store)                                 // just returns the current state

    case Assignment(x, a, _) =>
      if(e.contains(x)) TC(e, store + (e(x) -> aEval(a)(e)(store)))  // updates the state, assigning x -> A[x]s
      else {
        val env = createLocationForNewVariable(e, x)
        TC(env, store + (env(x) -> aEval(a)(env)(store)))
      }

    case IfThenElse(c, thenStmt, elseStmt) =>
      if(bEval(c.exp)(e)(store)) interpret(thenStmt, store)(e)          // if the condition is true, executes thenStmt
      else                    interpret(elseStmt, store)(e)          // otherwise, executes the elseStmt

    case While(c, ws) =>
      if(bEval(c.exp)(e)(store))  interpret(Sequence(ws, stmt), store)(e)  // beautiful recursion here.
      else                          TC(e, store)                                  // exit the while without changing the state

    case Sequence(stmt1, stmt2) => interpret(stmt1, store)(e) match {    // runs the first statement
      case TC(newEnv, newState) => interpret(stmt2, newState)(newEnv)
      case SC(newStmt, newEnv, newState) => interpret(Sequence(newStmt, stmt2), newState)(newEnv)
    }

    case Call(name, args, _, _)  =>
      // lets setup some temp variables.
      val p = procedure(name)

      val newLocations: Map[Var, Loc] = p.formalArgs.map(x => x.name -> allocation()).toMap   // a map from var to location

      val newEnvironment: Env = e ++ newLocations                      // the new environment

      val resParameters: List[(Var, Var)] =
        p.formalArgs.zip(args).filter({
          case (FormalArgument(_, ByResult), Variable(_)) => true   // we are only interested in ByResult parameters
          case _ => false
        }
        ).map({case (f: FormalArgument, v: Variable) => (f.name, v.name)})

      val newStore = store ++ p.formalArgs.zip(args).map( {
        case (FormalArgument(argName, ByValue), exp) => newLocations(argName) -> aEval(exp)(e)(store)
        case (FormalArgument(argName, ByResult), _)  => newLocations(argName) -> undef
      }).toMap

      interpret(Bind(newEnvironment, p.stmt, resParameters), newStore)(e)

    case Bind(nEnv, s, args) => interpret(s, store)(nEnv) match {
      case SC(newStmt, newEnv, newStore) => interpret(Bind(nEnv, newStmt, args), newStore)(newEnv)
      case TC(newEnv, newStore)          => TC(newEnv, newStore ++ args.map({case (f, a) => newEnv(a) -> newStore(nEnv(f))}))
    }
  }

  /*
   * Finds a procedure with a given name.
   *
   * Note: this implementation assumes the procedure exists
   * in the program wp. If this is not the case, an exception
   * will be thrown.
   */
  def procedure(name: String): Procedure = wp.declarations.find(p => p.name == name).get

  // The bind-construct. This is a new (internal or artificial) statement.
  // It combines the procedure "stmt" with a new environment that maps
  // the formal arguments of a procedure into new locations and that also
  // records the bindings for the "by result" parameters.
  // Here we generalize the book a bit, by allowing more than one
  // parameter by result in a procedure declaration.

  case class Bind(newEnv: Env, stmt: Stmt, resParameterBindings: List[(Var, Var)]) extends Stmt
}
