package br.unb.cic.wlang.semantics

import br.unb.cic.wlang.Stmt
import br.unb.cic.wlang.semantics.Environment._

/** An abstract configuration. I would actually name it
 * as computation, since it corresponds to the result
 * of a computation */
trait AbsConfiguration

/*
 * A computation might be a simple computation (SC) or a
 * terminal computation (TC)
 */
case class TC(env: Env, store : Store) extends AbsConfiguration
case class SC(stmt: Stmt, env: Env, store: Store) extends AbsConfiguration