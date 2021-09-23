package br.unb.cic.wlang.semantics

import scala.collection.immutable.HashMap

object Environment {
  type Var   = String
  type Loc   = Int
  type Z     = Int

  type Env   = HashMap[Var, Loc]    // here we are modeling a location as an integer.
  type Store = HashMap[Loc, Z]
}
