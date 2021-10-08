package br.unb.cic.wlang.df.framework

trait MeetOperator

case object Union extends MeetOperator
case object Intersection extends MeetOperator

case class Lattice[Abstraction](meet: MeetOperator, bottom: Set[Abstraction]) {

  def meetOperator(s1: Set[Abstraction], s2: Set[Abstraction]): Set[Abstraction] = meet match {
    case Union => s1 union s2
    case Intersection => s1 intersect s2
  }

  def orderOperator(s1: Set[Abstraction], s2: Set[Abstraction]) : Boolean = meet match {
    case Union => s1 subsetOf s2
    case Intersection => s2 subsetOf s1
  }
}