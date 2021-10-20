package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.WhileProgram.{Label}

import scala.collection.mutable

trait MFP[Abstraction] extends GenericFramework[Abstraction] {

  override def execute(): (Result, Result) = {
    // Step1: Initialization (of w and analysis)
    val f = buildControlFlowGraph()
    val extremeLabels = findExtremeLabels()

    var w : List[(Label, Label)] = List()
    val analysis : Result = new mutable.HashMap()

    for((l1, l2) <- f) {
      w = (l1, l2) :: w      // "cons" in Scala is '::'
    }

    val allLabels: Set[Label] =  f.map( { case (l1, l2) => Set(l1, l2) }).fold(Set[Label]())(_ union _)

    for(l <- allLabels union extremeLabels) {
      if(extremeLabels.contains(l)) analysis(l) = extremeValues()
      else analysis(l) = lattice().bottom
    }

    // Step2: Iteration (updating w and analysis)
    while(!w.isEmpty) {
      val (l1, l2) = w.head
      w = w.tail
      if(! lattice().orderOperator(transferFunction(analysis(l1), l1), analysis(l2))) {
        analysis(l2) = lattice().meetOperator(analysis(l2), transferFunction(analysis(l1), l1))
        for( (a, b) <- f.filter( { case (a, _) => a == l2 }) ) {
          w = (a, b) :: w
        }
      }
    }

    val mfp1: Result = analysis
    val mfp2: Result = new mutable.HashMap()

    for(l <- allLabels) {
      mfp2(l) = transferFunction(analysis(l), l)
    }

    (mfp1, mfp2)
  }
}
