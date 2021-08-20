package br.unb.cic.wlang

import scala.collection.mutable
import CFGBuilder.flow
import WhileProgram.{Label, labels, block, initLabel, fv, nonTrivialExpression}

/** Implementation of the Reaching Definition algorithm.
  */
object AvailableExpression {

  type Abstraction = Set[(Exp)]   //abstração para AE não é (String,Label) é AExp
  type DS = mutable.HashMap[Int, Abstraction]

  val empty: Abstraction = Set.empty
  def execute(program: WhileProgram): (DS, DS) = {
    var fixed = false

    // writing entry and exits as functions would be a possible
    // solution. nonetheless, since one depends on each other,
    // and the CFG might include cycles, a table-based implementation
    // avoids infinite loops.
    val entry: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
    val exit: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
    val nonTrivialExpressionSet: Set[Exp] = nonTrivialExpression(program)   // Conjunto de todas as non-trivial-expressions do programa -> não consegui usar na kill e gen

    // we need to initialize exit..., since we have
    // to first compute entry[l] from exit[l]. after
    // that, we recompute exit[l] from entry[l].
    for (label <- labels(program)) {
      exit(label) = empty    //exit(label) = nonTrivialExpressionSet não funcionou, os mesmos asserts apresentaram erro
    }

    do {                           // faça...até, executa ao menos uma vez
      val entryOld = entry.clone()
      val exitOld = exit.clone()

      for (label <- labels(program)) {
        entry(label) =
          if (label == initLabel(program.stmt))
            empty  //return empty para entry(1)
          else {
            // ⋂ { exit(from) | (from, to) <- flow(program) and to == label}
            // conforme equação da página 38 (49 no pdf) do PPA
            var res = empty
            var acc = empty               
            //var res = nonTrivialExpressionSet //res = nonTrivialExpressionSet não funcionou, mas pode ser a gen o problema
            for ((from, to) <- flow(program) if to == label) {  // | (ℓ',ℓ) ∈ flow(S*), sendo S* os stmt do program
              // acc.++(exit(from))
              res = exit(from) intersect res                    // ⋂ { AExit(ℓ') } -> lembrar: o {exit(from)} == entry(to)
              
              if (res == empty) 
                res = exit(from) intersect acc   //encontrar a 'largest solution' página 38-39 (49-50 no PDF) do livro PPA
              }
            res
          }
        val b = block(label, program) // block with a given label *label*
        exit(label) = (entry(label) diff kill(b.get, program)) union gen(b.get)  // aqui é igual ao RD
      }
      fixed = (entryOld, exitOld) == (entry, exit)
    } while (!fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.1 of the PPA book */
  def kill(block: Block, program: WhileProgram): Set[Exp] =
    block match {
      case Assignment(x, exp, label) => nonTrivialExpression(exp)  //killAE({X := a}ℓ) = {a' ∈ AExp*, | x ∈ FV(a')}
       //onTrivialExpressionSet[exp.left] union nonTrivialExpressionSet[exp.right] -> não encontra o nteSet pq?
      case Skip(_)         => Set.empty
      case Condition(_, _) => Set.empty
    }

  /* gen definition according to Table 2.1 of the PPA book */
  def gen(block: Block): Set[Exp] = block match {
    case Assignment(x, exp, label) => nonTrivialExpression(exp) //genAE({X := a}ℓ) = {a' ∈ AExp*, | x ∉ FV(a')} -> creio que esteja errado
    case Skip(_)                 => Set.empty
    case Condition(b, _)         => nonTrivialExpression(b)
  }
}
