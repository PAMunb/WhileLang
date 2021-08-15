package br.unb.cic.wlang

import scala.collection.mutable

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object ReachDefinition {

  type Abstraction = Set[(String, Stmt)]
  type DS = mutable.HashMap[Stmt, Abstraction]

  val empty: Abstraction = Set.empty

  def execute(program: WhileProgram): (DS, DS) = {
    val cfg = CFGBuilder.build(program)
    val stmts = CFGBuilder.stmts(program)

    // initialization
    val universalSet = initUniversalSet(program.stmt)
    val out = initOutSet(stmts)
    val in = new DS()

    var fixedPoint = false

    while(!fixedPoint) {
      val (oldIn, oldOut) = (in.clone(), out.clone())
      for(s <- stmts) {
        //IN[S] = U {OUT[from] | (from, to) in cfg, S == to}  //OK conforme definição do livro para o RDEntry(to)
        //This is not so beautiful in Scala =(
        in(s) =  (for { (from,to) <- cfg if to == s } yield out(from)).foldLeft(empty)(_ union _) //não consegui ler esse trecho

        //OUT[S] = GEN(S) U (IN[S] - KILL[S]) //OK conforme definição do livro para o RDexit(to)
        out(s) = gen(s) union (in.getOrElse(s, empty) diff kill(s, universalSet)) //não consegui ler esse trecho
      }
      fixedPoint = (oldIn, oldOut) == (in, out) //Se não há mais alterações nos conjuntos entre RDentry(from), RDexit(from) e RDentry(to), RDexit(to) então chegou no fixed point e sai do laço
    }
    (in, out)
  }

  def initOutSet(stmts: Set[Stmt]): DS = { //inicializa os sets dos stmts como vazio? ok!
    val out = new DS()
    stmts.foreach(s => out += s -> empty)
    out
  }

  def initUniversalSet(stmt: Stmt): Abstraction = stmt match  { //não entendi a necessidade desse inituniversal
    case Assignment(v, _, _) => Set((v, stmt))
    case Skip(_) => Set.empty
    case IfThenElse(_, s1, s2, _) => initUniversalSet(s1) union initUniversalSet(s2)
    case While(_, s, _) => initUniversalSet(s)
    case Sequence(s1, s2) => initUniversalSet(s1) union initUniversalSet(s2)
  }

  def kill(stmt: Stmt, universalSet: Abstraction): Abstraction = stmt match {
    case Assignment(v, _, _) => universalSet.filter(t => v == t._1) //filtra true/false mas não entendi como o 't._1' é true somente p/ assignments a 'v'
    case _ => Set.empty
  }

  def gen(stmt: Stmt): Abstraction = stmt match {
    case Assignment(v, _, _) => Set((v,stmt))  //v = nome da variável que está sendo definida no Assignment
    case _ => Set.empty
  }

}
