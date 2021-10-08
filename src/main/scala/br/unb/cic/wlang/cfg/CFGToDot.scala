package br.unb.cic.wlang.cfg

import CFGBuilder.CFG
import org.typelevel.paiges.Doc

object CFGToDot {

  /**
   * Export a dot representation of a CFG.
   *
   * @param cfg A control flow graph.
   *
   * @return a string with a dot representation of a CFG.
   */
  def exportDot(cfg: CFG): String = {
    val prefix = Doc.text("digraph CFG { ")
    val edges = cfg.map { case (from, to) =>
      Doc.text(from.toString) + Doc.space + Doc.text("->") + Doc.space + Doc.text(to.toString)
    }
    val body = Doc.intercalate(Doc.text("\n"), edges)
    val suffix = Doc.text("}")
    val res = body.tightBracketBy(prefix, suffix)
    res.render(20)
  }

}
