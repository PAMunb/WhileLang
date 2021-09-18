package br.unb.cic.wlang.parser

import java.nio.file.Paths
import scala.io.Source

object ResourceHandle {
    def getContent(resource: String): String = {
      val path = Paths.get(getClass.getClassLoader.getResource(resource).toURI)
      assert(path != null)

      val file = Source.fromFile(path.toString)
      val content = file.getLines().mkString("\n")

      file.close()

      content
    }
}
