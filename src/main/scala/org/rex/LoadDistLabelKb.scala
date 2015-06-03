package org.rex

import java.io.{ BufferedWriter, FileWriter, File }

import scala.io.Source
import scala.util.Try

object LoadDistLabelKb {

  import DistLabelStr.KnowledgeBase

  def apply(f: File): Try[KnowledgeBase] =
    Try {
      Source.fromFile(f)
        .getLines()
      // deserialize the KB
      // do it by reading a (Q,A,Rel) per line
      ???
    }

  def apply(kb: KnowledgeBase)(f: File): Try[Unit] =
    Try {
      val w = new BufferedWriter(new FileWriter(f))
      try {
        // serialize the KB
        // do it as a form of (Q,A,Rel) per line
        ???
      } finally {
        w.close()
      }
    }

}