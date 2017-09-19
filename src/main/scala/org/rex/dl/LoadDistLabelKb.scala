package org.rex.dl

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source
import scala.util.Try

object LoadDistLabelKb {

  import org.rex.dl.DistLabelStr._

  type Triple = (Query, Answer, Label)

  def parseTripleFromLine(l: String): Triple = {
    val bits = l.split("\t")
    (bits(0), bits(1), bits(2))
  }

  def apply(f: File): Try[KnowledgeBase] =
    Try {
      Source
        .fromFile(f)
        .getLines()
        .map(parseTripleFromLine)
        .foldLeft(Map.empty[Query, Map[Answer, Labels]]) {

          case (m, (q, a, r)) =>
            if (m contains q) {
              val answersForQ = m(q)
              (m - q) + (q -> (
                if (answersForQ contains a)
                  (answersForQ - a) + (a -> (answersForQ(a) + r))
                else
                  answersForQ + (a -> Set(r))
              ))

            } else
              m + (q -> Map(a -> Set(r)))
        }
    }

  def apply(kb: KnowledgeBase)(f: File): Try[Unit] =
    Try {
      val w = new BufferedWriter(new FileWriter(f))
      try {
        kb.foreach {
          case (q, amap) =>
            amap.foreach {
              case (a, labels) =>
                labels.foreach { l =>
                  w.write(s"${writeTripleToLine(q, a, l)}\n")
                }
            }
        }
      } finally {
        w.close()
      }
    }

  @inline def writeTripleToLine(t: Triple): String =
    writeTripleToLine(t._1, t._2, t._3)

  @inline def writeTripleToLine(q: Query, a: Answer, l: Label): String =
    s"$q\t$q\t$l"

}
