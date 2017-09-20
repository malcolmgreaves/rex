package org.rex.relation_extract

import scala.language.implicitConversions

case class InsideFeatures(ngramWidth: Int, skipSize: Int)

object InsideFeatures {

  @inline def apply(inside: InsideFeatures)(inner: Seq[String]): Seq[String] =
    if (inner.size == 0) {
      nothing
    } else {
      val end = inner.size - inside.ngramWidth + 2
      (0 until end).flatMap(sliceStart =>
        selectKSkipGram(inner.slice(sliceStart, inner.size), inside.ngramWidth, inside.skipSize))
    }

  @inline private def selectKSkipGram(s: Seq[String], n: Int, k: Int): Seq[String] = {
    val first = s.head
    if (n <= 1)
      Seq(first)
    else
      Seq(first) ++
        (0 until Math.min(k + 1, s.size)).flatMap(j => {
          val rest = s.slice(j + 1, s.size)
          if (rest.size > 0)
            selectKSkipGram(rest, n - 1, k - j)
              .map(gram => s"$first,$gram")
          else
            nothing
        })
  }

  private val nothing = IndexedSeq.empty[String]
}
