package org.rex.relation_extract

import scala.language.implicitConversions

case class AdjacentFeatures(ngramWidth: Int)

object AdjacentFeatures {

  def left(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] =
    s.slice(index - ngramWidth, index)

  def right(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] =
    s.slice(index + 1, index + 1 + ngramWidth)

}
