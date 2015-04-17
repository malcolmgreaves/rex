package org.rex

import scala.language.implicitConversions

case class AdjacentFeatures(ngramWidth: Int)

object AdjacentFeatures {

  @inline def left[T](s: Seq[T], index: Int)(ngramWidth: Int): Seq[T] =
    s.slice(index - ngramWidth, index)

  @inline def right[T](s: Seq[T], index: Int)(ngramWidth: Int): Seq[T] =
    s.slice(index + 1, index + 1 + ngramWidth)

}
