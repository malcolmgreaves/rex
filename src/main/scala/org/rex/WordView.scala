package org.rex

import scala.language.implicitConversions

trait WordView extends Serializable {
  def apply(s: Sentence)(i: Int): String
}

object WordView {

  implicit def fn2wordView(fn: Sentence => Int => String): WordView =
    new WordView {
      override def apply(s: Sentence)(i: Int): String = fn(s)(i)
    }

  lazy val lowercase: WordView =
    (s: Sentence) =>
      (i: Int) =>
        s.tokens(i).toLowerCase

  lazy val identity: WordView =
    (s: Sentence) =>
      (i: Int) =>
        s.tokens(i)
}