package org.rex

import spire.syntax.cfor._

import scala.language.implicitConversions

trait WordView {
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

/**
 * Turns a sentence into a sequence of it's viewable words along
 * with whether or not the viewable word should be filtered.
 */
trait SentenceViewFilter extends (Sentence => Seq[(String, Boolean)])

object SentenceViewFilter {

  implicit def fn2sentView(f: Sentence => Seq[(String, Boolean)]): SentenceViewFilter =
    new SentenceViewFilter {
      override def apply(v1: Sentence): Seq[(String, Boolean)] = f(v1)
    }

  def apply(wordView: WordView, wordFilter: WordFilter): SentenceViewFilter =
    (s: Sentence) => {
      val wv = wordView(s) _
      val wf = wordFilter(s) _

      // local mutability for efficicency
      val buff = new Array[(String, Boolean)](s.tokens.size)
      cfor(0)(_ < s.tokens.size, _ + 1) { i =>
        buff(i) = (wv(i), wf(i))
      }
      buff.toSeq
    }

  lazy val noKnownPunctLowercase =
    apply(WordView.lowercase, WordFilter.noKnownPunct)

  lazy val noTagPunctLowercase =
    apply(WordView.lowercase, WordFilter.noTaggedPunct)

}