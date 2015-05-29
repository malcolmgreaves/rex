package org.rex

import spire.syntax.cfor._

import scala.language.implicitConversions
import scala.xml.parsing.TokenTests

/**
 * Turns a sentence into a sequence of it's viewable words along
 * with whether or not the viewable word should be filtered.
 */
object SentenceViewFilter {

  type TokenTest = (String, Boolean)
  type Fn = Sentence => Seq[TokenTest]

  def apply(wordView: WordView.Fn, wordFilter: WordFilter.Fn): SentenceViewFilter.Fn =
    (s: Sentence) => {
      val wv = wordView(s)
      val wf = wordFilter(s)

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