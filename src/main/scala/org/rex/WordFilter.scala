package org.rex

import scala.language.implicitConversions

/**
 * Evaluates to true iff the word at position i in sentence s should be accepted.
 */
trait WordFilter extends Serializable {
  def apply(s: Sentence)(i: Int): Boolean
}

object WordFilter {

  implicit def fn2wordFilter(fn: Sentence => Int => Boolean): WordFilter =
    new WordFilter {
      override def apply(s: Sentence)(i: Int): Boolean = fn(s)(i)
    }

  lazy val permitAll: WordFilter =
    (ignoreS: Sentence) =>
      (ignoreI: Int) =>
        true

  lazy val noTaggedPunct: WordFilter =
    (s: Sentence) => s.tags match {

      case Some(tags) =>
        (i: Int) =>
          tags(i) != s.tokens(i)

          case None =>
        (ignore: Int) =>
          true
    }

  lazy val noKnownPunct: WordFilter = {
    (s: Sentence) =>
      (i: Int) =>
        !knownPunct.contains(s.tokens(i))
  }

  val knownPunct = Set(
    "~", "`", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "_", "+", "=",
    "{", "[", "]", "}", "|", "\\",
    ":", ";", "\"", "'",
    ".", ",", "<", ">", "?", "/"
  )

}