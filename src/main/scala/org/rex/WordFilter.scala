package org.rex

import scala.language.implicitConversions

object WordFilter {

  type Index = Int

  /**
    * Evaluates to true iff the word at position i in sentence s should be accepted.
    */
  type Fn = Sentence => Index => Boolean

  lazy val permitAll: WordFilter.Fn =
    (ignoreS: Sentence) => (ignoreI: Int) => true

  lazy val noTaggedPunct: WordFilter.Fn =
    (s: Sentence) =>
      s.tags match {

        case Some(tags) =>
          (i: Int) =>
            tags(i) != s.tokens(i)

        case None =>
          (ignore: Int) =>
            true
    }

  lazy val noKnownPunct: WordFilter.Fn = { (s: Sentence) => (i: Int) =>
    !knownPunct.contains(s.tokens(i))
  }

  val knownPunct = Set(
    "~",
    "`",
    "!",
    "@",
    "#",
    "$",
    "%",
    "^",
    "&",
    "*",
    "(",
    ")",
    "-",
    "_",
    "+",
    "=",
    "{",
    "[",
    "]",
    "}",
    "|",
    "\\",
    ":",
    ";",
    "\"",
    "'",
    ".",
    ",",
    "<",
    ">",
    "?",
    "/"
  )

}
