package org.rex

package object text {

  def textOf(d: Document, sentenceSep: String = "\n"): String =
    d.sentences.map { textOf }.mkString(sentenceSep)

  def textOf(s: Sentence): String =
    s.tokens
      .foldLeft("") {
        case (accum, token) =>
          if (knownPunct.contains(token))
            accum + token
          else
            accum + " " + token
      }
      .trim

  lazy val knownPunct = Set(
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
