package org.rex.text

import scala.language.implicitConversions

object WordView {

  type Index = Int

  type Fn = Sentence => Index => String

  lazy val lowercase: WordView.Fn =
    (s: Sentence) => (i: Int) => s.tokens(i).toLowerCase

  lazy val identity: WordView.Fn =
    (s: Sentence) => (i: Int) => s.tokens(i)
}
