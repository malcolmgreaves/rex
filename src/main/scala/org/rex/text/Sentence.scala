package org.rex.text

/**
  * Represents a sentence of words, each with possible tags.
  *
  * @param tokens Every token in the sentence, in-order.
  * @param tags If present, has the part-of-speech tag information for each token.
  * @param entities If present, has the named entity information for each token.
  */
case class Sentence(tokens: Seq[String],
                    tags: Option[Seq[String]] = None,
                    entities: Option[Seq[String]] = None)

object Sentence {

  import edu.arizona.sista.processors.{Sentence => SistaSentence}

  /** Converts a edu.arizaona.sista.processors.Sentence into a org.rex.Sentence in a straightforward manner. */
  implicit def sistaSentence2Sentence(s: SistaSentence): Sentence =
    Sentence(s.words, s.tags.map(_.toSeq), s.entities.map(_.toSeq))

}
