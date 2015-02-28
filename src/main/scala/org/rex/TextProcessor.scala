package org.rex

import edu.arizona.sista.processors._
import corenlp.CoreNLPProcessor
import edu.arizona.sista.struct.DirectedGraphEdgeIterator

/** Adds structure to text.
  *
  * Is able to turn unstructured text into a logically organized Document,
  * where individual Sentences, with descrete, individual tokens and optional additional
  * syntactic and semantic information, are all easily accessible.
  * */
trait TextProcessor {
  /** Context for text processing. Can include information about NER and POS tags. */
  def conf: ProcessingConf

  /** Converts a block of text into a Document with the input id. */
  def process(id: String, text: String): Document
}

object TextProcessor {

  /** Uses the Processor to convert a block of text into a document.
    *
    * Conversion is based upon Sista + CoreNLP libraries. Uses the interfaces
    * defined here, however.
    *
    * The input ProcessorConf is assumed to be based upon what the Processor
    * uses. Caller is responsibile for verifying and agreeing to this assumption.
    */
  def apply(pConf: ProcessingConf, corenlpProcessor: Processor): TextProcessor =
    new TextProcessor {
      override val conf = pConf

      override def process(id: String, text: String): Document = {
        val processedDoc = corenlpProcessor.annotate(text)
        Document(id,
          processedDoc.sentences.map(s =>
            Sentence(
              s.words.toSeq,
              s.tags.map(_.toSeq),
              s.entities.map(_.toSeq)
            )
          )
        )
      }
    }

}

trait ProcessingConf {
  /** Information about the named entities that the associated processor uses, if applicable. */
  def entSet: Option[NamedEntitySet]

  /** Information about the part-of-speech tags that the associated processor uses, if applicable. */
  def tagSet: Option[PosTagSet]
}

trait NamedEntitySet {
  /** All named entity tags. Does not include the nonEntityTag. */
  def tags: Set[String]

  /** A value that represents a non-existant tag. */
  def nonEntityTag: String
}

object NamedEntitySet {

  /** Contains implicit NamedEntitySet with PERSON, LOCATION, and ORGANIZATION tags and "" non-entity tag */
  object Default3Class {
    implicit val entSet =
      new NamedEntitySet {
        override val tags = Set("PERSON", "LOCATION", "ORGANIZATION")
        override val nonEntityTag: String = ""
      }
  }

}

trait PosTagSet {
  /** All part-of-speech tags. */
  def tags: Set[String]
}
