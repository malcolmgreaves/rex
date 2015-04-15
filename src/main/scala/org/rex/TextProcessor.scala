package org.rex

import edu.arizona.sista.processors._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

/**
 * Adds structure to text.
 *
 * Is able to turn unstructured text into a logically organized Document,
 * where individual Sentences, with descrete, individual tokens and optional additional
 * syntactic and semantic information, are all easily accessible.
 */
trait TextProcessor extends Serializable {
  /** Context for text processing. Can include information about NER and POS tags. */
  def conf: ProcessingConf

  /** Converts a block of text into a Document with the input id. */
  def process(id: String, text: String): Document
}

class PrebuiltTextProcessor extends TextProcessor {

  private val x = TextProcessor(
    ProcessingConf(Some(NamedEntitySet.Default4Class.entSet), None),
    new CoreNLPProcessor(withDiscourse = false)
  )

  /** Context for text processing. Can include information about NER and POS tags. */
  def conf: ProcessingConf = x.conf

  /** Converts a block of text into a Document with the input id. */
  def process(id: String, text: String): Document = x.process(id, text)
}

object TextProcessor {

  // implicit conversion from a Sista project Document type to a REx project Document
  import Document.sistaDoc2Doc

  /**
   * Uses the Processor to convert a block of text into a document.
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
      override def process(id: String, text: String): Document =
        (id, corenlpProcessor.annotate(text))
    }

}

trait ProcessingConf extends Serializable {
  /** Information about the named entities that the associated processor uses, if applicable. */
  def entSet: Option[NamedEntitySet]

  /** Information about the part-of-speech tags that the associated processor uses, if applicable. */
  def tagSet: Option[PosTagSet]
}

object ProcessingConf {

  def apply(entities: Option[NamedEntitySet], tags: Option[PosTagSet]): ProcessingConf =
    new ProcessingConf {
      override val entSet = entities
      override val tagSet = tags
    }
}

trait NamedEntitySet extends Serializable {
  /** All named entity tags. Does not include the nonEntityTag. */
  def tags: Set[String]

  /** A value that represents a non-existant tag. */
  def nonEntityTag: String
}

object NamedEntitySet {

  /** Contains implicit NamedEntitySet with PERSON, LOCATION, and ORGANIZATION tags and "" non-entity tag */
  object Default4Class {
    implicit val entSet =
      new NamedEntitySet {
        override val tags = Set("PERSON", "LOCATION", "ORGANIZATION", "DATE")
        override val nonEntityTag: String = "O"
      }
  }

}

trait PosTagSet extends Serializable {
  /** All part-of-speech tags. */
  def tags: Set[String]
}
