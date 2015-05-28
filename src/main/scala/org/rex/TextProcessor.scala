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
        (
          id,
          {
            val doc = corenlpProcessor.mkDocument(text)

            if (conf.tagSet.isDefined) {
              corenlpProcessor.tagPartsOfSpeech(doc)

              if (conf.lemmatize) {
                corenlpProcessor.lemmatize(doc)

                if (conf.entSet.isDefined)
                  corenlpProcessor.recognizeNamedEntities(doc)

                if (conf.parse) {
                  corenlpProcessor.parse(doc)

                  if (conf.resolveCoreference)
                    corenlpProcessor.resolveCoreference(doc)

                  if (conf.discourse)
                    corenlpProcessor.discourse(doc)
                }
              }
            }

            doc
          }
        )
    }

}

/**
 * Information about the configuration of a natural language parser.
 *
 * @param entSet Information about the named entities that the associated processor uses, if applicable.
 * @param tagSet Information about the part-of-speech tags that the associated processor uses, if applicable.
 * @param parse Whether or not the associated parser should construct a syntactic parse tree of sentences.
 * @param lemmatize Whether or not the associated parser should perform lemmatization. Needs POS tagging.
 * @param resolveCoreference Whether or not the associated parser should perform co-reference resolution. Needs parsing.
 * @param discourse Whether or not the associated parser should perform additional discourse parsing. Needs parsing.
 */
case class ProcessingConf(
  entSet: Option[NamedEntitySet],
  tagSet: Option[PosTagSet],
  parse: Boolean,
  lemmatize: Boolean,
  resolveCoreference: Boolean,
  discourse: Boolean)

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

object PosTagSet {

  object DefaultPennTreebank {
    implicit val posSet =
      new PosTagSet {
        override val tags = Set(
          "$",
          "``",
          "''",
          "(",
          ")",
          ",",
          "--",
          ".",
          ":",
          "CC",
          "CD",
          "DT",
          "EX",
          "FW",
          "IN",
          "JJ",
          "JJR",
          "JJS",
          "LS",
          "MD",
          "NN",
          "NNP",
          "NNPS",
          "NNS",
          "PDT",
          "POS",
          "PRP",
          "PRP$",
          "RB",
          "RBR",
          "RBS",
          "RP",
          "SYM",
          "TO",
          "UH",
          "VB",
          "VBD",
          "VBG",
          "VBN",
          "VBP",
          "VBZ",
          "WDT",
          "WP",
          "WP$",
          "WRB"
        )
      }
  }

}