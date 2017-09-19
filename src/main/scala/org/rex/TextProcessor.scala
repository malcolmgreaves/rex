package org.rex

import edu.arizona.sista.processors._
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

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
