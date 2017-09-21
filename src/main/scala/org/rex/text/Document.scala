package org.rex.text

import edu.arizona.sista.processors.CorefMention

/**
  * Represents a uniquely identifiable ordered sequence of sentences.
  *
  * @param id A unique identifier for this document.
  * @param sentences The document's sentences, in-order.
  * @param corefMentions Pairs of co-referent entities across the sentences in this document.
  */
case class Document(id: String, sentences: Seq[Sentence], corefMentions: Option[Seq[Coref]] = None)

object Document {

  import edu.arizona.sista.processors.{Document => SistaDocument}

  /** Converts a edu.arizaona.sista.processors.Document into a org.rex.Document in a straightforward manner. */
  implicit def sistaDoc2Doc(idAndDoc: (String, SistaDocument)): Document =
    Document(
      idAndDoc._1,
      idAndDoc._2.sentences.map(Sentence.sistaSentence2Sentence).toSeq,
      idAndDoc._2.coreferenceChains.map(
        chains =>
          chains.getChains
            .map(chain => Coref(chain.map(Mention.sistaCorefMention2Mention).toSeq))
            .toSeq)
    )
}

/**
  * Encapsulates information about many related co-referent mentions.
  *
  * @param mentions A chain of mentions, all of which have been determined to be coreferent.
  */
case class Coref(mentions: Seq[Mention]) extends Serializable

/**
  * Encapsulates the information of a co-reference mention.
  *
  * @param sentenceNum The number, or index, for the sentence in an associated Document.
  * @param from The index that the mention starts on.
  * @param until The index immediately after the end of the mention.
  */
case class Mention(sentenceNum: Int, from: Int, until: Int) extends Serializable

object Mention {

  implicit def sistaCorefMention2Mention(cm: CorefMention): Mention =
    Mention(cm.sentenceIndex, cm.headIndex, cm.endOffset)
}
