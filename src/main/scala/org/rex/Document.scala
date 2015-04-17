package org.rex

import language.implicitConversions

import edu.arizona.sista.processors.CorefMention

/**
 * Represents a uniquely identifiable ordered sequence of sentences.
 *
 * @param id A unique identifier for this document.
 * @param sentences The document's sentences, in-order.
 * @param corefMentions Pairs of co-referent entities across the sentences in this document.
 */
case class Document(
  id: String,
  sentences: Seq[Sentence],
  corefMentions: Option[Seq[Coref]] = None)

object Document {

  import edu.arizona.sista.processors.{ Document => SistaDocument }

  /** Converts a edu.arizaona.sista.processors.Document into a org.rex.Document in a straightforward manner. */
  implicit def sistaDoc2Doc(idAndDoc: (String, SistaDocument)): Document =
    Document(
      idAndDoc._1,
      idAndDoc._2.sentences.map(Sentence.sistaSentence2Sentence).toSeq,
      idAndDoc._2.coreferenceChains.map(chains =>
        chains.getChains.map(chain =>
          Coref(chain.map(Mention.sistaCorefMention2Mention).toSeq)
        ).toSeq
      )
    )
}

/**
 * Represents a sentence of words, each with possible tags.
 *
 * @param tokens Every token in the sentence, in-order.
 * @param tags If present, has the part-of-speech tag information for each token.
 * @param entities If present, has the named entity information for each token.
 */
case class Sentence(
  tokens: Seq[String],
  tags: Option[Seq[String]] = None,
  entities: Option[Seq[String]] = None)

object Sentence {

  /**
   * If present, is the sentence tokens combined based upon the entity information.
   *
   * Successive tokens with the samed named entity label are combined into a single token.
   * Space (" ") is inserted between each combined token.
   */
  def chunkTokens(s: Sentence)(implicit entSet: NamedEntitySet): Option[Seq[String]] = {

    s.entities.map(ents =>

      if (ents.size <= 1) {
        s.tokens

      } else {

        val (chunkedIndices, _, lastWorkingIndices) =
          ents.slice(1, ents.size).zip(s.tokens.slice(1, s.tokens.size))
            .zipWithIndex
            .map({ case ((e, t), indexMinus1) => (e, t, indexMinus1 + 1) })
            .foldLeft((Seq.empty[Seq[Int]], ents.head, Seq(0)))({

              case ((indicesChunked, previousEnt, workingIndices), (entity, token, index)) =>

                val isNonEnt = entity == entSet.nonEntityTag

                val continueToChunk = !isNonEnt && previousEnt == entity

                val updatedWorkingIndices =
                  if (continueToChunk)
                    workingIndices :+ index
                  else
                    Seq(index)

                val updatedIndices =
                  if (!continueToChunk)
                    if (workingIndices.size > 0)
                      indicesChunked :+ workingIndices
                    else
                      indicesChunked
                  else
                    indicesChunked

                (updatedIndices, entity, updatedWorkingIndices)
            })

        val allChunkedIndices =
          if (lastWorkingIndices.isEmpty)
            chunkedIndices
          else
            chunkedIndices :+ lastWorkingIndices

        allChunkedIndices
          .foldLeft(Seq.empty[String])({

            case (newChunkedTokens, indices) =>
              newChunkedTokens :+ indices.map(index => s.tokens(index)).mkString(" ")
          })

      }
    )

  }

  import edu.arizona.sista.processors.{ Sentence => SistaSentence }

  /** Converts a edu.arizaona.sista.processors.Sentence into a org.rex.Sentence in a straightforward manner. */
  implicit def sistaSentence2Sentence(s: SistaSentence): Sentence =
    Sentence(s.words, s.tags.map(_.toSeq), s.entities.map(_.toSeq))

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
