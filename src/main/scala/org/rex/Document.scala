package org.rex

import language.implicitConversions

import edu.arizona.sista.processors.CorefMention

/**
 * Represents a uniquely identifiable ordered sequence of sentences.
 *
 * @param corefMentions Pairs of co-referent entities across the sentences in this document.
 */
case class Document(id: String,
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
case class Sentence(tokens: Seq[String],
  tags: Option[Seq[String]] = None,
  entities: Option[Seq[String]] = None)

object Sentence {

  /**
   * If present, is the sentence tokens combined based upon the entity information.
   *
   * Successive tokens with the samed named entity label are combined into a single token.
   * Space (" ") is inserted between each combined token.
   */
  def chunkTokens(s: Sentence)(implicit entSet: NamedEntitySet): Option[Seq[String]] =
    s.entities.map(ents =>

      if (ents.size <= 1) {
        ents

      } else {
        val (allChunked, _, lastWorkingSeq) =
          ents.slice(1, ents.size).zip(s.tokens.slice(1, s.tokens.size))
            .foldLeft((Seq.empty[String], ents.head, Seq(s.tokens.head)))({

              case ((chunked, previousEnt, workingSeq), (entity, token)) =>

                val isNonEnt = entity == entSet.nonEntityTag

                val continueToChunk = !isNonEnt && previousEnt == entity

                val updatedWorkingSeq =
                  if (continueToChunk)
                    workingSeq :+ token
                  else
                    Seq(token)

                val updatedChunk = {
                  val c =
                    if (!continueToChunk)
                      if (workingSeq.size > 0)
                        chunked :+ workingSeq.mkString(" ")
                      else
                        chunked
                    else
                      chunked

                  c
                  //                if (isNonEnt) c :+ token else c
                }

                (updatedChunk, entity, updatedWorkingSeq)
            })

        if (lastWorkingSeq.isEmpty)
          allChunked
        else
          allChunked :+ lastWorkingSeq.mkString(" ")
      }
    )

  import edu.arizona.sista.processors.{ Sentence => SistaSentence }

  /** Converts a edu.arizaona.sista.processors.Sentence into a org.rex.Sentence in a straightforward manner. */
  implicit def sistaSentence2Sentence(s: SistaSentence): Sentence =
    Sentence(s.words, s.tags.map(_.toSeq), s.entities.map(_.toSeq))

}

trait Coref extends Serializable {
  /** A chain of mentions, all of which have been determined to be coreferent. */
  def mentions: Seq[Mention]
}

object Coref {
  def apply(corefMentions: Seq[Mention]): Coref =
    new Coref {
      override val mentions = corefMentions
    }
}

trait Mention extends Serializable {
  /** The number, or index, for the sentence in an associated Document. */
  def sentenceNum: Int

  /** The index that the mention starts on. */
  def from: Int

  /** The index immediately after the end of the mention. */
  def until: Int
}

object Mention {

  def apply(sentNum: Int, fromIndex: Int, untilIndexP1: Int): Mention =
    new Mention {
      override val sentenceNum = sentNum
      override val from = fromIndex
      override val until = untilIndexP1
    }

  implicit def sistaCorefMention2Mention(cm: CorefMention): Mention =
    apply(cm.sentenceIndex, cm.headIndex, cm.endOffset)

}
