package org.rex

import edu.arizona.sista.processors.CorefMention


/** Represents a uniquely identifiable ordered sequence of sentences. */
trait Document {
  /** Unique identifier for the document. */
  def id: String

  /** The document's sentences in-order */
  def sentences: Seq[Sentence]

  /** Pairs of co-referent entities across the sentences in this document. */
  def corefMentions: Option[Seq[Coref]]
}

object Document {

  /** Creates a document using an annonymous class */
  def apply(docId: String, docSentences: Seq[Sentence], corefMentionz: Option[Seq[Coref]] = None): Document =
    new Document {
      override val id = docId
      override val sentences = docSentences
      override val corefMentions = corefMentionz
    }


  import edu.arizona.sista.processors.{Document => SistaDocument}

  /** Converts a edu.arizaona.sista.processors.Document into a org.rex.Document in a straightforward manner. */
  implicit def sistaDoc2Doc(idAndDoc: (String, SistaDocument)): Document =
    apply(
      idAndDoc._1,
      idAndDoc._2.sentences.map(Sentence.sistaSentence2Sentence).toSeq,
      idAndDoc._2.coreferenceChains.map(chains =>
        chains.getChains.map(chain =>
          Coref(chain.map(Mention.sistaCorefMention2Mention).toSeq)
        ).toSeq
      )
    )

}

trait Sentence {
  /** Every token in the sentence, in-order */
  def tokens: Seq[String]

  /** If present, has the part-of-speech tag information for each token. */
  def tags: Option[Seq[String]]

  /** If present, has the named entity information for each token. */
  def entities: Option[Seq[String]]
}

object Sentence {

  /** Creates a sentence using an anoymous class. */
  def apply(sTokens: Seq[String],
            sTags: Option[Seq[String]] = None,
            sEntites: Option[Seq[String]] = None): Sentence =
    new Sentence {
      override val tokens = sTokens
      override val tags = sTags
      override val entities = sEntites
    }

  /** If present, is the sentence tokens combined based upon the entity information.
    *
    * Successive tokens with the samed named entity label are combined into a single token.
    * Space (" ") is inserted between each combined token.
    * */
  def chunkTokens(s: Sentence)(implicit entSet: NamedEntitySet): Option[Seq[String]] =
    s.entities.map(ents => {
      val (allChunked, _, lastWorkingSeq) =
        ents.zip(s.tokens).foldLeft((Seq.empty[String], entSet.nonEntityTag, Seq.empty[String]))({
          case ((chunked, previousEnt, workingSeq), (entity, token)) =>

            val isNonEnt = entity == entSet.nonEntityTag

            val continueToChunk = !isNonEnt && entity == previousEnt

            val updatedWorkingSeq =
              if (continueToChunk)
                workingSeq :+ token
              else
                Seq.empty[String]

            val updatedChunk = {
              val c =
                if (!continueToChunk)
                  if (workingSeq.size > 0)
                    chunked :+ workingSeq.mkString(" ")
                  else
                    chunked
                else
                  chunked

              if (isNonEnt) c :+ token else c
            }

            (updatedChunk, entity, updatedWorkingSeq)
        })

      if(lastWorkingSeq.isEmpty)
        allChunked
      else
        allChunked :+ lastWorkingSeq.mkString(" ")
    })

  import edu.arizona.sista.processors.{Sentence => SistaSentence}

  /** Converts a edu.arizaona.sista.processors.Sentence into a org.rex.Sentence in a straightforward manner. */
  implicit def sistaSentence2Sentence(s: SistaSentence): Sentence =
    apply(s.words, s.tags.map(_.toSeq), s.entities.map(_.toSeq))

}


trait Coref {
  /** A chain of mentions, all of which have been determined to be coreferent. */
  def mentions: Seq[Mention]
}

object Coref {
  def apply(corefMentions: Seq[Mention]): Coref =
    new Coref {
      override val mentions = corefMentions
    }
}

trait Mention {
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
