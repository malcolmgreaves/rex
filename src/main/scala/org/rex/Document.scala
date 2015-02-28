package org.rex


/** Represents a uniquely identifiable ordered sequence of sentences. */
trait Document {
  /** Unique identifier for the document. */
  def id: String
  /** The document's sentences in-order */
  def sentences: Seq[Sentence]
}

object Document {
  /** Creates a document using an annonymous class */
  def apply(docId: String, docSentences: Seq[Sentence]): Document =
    new Document {
      override val id = docId
      override val sentences = docSentences
    }

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
  def chunkTokens(s:Sentence)(implicit entSet: NamedEntitySet): Option[Seq[String]] =
    s.entities.map(ents =>
      ents.zip(s.tokens).foldLeft((Seq.empty[String], entSet.nonEntityTag, Seq.empty[String]))({
        case ((chunked, previousEnt, workingSeq), (entity, token)) =>
          if (previousEnt == entity)
            (chunked,
              previousEnt,
              if (entity != entSet.nonEntityTag) {
                workingSeq :+ token
              } else {
                workingSeq
              })
          else
            (if (workingSeq.size > 0) {
              chunked :+ workingSeq.mkString(" ")
            } else {
              chunked
            },
              entity,
              Seq(token))
      })._1
    )

}
