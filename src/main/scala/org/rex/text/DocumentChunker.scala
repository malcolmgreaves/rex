package org.rex.text

import org.rex.relation_extract.NeTagSet

import scala.language.implicitConversions

object DocumentChunker {
  type Fn = Document => Document
}

case object IdentityDocChunker extends DocumentChunker.Fn {
  override def apply(d: Document) = d
}

case class NerDocChunker(entitySet: NeTagSet) extends DocumentChunker.Fn {

  private val sentChunker = NerSentChunker(entitySet)

  import NerDocChunker._

  override def apply(d: Document): Document = {

    val sentsAndChunkedIndices = d.sentences.map(sentChunker)

    d.corefMentions match {

      case Some(coreferentMentions) =>
        val newCoreferentMentions =
          coreferentMentions
            .map(
              coref =>
                coref.copy(mentions = coref.mentions
                  .map(mention => {

                    val (_, chunkedIndices) =
                      sentsAndChunkedIndices(mention.sentenceNum)

                    val newIndexMapping = old2newIndices(chunkedIndices)

                    mention.copy(
                      from = newIndexMapping(mention.from),
                      until = newIndexMapping(mention.until)
                    )
                  })))

        d.copy(
          sentences = sentsAndChunkedIndices.map(_._1),
          corefMentions = Some(newCoreferentMentions)
        )

      case None =>
        d.copy(sentences = sentsAndChunkedIndices.map(_._1))
    }
  }

}

private object NerDocChunker {

  def old2newIndices(chunkedIndices: Seq[Seq[Int]]): Map[Int, Int] =
    chunkedIndices
      .map(chunk => {
        val newIndex = chunk.head
        chunk.map(x => (x, newIndex))
      })
      .flatten
      .toMap

}
