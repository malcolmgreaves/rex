package org.rex.text

import scala.language.implicitConversions

object SentenceChunker {

  type Index = Int

  type Fn = Sentence => (Sentence, Seq[Seq[Index]])
}

case object IdentitySentChunker extends SentenceChunker.Fn {
  override def apply(s: Sentence): (Sentence, Seq[Seq[Int]]) =
    (s, Seq.empty[Seq[Int]])
}

case class NerSentChunker(entSet: NeTagSet) extends SentenceChunker.Fn {

  import NerSentChunker._

  val isNonEntity: (String) => Boolean =
    _ == entSet.nonEntityTag

  override def apply(s: Sentence): (Sentence, Seq[Seq[Int]]) =
    s.entities
      .map { ents =>
        if (ents.size <= 1) {
          (s, Seq.empty[Seq[Int]])

        } else {

          val (chunkedIndices, _, lastWorkingIndices) =
            ents
              .slice(1, ents.size)
              .zip(s.tokens.slice(1, s.tokens.size))
              .zipWithIndex
              .map { case ((e, t), indexMinus1) => (e, t, indexMinus1 + 1) }
              .foldLeft((Seq.empty[Seq[Int]], ents.head, Seq(0)))({

                case ((indicesChunked, previousEnt, workingIndices), (entity, token, index)) =>
                  val continueToChunk = !isNonEntity(entity) && previousEnt == entity

                  val updatedWorkingIndices =
                    if (continueToChunk)
                      workingIndices :+ index
                    else
                      Seq(index)

                  val updatedIndices =
                    if (!continueToChunk)
                      if (workingIndices.nonEmpty)
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

          val doChunking = chunk_h(allChunkedIndices) _

          (
            Sentence(
              doChunking(s.tokens, tokenToStr), // tokens
              s.tags.map(t => doChunking(t, firstToStr)), // pos tags
              Some(doChunking(ents, firstToStr)) // named entities
            ),
            allChunkedIndices
          )
        }
      }
      .getOrElse((s, Seq.empty[Seq[Int]]))
}

private object NerSentChunker {

  // for the token case
  def tokenToStr(tokens: Seq[String])(indices: Seq[Int]): String =
    indices
      .map { tokens.apply }
      .mkString(" ")

  // for the POS tag & NE tag cases
  def firstToStr(tokens: Seq[String])(indices: Seq[Int]): String =
    indices
      .map { tokens.apply }
      .headOption
      .getOrElse("")

  type Tokens2Str = Seq[String] => Seq[Int] => String

  // Does chunking on l according to the chunked indices (idxs).
  @inline
  def chunk_h(idxs: Seq[Seq[Int]])(tokens: Seq[String], toStr: Tokens2Str): Seq[String] = {
    val select = toStr(tokens)
    idxs
      .foldLeft(Seq.empty[String])({
        case (newChunked, indices) =>
          newChunked :+ select(indices)
      })
  }
}
