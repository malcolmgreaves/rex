package org.rex

import scala.language.implicitConversions

object SentenceChunker {

  type Index = Int

  type Fn = Sentence => (Sentence, Seq[Seq[Index]])
}

case object IdentitySentChunker extends SentenceChunker.Fn {
  override def apply(s: Sentence) = (s, Seq.empty[Seq[Int]])
}

case class NerSentChunker(entSet: NeTagSet) extends SentenceChunker.Fn {

  import NerSentChunker._

  override def apply(s: Sentence) =
    s.entities.map(ents =>

      if (ents.size <= 1) {
        (s, Seq.empty[Seq[Int]])

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

        val doChunking = chunk_h(allChunkedIndices) _

        (
          Sentence(
            doChunking(s.tokens, tokenToStr),
            s.tags.map(t => doChunking(t, firstToStr)),
            Some(doChunking(ents, firstToStr))
          ),
            allChunkedIndices
        )
      }
    ).getOrElse((s, Seq.empty[Seq[Int]]))
}

private object NerSentChunker {

  val tokenToStr =
    (l: Seq[String]) =>
      (indices: Seq[Int]) =>
        indices
          .map(index => l(index))
          .mkString(" ")

  val firstToStr =
    (l: Seq[String]) =>
      (indices: Seq[Int]) =>
        indices
          .map(index => l(index))
          .headOption.getOrElse("")

  // Does chunking on l according to the chunked indices (idxs).
  @inline def chunk_h(idxs: Seq[Seq[Int]])(l: Seq[String], toStr: Seq[String] => Seq[Int] => String): Seq[String] =
    idxs
      .foldLeft(Seq.empty[String])({
        case (newChunked, indices) =>
          newChunked :+ toStr(l)(indices)
      })

}