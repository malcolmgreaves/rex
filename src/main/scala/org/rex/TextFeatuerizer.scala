package org.rex

import nak.data.{FeatureObservation, Featurizer}

object TextFeatuerizer {

  private val nothing = IndexedSeq.empty[String]

  def apply(
             adjacentConf: Option[AdjacentFeatures] = Some(AdjacentFeatures(2)),
             insideConf: Option[(InsideFeatures, WordFilter)] = Some(InsideFeatures(2, 4), WordFilter.default),
             wordView: WordView = WordView.default): Featurizer[Candidate, String] =

    new Featurizer[Candidate, String] {

      def apply(cand: Candidate): Seq[FeatureObservation[String]] =
        (makeAdjacentFeatures(cand) ++ makeInsideFeatures(cand))
          .foldLeft(Map.empty[String, Double])(
            (fmap, feature) => fmap.get(feature) match {
              case Some(value) =>
                (fmap - feature) + (feature -> (value + 1.0))
              case None =>
                fmap + (feature -> 1.0)
            }
          ).map({
          case (feature, value) =>
            FeatureObservation[String](feature, value)
        }).toSeq

      private val makeAdjacentFeatures = adjacentConf match {

        case Some(AdjacentFeatures(width)) =>

          import AdjacentFeatures.{left, right}

          @inline def candgen(cand: Candidate): Seq[String] = {

            val adjFeatFn = cand match {

              case CandidateSentence(sentence, queryIndex, answerIndex) =>
                val words = viewableWords(sentence)
                val (l, r) =
                  if (queryIndex < answerIndex)
                    (left(words, queryIndex) _, right(words, answerIndex) _)
                  else
                    (left(words, answerIndex) _, right(words, queryIndex) _)
                (ngramSize: Int) =>
                  Seq(l(ngramSize), r(ngramSize))

              case CandidateCorefQuery(doc, query, _, _, _) =>
                val viewableQWords = viewableWords(doc.sentences(query.sentNum))
                (ngramSize: Int) => Seq(
                  left(viewableQWords, query.wordIndex)(ngramSize),
                  right(viewableQWords, query.wordIndex)(ngramSize)
                )

              case CandidateCorefAnswer(doc, _, _, _, answer) =>
                val viewableAWords = viewableWords(doc.sentences(answer.sentNum))
                (ngramSize: Int) => Seq(
                  left(viewableAWords, answer.wordIndex)(ngramSize),
                  right(viewableAWords, answer.wordIndex)(ngramSize)
                )
            }

            (1 until width + 1).flatMap(adjFeatFn).flatten
          }
          candgen _

        case None =>
          @inline def candgen(ignore: Candidate): Seq[String] =
            nothing
          candgen _
      }

      private val makeInsideFeatures = insideConf match {

        case Some((inside, wordFilter)) =>

          (cand: Candidate) => {

            val wf = wordFilter(cand.innerFromSentence) _

            val innerFiltered =
              (cand.startInnerIndex until cand.endInnerIndex)
                .flatMap(i =>
                  if(wf(i))
                    Some(cand.innerFromSentence.tokens(i))
                  else
                    None
                )

            if(innerFiltered.size > 0)
              InsideFeatures(inside)(innerFiltered)
            else
              nothing
          }

        case None =>
          (ignore: Candidate) => nothing
      }

      @inline private def viewableWords(s: Sentence): Seq[String] = {
        val wv = wordView(s) _
        var i = 0
        val size = s.tokens.size
        val retSeq = Array.fill[String](size)("")
        while (i < size) {
          retSeq(i) = wv(i)
          i += 1
        }
        retSeq.toSeq
      }
    }
}

object AdjacentFeatures {

  @inline def left(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] = {
    val leftmostBoundary = s.size - ngramWidth
    if (leftmostBoundary >= 0)
      s.slice(leftmostBoundary, index)
    else
      IndexedSeq.empty[String]
  }

  @inline def right(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] = {
    val rightmostBoundary = s.size + ngramWidth + 1
    if (rightmostBoundary <= s.size)
      s.slice(index + 1, rightmostBoundary)
    else
      IndexedSeq.empty[String]
  }
}

case class AdjacentFeatures(ngramWidth: Int)

object InsideFeatures {

  @inline def apply(inside: InsideFeatures)(inner: Seq[String]): Seq[String] =
    if (inner.size == 0) {
      Seq.empty[String]
    } else {
      val end = inner.size - inside.ngramWidth + 2
      (0 until end).flatMap(sliceStart =>
        selectKSkipGram(inner.slice(sliceStart, inner.size), inside.ngramWidth, inside.skipSize)
      )
    }

  @inline private def selectKSkipGram(s: Seq[String], n: Int, k: Int): Seq[String] = {
    val first = s(0)
    if (n <= 1)
      Seq(first)
    else
      Seq(first) ++
        (0 until Math.min(k + 1, s.size)).flatMap(j => {
          val rest = s.slice(j + 1, s.size)
          if (rest.size > 0)
            selectKSkipGram(rest, n - 1, k - j).map(gram => s"$first,$gram")
          else
            Seq.empty[String]
        })
  }
}

case class InsideFeatures(ngramWidth: Int, skipSize: Int)

object WordFilter {

  implicit def fn2wordFilter(fn: Sentence => Int => Boolean): WordFilter =
    new WordFilter {
      override def apply(s: Sentence)(i: Int): Boolean = fn(s)(i)
    }

  lazy val default: WordFilter =
    (s: Sentence) => s.tags match {
      case Some(tags) => (i: Int) => tags(i) != s.tokens(i)
      case None => (ignore: Int) => true
    }
}

trait WordFilter {
  def apply(s: Sentence)(i: Int): Boolean
}

object WordView {

  implicit def fn2wordView(fn: Sentence => Int => String): WordView =
    new WordView {
      override def apply(s: Sentence)(i: Int): String = fn(s)(i)
    }

  lazy val default: WordView =
    (s: Sentence) => (i: Int) => s.tokens(i)
}

trait WordView {
  def apply(s: Sentence)(i: Int): String
}