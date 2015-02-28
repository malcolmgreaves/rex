package org.rex

import nak.data.{FeatureObservation, Featurizer}

object TextFeatuerizer {

  private val nothing = IndexedSeq.empty[String]

  def apply(
             adjacentConf: Option[AdjacentFeatures] = Some(AdjacentFeatures(2)),
             insideConf: Option[(InsideFeatures, WordFilter)] = Some(InsideFeatures(2, 4), WordFilter.default),
             wordView: WordView = WordView.default): Featurizer[Candidate, String] =

    new Featurizer[Candidate, String] {

      import AdjacentFeatures._

      def apply(cand: Candidate): Seq[FeatureObservation[String]] = {

        val adjacentFeatures = adjacentConf match {

          case Some(AdjacentFeatures(width)) =>

            val adjFeatFn = cand match {

              case CandidateSentence(sentence, queryIndex, answerIndex) =>
                val viewableWords = sentence.words.map(wordView)
                val (l, r) =
                  if (queryIndex < answerIndex)
                    (left(viewableWords, queryIndex) _, right(viewableWords, answerIndex) _)
                  else
                    (left(viewableWords, answerIndex) _, right(viewableWords, queryIndex) _)
                (ngramSize: Int) =>
                  Seq(l(ngramSize), r(ngramSize))

              case CandidateCorefQuery(doc, query, _, _, _) =>
                val viewableQWords = doc.sentences(query.sentNum).words.map(wordView)
                (ngramSize: Int) => Seq(
                  left(viewableQWords, query.wordIndex)(ngramSize),
                  right(viewableQWords, query.wordIndex)(ngramSize)
                )

              case CandidateCorefAnswer(doc, _, _, _, answer) =>
                val viewableAWords = doc.sentences(answer.sentNum).words.map(wordView)
                (ngramSize: Int) => Seq(
                  left(viewableAWords, answer.wordIndex)(ngramSize),
                  right(viewableAWords, answer.wordIndex)(ngramSize)
                )

              case CandidateCorefBoth(doc, query, qCorefIndex, sharedSentNum, aCorefIndex, answer) =>
                val viewableQWords = doc.sentences(query.sentNum).words.map(wordView)
                val viewableAWords = doc.sentences(answer.sentNum).words.map(wordView)
                val (l, r) = {
                  val viewableSharedWords = doc.sentences(sharedSentNum).words.map(wordView)
                  if (qCorefIndex < aCorefIndex)
                    (left(viewableSharedWords, qCorefIndex) _, right(viewableSharedWords, aCorefIndex) _)
                  else
                    (left(viewableSharedWords, aCorefIndex) _, right(viewableSharedWords, qCorefIndex) _)
                }
                (ngramSize: Int) => Seq(
                  left(viewableQWords, query.wordIndex)(ngramSize),
                  right(viewableQWords, query.wordIndex)(ngramSize),
                  left(viewableAWords, answer.wordIndex)(ngramSize),
                  right(viewableAWords, answer.wordIndex)(ngramSize),
                  l(ngramSize),
                  r(ngramSize)
                )
            }

            (1 until width + 1).flatMap(adjFeatFn).flatten

          case None =>
            nothing
        }

        val insideFeatures = insideConf match {

          case Some((inside, wordFilter)) =>

            val filteredViewableInside = cand.inner.filter(wordFilter).map(wordView)

            if (filteredViewableInside.size > 0)
              InsideFeatures(inside)(filteredViewableInside)
            else
              nothing

          case None =>
            nothing
        }

        (adjacentFeatures ++ insideFeatures).foldLeft(Map.empty[String, Double])(
          (fmap, feature) => fmap.get(feature) match {
            case Some(value) => (fmap - feature) + (feature -> (value + 1.0))
            case None => fmap + (feature -> 1.0)
          }
        ).map({
          case (feature, value) => FeatureObservation[String](feature, value)
        }).toSeq
      }
    }
}

object AdjacentFeatures {

  def left(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] = {
    val leftmostBoundary = s.size - ngramWidth
    if (leftmostBoundary >= 0)
      s.slice(leftmostBoundary, index)
    else
      IndexedSeq.empty[String]
  }

  def right(s: Seq[String], index: Int)(ngramWidth: Int): Seq[String] = {
    val rightmostBoundary = s.size + ngramWidth + 1
    if (rightmostBoundary <= s.size)
      s.slice(index + 1, rightmostBoundary)
    else
      IndexedSeq.empty[String]
  }
}

case class AdjacentFeatures(ngramWidth: Int)

object InsideFeatures {

  def apply(inside: InsideFeatures)(inner: Seq[String]): Seq[String] =
    if (inner.size == 0) {
      Seq.empty[String]
    } else {
      val end = inner.size - inside.ngramWidth + 2
      (0 until end).flatMap(sliceStart =>
        selectKSkipGram(inner.slice(sliceStart, inner.size), inside.ngramWidth, inside.skipSize)
      )
    }

  def selectKSkipGram(s: Seq[String], n: Int, k: Int): Seq[String] = {
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
      override def apply(s:Sentence)(i:Int): Boolean = fn(s)(i)
    }

  lazy val default: WordFilter =
    (s: Sentence) => s.tags match {
      case Some(tags) => (i:Int) => tags(i) != s.tokens(i)
      case None => (ignore:Int) => true
    }
}

trait WordFilter {
  def apply(s: Sentence)(i:Int): Boolean
}

object WordView {

  implicit def fn2wordView(fn: Sentence => Int => String): WordView =
    new WordView {
      override def apply(s:Sentence)(i:Int): String = fn(s)(i)
    }

  lazy val default: WordView =
    (s:Sentence) => (i:Int) => s.tokens(i)
}

trait WordView {
  def apply(s:Sentence)(i:Int): String
}