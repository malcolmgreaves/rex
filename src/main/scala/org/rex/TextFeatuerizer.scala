package org.rex

import nak.data.{ FeatureObservation, Featurizer }
import org.rex.AdjacentFeatures._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import spire.syntax.cfor._

trait TextFeatuerizer extends Featurizer[Candidate, String]

object TextFeatuerizer {

  implicit def convFeatuerizer(f: Featurizer[Candidate, String]): TextFeatuerizer =
    new TextFeatuerizer {
      override def apply(v1: Candidate): Seq[FeatureObservation[String]] = f.apply(v1)
    }

  private type SentViewFilt = Seq[(String, Boolean)]

  private val nothingStrBool: SentViewFilt = IndexedSeq.empty[(String, Boolean)]

  /**
   * Creates a Featuerizer[Candidate,String] instance that creates ngram and skip-ngram
   * based features using information within the document that the Candidate comes from.
   *
   * The AdjacentFeatures and InsideFeatures configurations determine the evaluated
   * Featuerizer's behavior. Settings a configuration value to None implies to
   * not perform that type of featuerization. E.g. if insideConf is None, then skip-ngram
   * features will not be created. Similarily for adjacentConf and ngram features. The
   * default is to use bigrams for adjacent features and 4-skip bigrams for inside features.
   *
   * The wordView function determines how to interpret each element of a sentence. The default
   * is to only use the lower-cased version of the token. Alternatives include POS or NE tags,
   * or combinations thereof.
   */
  def apply(
    adjacentConf: Option[(AdjacentFeatures, SentenceViewFilter)],
    insideConf: Option[(InsideFeatures, WordFilter, WordView)]): TextFeatuerizer =

    new Featurizer[Candidate, String] {

      override def apply(cand: Candidate): Seq[FeatureObservation[String]] =
        (makeAdjacentFeatures(cand) ++ makeInsideFeatures(cand))
          .foldLeft(Map.empty[String, Double])(
            (fmap, feature) => fmap.get(feature) match {

              case Some(value) =>
                (fmap - feature) + (feature -> (value + 1.0))

              case None =>
                fmap + (feature -> 1.0)
            }
          )
          .map({
            case (feature, value) =>
              FeatureObservation[String](feature, value)
          })
          .toSeq

      @inline private def adjFeats1Sent(words: SentViewFilt, qIdx: Int, aIdx: Int): Int => Seq[SentViewFilt] = {
        val (lIndex, rIndex) =
          if (qIdx < aIdx)
            (qIdx, aIdx)
          else
            (aIdx, qIdx)

        (ngramSize: Int) => Seq(
          if (lIndex - ngramSize < 0)
            nothingStrBool
          else
            left(words, lIndex)(ngramSize),
          if (rIndex + ngramSize >= words.size - 1)
            nothingStrBool
          else
            right(words, rIndex)(ngramSize)
        )
      }

      private val makeAdjacentFeatures: Candidate => Seq[String] = adjacentConf match {

        case Some((AdjacentFeatures(width), sentViewFilt)) => {

          (cand: Candidate) =>
            {

              val createAdjacentFeaturesAtSize: Int => Seq[Seq[String]] = cand match {

                case CandidateSentence(sentence, queryIndex, answerIndex) => {

                  val sentenceFeatures =
                    adjFeats1Sent(
                      sentViewFilt(sentence),
                      queryIndex,
                      answerIndex
                    )

                  (ngramSize: Int) =>
                    sentenceFeatures(ngramSize)
                      .map(_.filter(_._2).map(_._1))
                      .filter(_.nonEmpty)
                }

                case CandidateCorefQuery(doc, query, shared, queryCoref, answer) => {

                  val sharedFeatures =
                    adjFeats1Sent(
                      sentViewFilt(doc.sentences(shared)),
                      queryCoref,
                      answer
                    )

                  val featuresAroundQuery =
                    adjFeats1Sent(
                      sentViewFilt(doc.sentences(query.sentNum)),
                      query.wordIndex,
                      query.wordIndex
                    )

                  (ngramSize: Int) =>
                    (sharedFeatures(ngramSize) ++ featuresAroundQuery(ngramSize))
                      .map(_.filter(_._2).map(_._1))
                      .filter(_.nonEmpty)
                }

                case CandidateCorefAnswer(doc, query, shared, answerCoref, answer) => {

                  val sharedFeatures =
                    adjFeats1Sent(
                      sentViewFilt(doc.sentences(shared)),
                      query,
                      answerCoref
                    )

                  val featuresAroundAnswer =
                    adjFeats1Sent(
                      sentViewFilt(doc.sentences(answer.sentNum)),
                      answer.wordIndex,
                      answer.wordIndex
                    )

                  (ngramSize: Int) =>
                    (sharedFeatures(ngramSize) ++ featuresAroundAnswer(ngramSize))
                      .map(_.filter(_._2).map(_._1))
                      .filter(_.nonEmpty)
                }
              }

              // the rest of code inside the next block {...} is all equivalent to:
              //                        (1 until width + 1)
              //                          .flatMap(createAdjacentFeaturesAtSize)
              //                          .filter(_.nonEmpty)
              //                          .map(features => s"""${features.mkString(",")}""")
              { // we use local mutable operations for efficiency
                val buff = new ArrayBuffer[String](width * 2)

                cfor(0)(_ < width + 1, _ + 1) { i =>

                  val featuresForI = createAdjacentFeaturesAtSize(i)

                  cfor(0)(_ < featuresForI.size, _ + 1) { j =>
                    val featuresForJ = featuresForI(j)
                    if (featuresForJ.nonEmpty) {
                      buff.append(s"""${featuresForJ.mkString(",")}""")
                    }
                  }
                }
                buff.toSeq
              }
            }
        }

        case None => {
          (ignore: Candidate) => nothingStr
        }
      }

      private val makeInsideFeatures: Candidate => Seq[String] = insideConf match {

        case Some((inside, wordFilter, wordView)) =>
          (cand: Candidate) => {

            val wf = wordFilter(cand.innerFromSentence) _
            val wv = wordView(cand.innerFromSentence) _

            // the following code to compute innerFiltered is equivalent to:
            //              val innerFiltered =
            //                (cand.startInnerIndex until cand.endInnerIndex)
            //                   .flatMap(i =>
            //                    if (wf(i))
            //                      Some(cand.innerFromSentence.tokens(i))
            //                    else
            //                      None
            //                  )
            // use encapsulated mutability for performance
            val innerFiltered = {
              val buff = new ArrayBuffer[String](cand.endInnerIndex - cand.startInnerIndex)

              cfor(cand.startInnerIndex)(_ < cand.endInnerIndex, _ + 1) { i =>
                if (wf(i)) {
                  buff.append(wv(i))
                }
              }

              buff.toSeq
            }

            if (innerFiltered.nonEmpty)
              InsideFeatures(inside)(innerFiltered.toSeq)
            else
              nothingStr
          }

          case None =>
          (ignore: Candidate) => nothingStr
      }
    }

  private val nothingStr = IndexedSeq.empty[String]
}
