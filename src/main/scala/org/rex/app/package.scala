package org.rex

import java.util.Random

import org.rex.app.Connl04Format._

import scala.concurrent.duration.Duration
import scala.concurrent.{ Future, Await, ExecutionContext }

package object app {

  def mkStdCandGen(labeledSentences: Reader[A forSome { type A }, LabeledSentence]#Readable): SentenceCandGen = {

    val pconf = {
      val (es, ps) = labeledSentences.foldLeft((Set.empty[String], Set.empty[String])) {
        case ((e, p), (s, _)) =>
          (e ++ s.entities.get, p ++ s.tags.get)
      }
      ProcessingConf(
        entSet = Some(NeTagSet(es, "O")),
        tagSet = Some(PosTagSet.DefaultPennTreebank.posSet),
        parse = false,
        lemmatize = true,
        resolveCoreference = false,
        discourse = false
      )
    }

    SentenceCandGen {
      val okTags = pconf.tagSet.get.nouns ++ pconf.tagSet.get.pronouns

      (s: Sentence) =>
        (index: Int) =>
          s.tags
            .map(posTags => okTags contains posTags(index))
            .getOrElse(WordFilter.noKnownPunct(s)(index))
    }
  }

  def mkTrainData(
    candgen: SentenceCandGen,
    labeledSentences: Reader[A forSome { type A }, LabeledSentence]#Readable): RelationLearner.TrainingData =

    labeledSentences
      .flatMap {
        case (sentence, relz) =>

          val labeled =
            relz.map { r =>
              (CandidateSentence(sentence, r.arg1, r.arg2), r.relation)
            }

          val anyIndexPairs = relz.map(r => (r.arg1, r.arg2)).toSet

          val unlabeled =
            candgen(Document("", Seq(sentence)))
              .flatMap(candidate => {
                if (!anyIndexPairs.contains((candidate.queryIndex, candidate.answerIndex)))
                  Some((candidate, negrel))
                else
                  None
              })

          labeled ++ unlabeled
      }
      .toIterable

  def makeBinary(max: Int)(v: Int): Int =
    if (v >= max - 1) 1 else 0

  type Train = RelationLearner.TrainingData
  type Test = RelationLearner.TrainingData

  def mkCrossValid(
    labeledData: RelationLearner.TrainingData,
    nFolds: Int)(implicit rand: Random): Traversable[(Train, Test)] = {

    val partitions =
      labeledData
        .map(x => (x, makeBinary(nFolds)(rand.nextInt(nFolds))))
        .toList
        .groupBy(_._2)
        .map { case (fold, dataPart) => (fold, dataPart.map(_._1).toTraversable) }
    println(s"partitions keyset: ${partitions.keySet}")

    (0 until nFolds - 1)
      .map(fold =>
        (
          partitions
          .filter { case (index, _) => index != fold }
          .map(_._2)
          .toTraversable
          .flatten,
          partitions(fold)
        )
      )
      .toTraversable
  }

  val negrel: RelationLearner.Label = "nothing"

  type MultiLearner = Map[RelationLearner.Label, RelationLearner.Learner]

  type MultiEstimator = Map[RelationLearner.Label, RelationLearner.Estimator]

  def trainLearners(
    rlearners: MultiLearner,
    train: RelationLearner.TrainingData)(implicit ec: ExecutionContext): MultiEstimator =
    Await.result(
      Future.sequence(
        rlearners
          .toSeq
          .map {
            case (r, rl) =>
              Future {
                (
                  r,
                  rl(
                    train.map {
                      case (inst, lab) =>
                        if (lab == r) (inst, r) else (inst, negrel)
                    }
                  )._2
                )
              }
          }
      ),
      Duration.Inf
    ).toMap

}
