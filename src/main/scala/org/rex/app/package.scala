package org.rex

import java.util.Random

import org.rex.app.Connl04Format._

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.language.existentials

package object app {

  def mkStdCandGen(
      labeledSentences: Reader[A forSome { type A }, LabeledSentence]#Readable)
    : SentenceCandGen = {

    val pconf = {
      val (es, ps) =
        labeledSentences.foldLeft((Set.empty[String], Set.empty[String])) {
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

      (s: Sentence) => (index: Int) =>
        s.tags
          .map(posTags => okTags contains posTags(index))
          .getOrElse(WordFilter.noKnownPunct(s)(index))
    }
  }

  def mkTrainData(
      candgen: SentenceCandGen,
      labeledSentences: Reader[A forSome { type A }, LabeledSentence]#Readable,
      noRelationPresent: RelationLearner.Label): RelationLearner.TrainingData =
    labeledSentences.flatMap {
      case (sentence, relz) =>
        val labeled =
          relz.map { r =>
            (CandidateSentence(sentence, r.arg1, r.arg2), r.relation)
          }

        val anyIndexPairs = relz.map(r => (r.arg1, r.arg2)).toSet

        val unlabeled =
          candgen(Document("", Seq(sentence)))
            .flatMap(candidate => {
              if (!anyIndexPairs.contains(
                    (candidate.queryIndex, candidate.answerIndex)))
                Some((candidate, noRelationPresent))
              else
                None
            })

        labeled ++ unlabeled
    }.toIterable

  def mkPositiveTrainData(
      labeledSentences: Reader[A forSome { type A }, LabeledSentence]#Readable)
    : RelationLearner.TrainingData =
    labeledSentences
      .flatMap {
        case (sentence, relz) =>
          relz.map { r =>
            (CandidateSentence(sentence, r.arg1, r.arg2), r.relation)
          }
      }

  type Train = RelationLearner.TrainingData
  type Test = RelationLearner.TrainingData

  def mkCrossValid(labeledData: RelationLearner.TrainingData, nFolds: Int)(
      implicit rand: Random): Traversable[(Train, Test)] = {

    val partitions = shuffleAssign(
      labeledData,
      (r: Random) => () => r.nextInt(nFolds)
    )

    (0 until nFolds)
      .map(
        fold =>
          (
            partitions
              .filter { case (index, _) => index != fold }
              .map(_._2)
              .toTraversable
              .flatten,
            partitions(fold)
        ))
      .toTraversable
  }

  def shuffleAssign(labeledData: RelationLearner.TrainingData,
                    randAssign: Random => () => Int)(implicit rand: Random) = {

    val rAssign = randAssign(rand)

    labeledData
      .map(x => (x, rAssign()))
      .toList
      .groupBy(_._2)
      .map { case (fold, dataPart) => (fold, dataPart.map(_._1).toTraversable) }
  }

  def trainTestSplit(labeledData: RelationLearner.TrainingData,
                     proportionTrain: Double)(
      implicit rand: Random): Traversable[(Train, Test)] = {

    val partitioned = shuffleAssign(
      labeledData,
      (r: Random) =>
        () =>
          if (r.nextDouble() < proportionTrain)
            0
          else
        1
    )

    Seq((partitioned(0), partitioned(1)))
  }

  val noRelation: RelationLearner.Label =
    "no_relation"

  val negrel: RelationLearner.Label =
    "negative"

  type MultiLearner = Map[RelationLearner.Label, RelationLearner.Learner]

  type MultiEstimator = Map[RelationLearner.Label, RelationLearner.Estimator]

  def trainLearners(rlearners: MultiLearner,
                    train: RelationLearner.TrainingData)(
      implicit ec: ExecutionContext): MultiEstimator =
    Await
      .result(
        Future.sequence(
          rlearners.toSeq
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
      )
      .toMap

}
