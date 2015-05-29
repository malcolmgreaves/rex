package org.rex

import java.io.File
import java.util.Random

import nak.liblinear.{ LiblinearConfig, SolverType }

import scala.io.Source
import scala.language.{ implicitConversions, postfixOps }
import scala.util.Try

object RelationExtractionUiuaConll extends App {

  sealed trait Line
  case object Break extends Line
  case class TokenLine(sentence: Int, neTag: String, token: Int, posTag: String, word: String) extends Line
  case class RelationLine(arg1: Int, arg2: Int, relation: String) extends Line
  object Line {
    def apply(s: String): Option[Line] = {
      val bits = s.split("\t")
      Try {
        bits.length match {

          case 9 =>
            TokenLine(
              bits(0).toInt,
              bits(1).toUpperCase,
              bits(2).toInt,
              bits(4),
              bits(5)
            )

          case 3 =>
            RelationLine(
              bits(0).toInt,
              bits(1).toInt,
              bits(2)
            )

          case 1 if s == "" =>
            Break
        }
      } toOption
    }
  }

  type LabeledLines = (Seq[TokenLine], Seq[RelationLine])

  def lineAggregate(lines: Traversable[Line]): Seq[LabeledLines] =
    lines
      .foldLeft((Seq.empty[LabeledLines], Seq.empty[TokenLine], Seq.empty[RelationLine], false)) {
        case ((labeled, workingToks, workingRels, brokenBefore), l) => l match {

          case Break =>
            if (!brokenBefore)
              (labeled, workingToks, workingRels, true)
            else
              (
                labeled :+ ((workingToks, workingRels)),
                Seq.empty[TokenLine],
                Seq.empty[RelationLine],
                false
              )

          case tl: TokenLine =>
            (labeled, workingToks :+ tl, workingRels, brokenBefore)

          case rl: RelationLine =>
            (labeled, workingToks, workingRels :+ rl, brokenBefore)
        }
      }._1

  val inputFile = new File(
    args.headOption
      .getOrElse("/Users/mgreaves/data/uiuc/entity_and_relation_regcognition_corpora/conll04.corp")
  )
  println(s"Reading input from:\n$inputFile")
  val input = Source.fromFile(inputFile)

  def cleanWord(word: String) =
    word.replaceAll("/", " ")

  //
  // Seq[Sentence,Relation]
  //

  def sentenceFrom(tls: Seq[TokenLine]) = Sentence(
    tls.map(_.word).map(cleanWord),
    Some(tls.map(_.posTag)),
    Some(tls.map(_.neTag))
  )

  val rawLines = input.getLines().flatMap(Line.apply).toSeq

  println(s"Obtained ${rawLines.size} individual lines")

  val groupedLines = lineAggregate(rawLines)

  println(s"Obtained ${groupedLines.size} grouped lines")

  val labeledSentences: Seq[(Sentence, Seq[RelationLine])] =
    groupedLines.map(x => x.copy(_1 = sentenceFrom(x._1)))

  println(s"Obtained ${labeledSentences.size} sentences")
  println(s"Of those, ${labeledSentences.filter(_._2.nonEmpty).size} are labeled")

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


  val negrel = "nothing"

  val candgen = SentenceCandGen(WordFilter.noKnownPunct)

  val trainingData: RelationLearner.TrainingData =
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
                if(!anyIndexPairs.contains((candidate.queryIndex, candidate.answerIndex)))
                  Some((candidate, negrel))
                else
                  None
              })

        labeled ++ unlabeled
      }

  println(s"A total of ${trainingData.size} candidates, of which ${trainingData.count(_._2 == negrel)} are unlabeled.")

  val (train, test) = {
    val rand = new Random()
    val grouped =
      trainingData
        .map(x => (x, rand.nextInt(2)))
        .toList
        .groupBy(_._2)

    println(s"grouped keyset: ${grouped.keySet}")

    (grouped(0).map(_._1), grouped(1).map(_._1))
  }

  val relations = trainingData.map(_._2).toSet

  val llConf = LiblinearConfig(
    solverType = SolverType.L1R_L2LOSS_SVC,
    cost = 12.0,
    eps = 0.001,
    showDebug = false
  )

  val featurizer =
    CandidateFeatuerizer(
      Some((
        AdjacentFeatures(2),
        SentenceViewFilter.noKnownPunctLowercase
      )),
      Some((
        InsideFeatures(2, 4),
        WordFilter.noKnownPunct,
        WordView.lowercase
      ))
    )

  val rlearners =
    relations
      .map(r => (r, RelationLearner(llConf, featurizer)))
      .filter(_._1 == negrel)
      .toMap

  println(s"Training on ${train.size} instances, one binary SVM per relation (${relations.size} relations)")

  val estimators =
    rlearners
      .map {
        case (r, rl) =>
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

  val numberCorrectPredictions =
    test
      .foldLeft(0) {
        case (nCorrect, (instance, label)) =>

          val predicted =
            Learning.argmax(
              estimators
                .map { case (r, estimator) => (r, estimator(instance).result.head) }
                .toSeq
            )(Learning.TupleVal2[String])._1

          if (predicted == label)
            nCorrect + 1
          else
            nCorrect
      }

  println(s"# correct $numberCorrectPredictions out of ${test.size} : accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}")

}