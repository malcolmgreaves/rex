package org.rex

import java.util.Random

import nak.liblinear.{ SolverType, LiblinearConfig }

import scala.language.{ implicitConversions, postfixOps }
import scala.util.Try
import java.io.File
import scala.io.Source

object ReleationExtractionLocal extends App {

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

  //  val pconf = {
  //    val (es, ps) = labeledSentences.foldLeft((Set.empty[String], Set.empty[String])){
  //      case ((e,p), (s, _)) =>
  //        (e ++ s.entities.get, p ++ s.tags.get)
  //    }
  //    ProcessingConf(
  //      entSet = Some(NeTagSet(es, "O")),
  //      tagSet = Some(PosTagSet.DefaultPennTreebank.posSet),
  //      parse = false,
  //      lemmatize = true,
  //      resolveCoreference = false,
  //      discourse = false
  //    )
  //  }
  //
  //  val candidatePipeln = Pipeline(
  //    CoreNlpTextProcessor(pconf),
  //    NerDocChunker(pconf.entSet.get),
  //    {
  //      val wf = WordFilter.noKnownPunct
  //      CorefCandGen(wf,wf)
  //    }
  //  )
  //
  //  val negrel = "nothing"
  val trainingData: RelationLearner.TrainingData =
    labeledSentences
      .filter { _._2 nonEmpty }
      .flatMap {
        case (sentence, relations) =>
          relations.map { r =>
            (CandidateSentence(sentence, r.arg1, r.arg2), r.relation)
          }
      }

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

  val rlearner = RelationLearner(
    LiblinearConfig(
      solverType = SolverType.MCSVM_CS,
      cost = 1.0,
      eps = 0.001,
      showDebug = false
    ),
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
  )

  println(s"Training on ${train.size} instances")
  val (classifier, _) = rlearner(train)

  val numberCorrectPredictions =
    test
      .foldLeft(0) {
        case (nCorrect, (instance, label)) =>
          val predicted = classifier(instance)
          if (predicted == label)
            nCorrect + 1
          else
            nCorrect
      }

  println(s"# correct $numberCorrectPredictions out of ${test.size} : accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}")

}