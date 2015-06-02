package org.rex

import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit

import edu.stanford.nlp.io.EncodingPrintWriter.out
import nak.liblinear.{ LiblinearConfig, SolverType }
import org.apache.log4j.Level

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.io.Source
import scala.language.{existentials, implicitConversions, postfixOps}
import scala.util.Try

object RelationExtractionLearningMain extends App {

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

  sealed trait Reader {

    type InputSource
    type Element

    type Fn = InputSource => Iterator[Element]
  }

  object ReaderMap {

    def apply(s: String): Option[(T#Fn, T) forSome {type T <: Reader}] =
      s match {
        case "conll" =>
          Some((LabeledConll04Reader.read, LabeledConll04Reader))

        case _ =>
          None
      }
  }

  type LabeledSentence = (Sentence, Seq[RelationLine])

  case object LabeledConll04Reader extends Reader { //[File, LabeledSentence] {

    override type InputSource = File

    override type Element = LabeledSentence

    def cleanWord(word: String): String =
      word.replaceAll("/", " ")

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

    def sentenceFrom(tls: Seq[TokenLine]): Sentence =
      Sentence(
        tls.map(_.word).map(cleanWord),
        Some(tls.map(_.posTag)),
        Some(tls.map(_.neTag))
      )

    val read: Fn =
      (inFi: File) => {
        println(s"Reading input from:\n$inFi")
        val input = Source.fromFile(inFi)

        val rawLines = input.getLines().flatMap(Line.apply).toSeq
        println(s"Obtained ${rawLines.size} individual lines")

        val groupedLines = lineAggregate(rawLines)
        println(s"Obtained ${groupedLines.size} grouped lines")

        val labeledSentences: Seq[(Sentence, Seq[RelationLine])] =
          groupedLines
            .map(x => x.copy(_1 = sentenceFrom(x._1)))
        println(s"Obtained ${labeledSentences.size} sentences")
        println(s"Of those, ${labeledSentences.count(_._2.nonEmpty)} are labeled")

        labeledSentences.toIterator
      }
  }

  sealed trait Command

  case class LearningCmd(
    labeledInput: File,
    reader: Reader#Fn,
    modelOut: Option[File]) extends Command

  object LearningCmd {
    def emptyUnsafe = LearningCmd(null, null, None)
  }

  case class Evaluation(
    labeledInput: File,
    reader: Reader#Fn,
    modelIn: Option[File],
    evalOut: Option[File])

  object Evaluation {
    def emptyUnsafe = Evaluation(null, null, None, None)
  }

  case class Extraction(
    rawInput: File,
    reader: Reader#Fn,
    modelIn: Option[File],
    extractOut: Option[File]) extends Command

  object Extraction {
    def emptyUnsafe = Extraction(null, null, None, None)
  }

  case class RelConfig(
    lr: Option[LearningCmd],
    ev: Option[Evaluation],
    ex: Option[Extraction])

  object RelConfig {
    def empty = RelConfig(None, None, None)
  }

  val parser = new scopt.OptionParser[RelConfig]("scopt") {
    head("Relation Learning and Extraction")
    note("some notes.\n")
    help("help")
      .text("prints this usage text")
    cmd("learning")
      .optional()
      .action { (_, c) => c.copy(lr = Some(LearningCmd.emptyUnsafe)) }
      .text("Perform relation learning.")
      .children(
        opt[File]("labeledInput")
          .abbr("li")
          .valueName("<file>")
          .action { (li, c) =>
            c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
              .copy(labeledInput = li)))
          }
          .text("Input of labeled relation data."),
        opt[String]("learnReader")
          .abbr("lr")
          .valueName("typeOf[Reader]")
          .action { (readerStrInput, c) =>
            ReaderMap(readerStrInput).map { r =>
              c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
                .copy(reader = r._1)))
            }.getOrElse(c)
          }
          .text("Input (training) data file format reader type (conll)"),
        opt[File]("modelOut")
          .abbr("mo")
          .optional()
          .valueName("<file>")
          .action { (mo, c) =>
            c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe).copy(modelOut = Some(mo))))
          }
          .text("Path to where the trained relation classifier and pipeline config should be saved to.")
      )
    cmd("evaluation")
      .optional()
      .action { (_, c) => c.copy(ev = Some(Evaluation.emptyUnsafe)) }
      .text("Evaluate a relation learning model.")
      .children()
    //      .checkConfig { c =>
    //      if (true)
    //        failure("xyz cannot keep alive")
    //      else
    //        success
    //    }

  }

  // parser.parse returns Option[C]
  parser.parse(args, RelConfig.empty) match {

    case Some(config) =>
      val maybeModelTrainData =
        config.lr.map {
          case LearningCmd(labeledInput, reader, modelOut) =>

            reader(labeledInput)

            ???
        }

    case None =>
      // arguments are bad, error message will have been displayed
      println("Bad arguments. Exiting.")
      System.exit(1)
  }

  //  val inputFile = new File(
  //    args.headOption
  //      .getOrElse("/Users/mgreaves/data/uiuc/entity_and_relation_regcognition_corpora/conll04.corp")
  //  )
  //  println(s"Reading input from:\n$inputFile")
  //  val input = Source.fromFile(inputFile)
  //
  //  def cleanWord(word: String) =
  //    word.replaceAll("/", " ")
  //
  //  //
  //  // Seq[Sentence,Relation]
  //  //
  //
  //  def sentenceFrom(tls: Seq[TokenLine]) = Sentence(
  //    tls.map(_.word).map(cleanWord),
  //    Some(tls.map(_.posTag)),
  //    Some(tls.map(_.neTag))
  //  )
  //
  //  val rawLines = input.getLines().flatMap(Line.apply).toSeq
  //
  //  println(s"Obtained ${rawLines.size} individual lines")
  //
  //  val groupedLines = LabeledConll04Reader.lineAggregate(rawLines)
  //
  //  println(s"Obtained ${groupedLines.size} grouped lines")
  //
  //  val labeledSentences: Seq[(Sentence, Seq[RelationLine])] =
  //    groupedLines.map(x => x.copy(_1 = sentenceFrom(x._1)))
  //
  //  println(s"Obtained ${labeledSentences.size} sentences")
  //  println(s"Of those, ${labeledSentences.count(_._2.nonEmpty)} are labeled")
  //
  //  val pconf = {
  //    val (es, ps) = labeledSentences.foldLeft((Set.empty[String], Set.empty[String])) {
  //      case ((e, p), (s, _)) =>
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
  //  val negrel = "nothing"
  //
  //  val candgen = SentenceCandGen {
  //    val okTags = pconf.tagSet.get.nouns ++ pconf.tagSet.get.pronouns
  //
  //    (s: Sentence) =>
  //      (index: Int) =>
  //        s.tags
  //          .map(posTags => okTags contains posTags(index))
  //          .getOrElse(WordFilter.noKnownPunct(s)(index))
  //  }
  //
  //  val trainingData: RelationLearner.TrainingData =
  //    labeledSentences
  //      .flatMap {
  //        case (sentence, relz) =>
  //
  //          val labeled =
  //            relz.map { r =>
  //              (CandidateSentence(sentence, r.arg1, r.arg2), r.relation)
  //            }
  //
  //          val anyIndexPairs = relz.map(r => (r.arg1, r.arg2)).toSet
  //
  //          val unlabeled =
  //            candgen(Document("", Seq(sentence)))
  //              .flatMap(candidate => {
  //                if (!anyIndexPairs.contains((candidate.queryIndex, candidate.answerIndex)))
  //                  Some((candidate, negrel))
  //                else
  //                  None
  //              })
  //
  //          labeled ++ unlabeled
  //      }
  //
  //  println(s"A total of ${trainingData.size} candidates, of which ${trainingData.count(_._2 == negrel)} are unlabeled.")
  //
  //  def makeBinary(max: Int)(v: Int): Int =
  //    if (v >= max - 1) 1 else 0
  //
  //  val (train, test) = {
  //    val lim = 4
  //    val rand = new Random()
  //    val grouped =
  //      trainingData
  //        .map(x => (x, makeBinary(lim)(rand.nextInt(lim))))
  //        .toList
  //        .groupBy(_._2)
  //
  //    println(s"grouped keyset: ${grouped.keySet}")
  //
  //    (grouped(0).map(_._1), grouped(1).map(_._1))
  //  }
  //
  //  val relations =
  //    trainingData
  //      .filter(_._2 != negrel)
  //      .map(_._2)
  //      .toSet
  //
  //  val llConf = LiblinearConfig(
  //    solverType = SolverType.L1R_L2LOSS_SVC,
  //    cost = 12.0,
  //    eps = 0.001,
  //    showDebug = false
  //  )
  //
  //  val featurizer =
  //    CandidateFeatuerizer(
  //      Some((
  //        AdjacentFeatures(2),
  //        SentenceViewFilter.noKnownPunctLowercase
  //      )),
  //      Some((
  //        InsideFeatures(2, 4),
  //        WordFilter.noKnownPunct,
  //        WordView.lowercase
  //      ))
  //    )
  //
  //  val sourceRelationLearner = RelationLearner(llConf, featurizer)
  //
  //  val rlearners =
  //    relations
  //      .map(r => (r, sourceRelationLearner))
  //      .toMap
  //
  //  println(s"Training on ${train.size} instances, one binary SVM per relation (${relations.size} relations)")
  //
  //  val start = System.currentTimeMillis()
  //  val estimators = {
  //    import scala.concurrent.ExecutionContext.Implicits.global
  //    Await.result(
  //      Future.sequence(
  //        rlearners
  //          .toSeq
  //          .map {
  //            case (r, rl) =>
  //              Future {
  //                (
  //                  r,
  //                  rl(
  //                    train.map {
  //                      case (inst, lab) =>
  //                        if (lab == r) (inst, r) else (inst, negrel)
  //                    }
  //                  )._2
  //                )
  //              }
  //          }
  //      ),
  //      Duration.Inf
  //    )
  //  }
  //  val end = System.currentTimeMillis()
  //
  //  println(s"finished training in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} minutes (${end - start} ms)")
  //
  //  val numberCorrectPredictions =
  //    test
  //      .foldLeft(0) {
  //        case (nCorrect, (instance, label)) =>
  //
  //          val predicted =
  //            Learning.argmax(
  //              estimators
  //                .map { case (r, estimator) => (r, estimator(instance).result.head) }
  //            )(Learning.TupleVal2[String])._1
  //
  //          if (predicted == label)
  //            nCorrect + 1
  //          else
  //            nCorrect
  //      }
  //
  //  println(s"# correct $numberCorrectPredictions out of ${test.size} : accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}")

}