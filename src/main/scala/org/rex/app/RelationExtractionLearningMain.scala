package org.rex.app

import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import nak.liblinear.{ LiblinearConfig, SolverType }
import org.rex.app.Connl04Format._
import org.rex._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.language.{ existentials, implicitConversions, postfixOps }

object RelationExtractionLearningMain extends App {

  sealed trait Command

  sealed trait CommandT

  case class LearningCmd(
    labeledInput: File,
    reader: Reader[File, LabeledSentence]#Fn,
    doCandGen: Boolean = true,
    modelOut: Option[File],
    cost: Option[Double],
    eps: Option[Double]) extends Command

  case object LearningCmd extends CommandT {
    val emptyUnsafe = LearningCmd(null, null, doCandGen = true, None, None, None)
  }

  case class Evaluation(
    labeledInput: File,
    reader: Reader[File, LabeledSentence]#Fn,
    modelIn: Option[File],
    evalOut: Option[File],
    maybeNFolds: Option[Int]) extends Command

  case object Evaluation extends CommandT {
    val emptyUnsafe = Evaluation(null, null, None, None, None)
  }

  case class Extraction(
    rawInput: File,
    reader: Reader[Any, Any],
    modelIn: Option[File],
    extractOut: Option[File]) extends Command

  case object Extraction extends CommandT {
    val emptyUnsafe = Extraction(null, null, None, None)
  }

  case class RelConfig(
    cmd: CommandT,
    lr: Option[LearningCmd],
    ev: Option[Evaluation],
    ex: Option[Extraction])

  object RelConfig {
    val emptyUnsafe = RelConfig(null, None, None, None)
  }

  val parser = new scopt.OptionParser[RelConfig]("relation-extraction-learning-main") {
    head("Relation Learning and Extraction")

    help("help")
      .text("prints this usage text")

    cmd("learning")
      .optional()
      .action { (_, c) =>
        (
          if (!c.lr.isDefined)
            c.copy(lr = Some(LearningCmd.emptyUnsafe))
          else
            c
        ).copy(cmd = LearningCmd)
      }

    cmd("evaluation")
      .optional()
      .action { (_, c) =>
        (
          if (!c.ev.isDefined)
            c.copy(ev = Some(Evaluation.emptyUnsafe))
          else
            c
        ).copy(cmd = Evaluation)
      }

    cmd("extraction")
      .optional()
      .action { (_, c) =>
        (
          if (!c.ex.isDefined)
            c.copy(ex = Some(Extraction.emptyUnsafe))
          else
            c
        ).copy(cmd = Extraction)
      }

    opt[File]("labeledInput")
      .optional()
      .abbr("li")
      .valueName("<file>")
      .action { (li, c) =>
        c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
          .copy(labeledInput = li)))
      }
      .text("Input of labeled relation data.")

    opt[String]("learnReader")
      .optional()
      .abbr("lr")
      .valueName("typeOf[Reader]")
      .action { (readerStrInput, c) =>
        ReaderMap[File, LabeledSentence](readerStrInput) match {
          case Some(r) =>
            c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
              .copy(reader = r)))
          case None =>
            println(s"ERROR: Unrecognized labeled reader: $readerStrInput")
            c
        }
      }
      .text("Input (training) data file format reader type (conll)")

    opt[File]("modelOut")
      .optional()
      .abbr("mo")
      .valueName("<file>")
      .action { (mo, c) =>
        c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
          .copy(modelOut = Some(mo))))
      }
      .text("Path to where the trained relation classifier and pipeline config should be saved to.")

    opt[Boolean]("candgen")
      .optional()
      .abbr("cg")
      .action { (cg, c) =>
        c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
          .copy(doCandGen = cg)))
      }
      .text("Perform sentence-based candidate generation during training? False means only use positively labeled things.")

    opt[Double]("cost")
      .optional()
      .abbr("c")
      .action { (cost, c) =>
        c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
          .copy(cost = Some(cost))))
      }
      .text("Positive mis-classificaiton cost for cost-sensative learning.")

    opt[Double]("eps")
      .optional()
      .abbr("e")
      .action { (eps, c) =>
        c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
          .copy(eps = Some(eps))))
      }
      .text("Stopping criterion for learning: when the parameter change between iterations is less than eps, learning stops.")

    opt[Int]("nfolds")
      .optional()
      .abbr("nf")
      .action { (nFolds, c) =>
        c.copy(ev = Some(c.ev.getOrElse(Evaluation.emptyUnsafe)
          .copy(maybeNFolds = Some(nFolds))))
      }
      .text("# Folds for cross validation")

  }

  // parser.parse returns Option[C]
  parser.parse(args, RelConfig.emptyUnsafe) match {

    case Some(config) =>
      if (!(config.lr.isDefined || config.ev.isDefined || config.ex.isDefined)) {
        println("ERROR: One of LearningCmd, EvaluationCmd, or ExtractionCmd must be defined.\n")
        parser.showUsage
        System.exit(1)
      }

      if (config.cmd == LearningCmd && config.lr.get.modelOut.isEmpty) {
        println("ERROR: Command is \"learning\" and no model output path is specified.\n")
        parser.showUsage
        System.exit(1)
      }

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

      implicit val rand = new Random()

      import scala.concurrent.ExecutionContext.Implicits.global

      val maybeModelTrainData: Option[(MultiLearner, RelationLearner.TrainingData, () => MultiEstimator)] =
        config.lr.map {
          case LearningCmd(labeledInput, reader, doCG, modelOut, cost, eps) =>

            val labeledSentences = reader(labeledInput)
            println(s"Obtained ${labeledSentences.size} sentences")
            println(s"Of those, ${labeledSentences.count(_._2.nonEmpty)} are labeled")

            val candgen = mkStdCandGen(labeledSentences)

            println(s"Making training data with sentence-based candidate generation? $doCG")
            val labeledData =
              if (doCG)
                mkTrainData(candgen, labeledSentences, noRelation)
              else
                mkPositiveTrainData(labeledSentences)
            println(s"A total of ${labeledData.size} candidates, of which ${labeledData.count(_._2 == negrel)} are unlabeled.")

            val relations =
              labeledData
                .foldLeft(Set.empty[RelationLearner.Label]) {
                  case (rs, (_, rel)) => rs + rel
                }
            println(s"""There are ${relations.size} relations:\n${relations.mkString("\n\t")}""")

            val sourceRelationLearner = RelationLearner(
              LiblinearConfig(
                solverType = SolverType.L1R_L2LOSS_SVC,
                cost = cost.getOrElse(8.5),
                eps = eps.getOrElse(0.001),
                showDebug = false
              ),
              featurizer
            )

            val rlearners: MultiLearner =
              relations
                .map(r => (r, sourceRelationLearner))
                .toMap

            val trainModel = {
              val calledAtLeastOnce = new AtomicBoolean()
              () => {
                val start = System.currentTimeMillis()
                val estimators = trainLearners(rlearners, labeledData)
                val end = System.currentTimeMillis()
                println(s"finished training in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} minutes (${end - start} ms)")

                modelOut.foreach { mo =>
                  if (calledAtLeastOnce.getAndSet(true))
                    println(s"WARNING: Model serialization & deserialization is not implemented. NOT saving model to: $mo")
                }

                estimators
              }

            }

            (rlearners, labeledData, trainModel)
        }

      val maybeEvaluation =
        config.ev.foreach {
          case Evaluation(labeledInput, reader, modelIn, evalOut, maybeNFolds) =>

            val eval =
              maybeModelTrainData match {

                case Some((rlearners, labeledData, completeEstimators)) =>
                  println(s"Ignoring Evaluation's labeledInput in favor of LearningCmd's labeledInput\n(ignored: $labeledInput)")

                  val nFolds = maybeNFolds.getOrElse(4)
                  println(s"Performing $nFolds-fold cross validation")
                  val dataTrainTest = mkCrossValid(labeledData, nFolds)

                  Await.result(
                    Future.sequence(
                      dataTrainTest
                        .toSeq
                        .zipWithIndex
                        .map {

                          case ((train, test), fIndex) =>
                            Future {
                              val fold = fIndex + 1
                              println(s"#$fold/$nFolds : Begin Training & testing")
                              val start = System.currentTimeMillis()

                              val estimators = trainLearners(rlearners, train)

                              val numberCorrectPredictions =
                                test
                                  .foldLeft(0) {
                                    case (nCorrect, (instance, label)) =>

                                      val predicted =
                                        Learning.argmax(
                                          estimators
                                            .map { case (r, estimator) => (r, estimator(instance).result.head) }
                                        )(Learning.TupleVal2[String])._1

                                      if (predicted == label)
                                        nCorrect + 1
                                      else
                                        nCorrect
                                  }

                              val end = System.currentTimeMillis()

                              println("" +
                                s"#$fold/$nFolds : Completed in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} minutes (${end - start} ms)\n" +
                                s"#$fold/$nFolds correct $numberCorrectPredictions out of ${test.size} : accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}"
                              )
                            }
                        }
                    ),
                    Duration.Inf
                  )

                case None =>
                  throw new RuntimeException("ERROR: Evaluation from serialized model is not implemented.")
              }

            evalOut.foreach { eo =>
              println(s"WARNING: Evaluation output is not implemented. NOT writing evaluation to: $eo")
            }

            eval
        }

      config.ex.foreach {
        case Extraction(rawInput, reader, modelIn, extractOut) =>
          throw new RuntimeException("ERROR: Extraction command unimplemented.")
      }

    case None =>
      // arguments are bad, error message will have been displayed
      println("Bad arguments. Exiting.")
      System.exit(1)
  }

}