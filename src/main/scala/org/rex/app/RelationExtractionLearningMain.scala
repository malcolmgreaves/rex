package org.rex.app

import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import nak.liblinear.{ LiblinearConfig, SolverType }
import org.rex.app.Connl04Format._
import org.rex._

import scala.concurrent.duration.Duration
import scala.language.{ existentials, implicitConversions, postfixOps }

object RelationExtractionLearningMain extends App {

  sealed trait Command

  case class LearningCmd(
    labeledInput: File,
    reader: Reader[File, LabeledSentence]#Fn,
    modelOut: Option[File]) extends Command

  object LearningCmd {
    val emptyUnsafe = LearningCmd(null, null, None)
  }

  case class Evaluation(
    labeledInput: File,
    reader: Reader[File, LabeledSentence]#Fn,
    modelIn: Option[File],
    evalOut: Option[File])

  object Evaluation {
    val emptyUnsafe = Evaluation(null, null, None, None)
  }

  case class Extraction(
    rawInput: File,
    reader: Reader[Any, Any],
    modelIn: Option[File],
    extractOut: Option[File]) extends Command

  object Extraction {
    val emptyUnsafe = Extraction(null, null, None, None)
  }

  case class RelConfig(
    lr: Option[LearningCmd],
    ev: Option[Evaluation],
    ex: Option[Extraction])

  object RelConfig {
    val empty = RelConfig(None, None, None)
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
            ReaderMap[File, LabeledSentence](readerStrInput).map { r =>
              c.copy(lr = Some(c.lr.getOrElse(LearningCmd.emptyUnsafe)
                .copy(reader = r)))
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
  }

  // parser.parse returns Option[C]
  parser.parse(args, RelConfig.empty) match {

    case Some(config) =>
      if (!(config.lr.isDefined || config.ev.isDefined || config.ex.isDefined)) {
        println("ERROR: One of LearningCmd, EvaluationCmd, or ExtractionCmd must be defined.")
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
          case LearningCmd(labeledInput, reader, modelOut) =>

            val labeledSentences = reader(labeledInput)
            println(s"Obtained ${labeledSentences.size} sentences")
            println(s"Of those, ${labeledSentences.count(_._2.nonEmpty)} are labeled")

            val candgen = mkStdCandGen(labeledSentences)

            val labeledData = mkTrainData(candgen, labeledSentences)
            println(s"A total of ${labeledData.size} candidates, of which ${labeledData.count(_._2 == negrel)} are unlabeled.")

            val relations =
              labeledData
                .filter(_._2 != negrel)
                .map(_._2)
                .toSet

            val sourceRelationLearner = RelationLearner(
              LiblinearConfig(
                solverType = SolverType.L1R_L2LOSS_SVC,
                cost = 12.0,
                eps = 0.001,
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
          case Evaluation(labeledInput, reader, modelIn, evalOut) =>

            val eval =
              maybeModelTrainData match {

                case Some((rlearners, labeledData, completeEstimators)) =>
                  println(s"Ignoring Evaluation's labeledInput in favor of LearningCmd's labeledInput\n(ignored: $labeledInput)")

                  val nFolds = 4
                  val dataTrainTest = mkCrossValid(labeledData, nFolds)
                  println(s"Performing $nFolds-fold cross validation")

                  dataTrainTest
                    .toSeq
                    .zipWithIndex
                    .foreach {

                      case ((train, test), fold) =>

                        println(s"#$fold : Begin Training & testing")
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

                        println(s"#$fold : Completed in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} minutes (${end - start} ms)")
                        println(s"# correct $numberCorrectPredictions out of ${test.size} : accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}")
                    }

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