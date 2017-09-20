package org.rex.app

import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import nak.liblinear.{LiblinearConfig, SolverType}
import org.rex.app.Connl04Format._
import org.rex._
import scopt.OptionParser
import org.rex.RelationLearner.{TrainingData => RelLearnTrainingData}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.language.{existentials, implicitConversions, postfixOps}

sealed trait Command

sealed trait CommandT

case class LearningCmd(labeledInput: File,
                       reader: Reader[File, LabeledSentence]#Fn,
                       doCandGen: Boolean = true,
                       modelOut: Option[File],
                       cost: Option[Double],
                       eps: Option[Double])
    extends Command

case object LearningCmd extends CommandT {
  val emptyUnsafe =
    LearningCmd(null, null, doCandGen = true, None, None, None)
}

case class Evaluation(labeledInput: File,
                      reader: Reader[File, LabeledSentence]#Fn,
                      modelIn: Option[File],
                      evalOut: Option[File],
                      maybeNFolds: Option[Int])
    extends Command

case object Evaluation extends CommandT {
  val emptyUnsafe = Evaluation(null, null, None, None, None)
}

case class Extraction(rawInput: File,
                      reader: Reader[Any, Any],
                      modelIn: Option[File],
                      extractOut: Option[File])
    extends Command

case object Extraction extends CommandT {
  val emptyUnsafe = Extraction(null, null, None, None)
}

case class RelConfig(cmd: CommandT,
                     lr: Option[LearningCmd],
                     ev: Option[Evaluation],
                     ex: Option[Extraction])

object RelConfig {
  val emptyUnsafe = RelConfig(null, None, None, None)
}

object RelationExtractionLearningMain {

  /** The app's command line option parser. */
  val parser: OptionParser[RelConfig] =
    new scopt.OptionParser[RelConfig]("relation-extraction-learning-main") {
      head("Relation Learning and Extraction")

      help("help")
        .text("prints this usage text")

      cmd("learning")
        .optional()
        .action { (_, c) =>
          (
            if (c.lr.isEmpty)
              c.copy(lr = Some(LearningCmd.emptyUnsafe))
            else
              c
          ).copy(cmd = LearningCmd)
        }
        .text("\tApp will learn a relation extraction model. \n" +
          "\tOne of three possible commands.")

      cmd("evaluation")
        .optional()
        .action { (_, c) =>
          (
            if (c.ev.isEmpty)
              c.copy(ev = Some(Evaluation.emptyUnsafe))
            else
              c
          ).copy(cmd = Evaluation)
        }
        .text(
          "\tApp will evaluate a relation extraction model on gold-standard, labeled relations.\n" +
            "\tOne of three possible commands.")

      cmd("extraction")
        .optional()
        .action { (_, c) =>
          (
            if (c.ex.isEmpty)
              c.copy(ex = Some(Extraction.emptyUnsafe))
            else
              c
          ).copy(cmd = Extraction)
        }
        .text("\tApp will perform relation extraction using a previously learned model.\n" +
          "\tOne of three possible commands.")

      opt[File]("labeledInput")
        .optional()
        .abbr("li")
        .valueName("<file>")
        .action { (li, c) =>
          c.copy(
            lr = Some(
              c.lr
                .getOrElse(LearningCmd.emptyUnsafe)
                .copy(labeledInput = li)))
        }
        .text("Input of labeled relation data.")

      opt[String]("input_text_conll")
        .optional()
        .abbr("input")
        .valueName("<filepath>")
        .text("Input (training) data file format reader type (conll)")
        .action { (readerStrInput, c) =>
          ReaderMap[File, LabeledSentence](readerStrInput) match {
            case Some(r) =>
              c.copy(
                lr = Some(
                  c.lr
                    .getOrElse(LearningCmd.emptyUnsafe)
                    .copy(reader = r)))
            case None =>
              println(s"ERROR: Unrecognized labeled reader: $readerStrInput")
              c
          }
        }

      opt[File]("model_output")
        .optional()
        .abbr("output")
        .valueName("<filepath>")
        .text(
          "Path to where the trained relation classifier and pipeline config should be saved to.")
        .action { (mo, c) =>
          c.copy(
            lr = Some(
              c.lr
                .getOrElse(LearningCmd.emptyUnsafe)
                .copy(modelOut = Some(mo))))
        }

      opt[Boolean]("candidate_generation_train")
        .optional()
        .abbr("cg")
        .valueName("<boolean>")
        .text("Perform sentence-based candidate generation during training?\n" +
          "\tFalse means only use positively labeled things.")
        .action { (cg, c) =>
          c.copy(
            lr = Some(
              c.lr
                .getOrElse(LearningCmd.emptyUnsafe)
                .copy(doCandGen = cg)))
        }

      opt[Double]("cost")
        .optional()
        .abbr("c")
        .valueName("<float>")
        .text("Positive mis-classification cost for cost-sensitive learning.")
        .action { (cost, c) =>
          c.copy(
            lr = Some(
              c.lr
                .getOrElse(LearningCmd.emptyUnsafe)
                .copy(cost = Some(cost))))
        }

      opt[Double]("eps")
        .optional()
        .abbr("e")
        .valueName("<float>")
        .text("Stopping criterion for learning: when the parameter change between iterations\n" +
          "\tis less than eps, learning stops.")
        .action { (eps, c) =>
          c.copy(
            lr = Some(
              c.lr
                .getOrElse(LearningCmd.emptyUnsafe)
                .copy(eps = Some(eps))))
        }

      opt[Int]("n_cv_folds")
        .optional()
        .abbr("cv")
        .valueName("<int>")
        .text("Number of cross validation folds: must be >= 2")
        .action { (nFolds, c) =>
          c.copy(
            ev = Some(
              c.ev
                .getOrElse(Evaluation.emptyUnsafe)
                .copy(maybeNFolds = Some(nFolds))))
        }

    }

  def main(args: Array[String]): Unit =
    parser.parse(args, RelConfig.emptyUnsafe) match {

      case Some(config) =>
        validate(config)
        implicit val r = new Random()
        import scala.concurrent.ExecutionContext.Implicits.global
        main_action(config)

      case None =>
        // arguments are bad, error message will have been displayed
        println("Bad arguments. Exiting.")
        System.exit(1)
    }

  /** Validates a configuration: exits with status code 1 if invalid. */
  def validate(config: RelConfig): Unit = {
    if (!(config.lr.isDefined || config.ev.isDefined || config.ex.isDefined)) {
      println(
        "ERROR: One of LearningCmd (learning), " +
          "EvaluationCmd (evaluation), " +
          "or ExtractionCmd (extraction) " +
          "must be supplied as the <COMMAND>.\n")
      parser.showUsage
      System.exit(1)
    }

    if (config.cmd == LearningCmd && config.lr.get.modelOut.isEmpty) {
      println(
        "ERROR: Command is \"learning\" and " +
          "no model output path is specified.\n")
      parser.showUsage
      System.exit(1)
    }
  }

  /** The application logic. Assumes configuration is valid. */
  def main_action(config: RelConfig)(implicit rand: Random, ec: ExecutionContext): Unit = {
    val featurizer =
      CandidateFeatuerizer(
        Some(
          (
            AdjacentFeatures(2),
            SentenceViewFilter.noKnownPunctLowercase
          )),
        Some(
          (
            InsideFeatures(2, 4),
            WordFilter.noKnownPunct,
            WordView.lowercase
          ))
      )

    val maybeModelTrainData: Option[(MultiLearner, RelLearnTrainingData, () => MultiEstimator)] =
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
          println(s"A total of ${labeledData.size} candidates, of which " +
            s"${(labeledData.count(_._2 != noRelation) / labeledData.size.toDouble) * 100.0} % " +
            s"are labeled.")

          val relations = labeledData.foldLeft(Set.empty[RelationLearner.Label]) {
            case (rs, (_, rel)) => rs + rel
          }
          println(s"""There are ${relations.size} relations:\n\t${relations.mkString("\n\t")}""")

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
            () =>
              {
                val start = System.currentTimeMillis()
                val estimators = trainLearners(rlearners, labeledData)
                val end = System.currentTimeMillis()
                println(
                  s"finished training in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} " +
                    s"minutes (${end - start} ms)")

                modelOut.foreach { mo =>
                  if (calledAtLeastOnce.getAndSet(true))
                    println(
                      s"WARNING: Model serialization & deserialization is not implemented. " +
                        s"NOT saving model to: $mo")
                }

                estimators
              }

          }

          (rlearners, labeledData, trainModel)
      }

    config.ev.foreach {
      case Evaluation(labeledInput, reader, modelIn, evalOut, maybeNFolds) =>
        maybeModelTrainData match {

          case Some((rlearners, labeledData, completeEstimators)) =>
            println(
              s"Ignoring Evaluation's labeledInput in favor of " +
                s"LearningCmd's labeledInput\n(ignored: $labeledInput)")

            val nFolds = maybeNFolds.getOrElse(4)
            val dataTrainTest =
              if (nFolds == 1) {
                println(s"Performing train-test with 75% train")
                trainTestSplit(labeledData, 0.75)

              } else {
                println(s"Performing $nFolds-fold cross validation")
                mkCrossValid(labeledData, nFolds)
              }

            dataTrainTest.toSeq.zipWithIndex
              .foreach {

                case ((train, test), fIndex) =>
                  val fold = fIndex + 1
                  println(s"#$fold/$nFolds : Begin Training & testing")
                  val start = System.currentTimeMillis()

                  val estimators = trainLearners(rlearners, train)

                  val numberCorrectPredictions =
                    test
                      .foldLeft(0) {
                        case (nCorrect, (instance, label)) =>
                          val predicted =
                            Learning
                              .argmax(
                                estimators
                                  .map {
                                    case (r, estimator) =>
                                      (r, estimator(instance).result.head)
                                  }
                              )(Learning.TupleVal2[String])
                              ._1

                          if (predicted == label)
                            nCorrect + 1
                          else
                            nCorrect
                      }

                  val end = System.currentTimeMillis()

                  println(
                    s"#$fold/$nFolds : Completed in " +
                      s"${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} minutes " +
                      s"(${end - start} ms)" +
                      "\n" +
                      s"#$fold/$nFolds correct $numberCorrectPredictions out of ${test.size} : " +
                      s"accuracy: ${(numberCorrectPredictions.toDouble / test.size) * 100.0}")
              }

          case None =>
            throw new RuntimeException(
              "ERROR: Evaluation from serialized model is not implemented.")
        }

        evalOut.foreach { eo =>
          println(s"WARNING: Evaluation output is not implemented. NOT writing evaluation to: $eo")
        }

    }

    config.ex.foreach {
      case Extraction(rawInput, reader, modelIn, extractOut) =>
        throw new RuntimeException("ERROR: Extraction command unimplemented.")
    }
  }

}
