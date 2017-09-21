package org.rex.app

import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit

import nak.liblinear.{LiblinearConfig, SolverType}
import org.rex.io.UiucRelationFmt._
import org.rex.io.{Reader, ReaderMap}
import org.rex.relation_extract.RelationLearner.{Label, TrainingData}
import org.rex.relation_extract._
import org.rex.text.{SentenceViewFilter, WordFilter, WordView}
import scopt.OptionParser

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.language.{existentials, implicitConversions, postfixOps}

object RelationExtractionLearningMain {

  /** The app's command line option parser. */
  lazy val parser: OptionParser[RelConfig] =
    new scopt.OptionParser[RelConfig]("relation-extraction-learning-main") {
      head("Relation Learning and Extraction")

      help("help")
        .text("prints this usage text")

      cmd("learning")
        .optional()
        .action { (_, c) =>
          c.copy(
            cmd = LearningCmd(labeledInput = null,
                              reader = null,
                              doCandGen = false,
                              modelOut = null,
                              cost = None,
                              eps = None,
                              sizeForFeatureHashing = None,
                              sampleNeg = None))
        }
        .text("\tApp will learn a relation extraction model. \n" +
          "\tOne of three possible commands.")

      cmd("evaluation")
        .optional()
        .action { (_, c) =>
          c.copy(
            cmd = EvaluationCmd(labeledInput = null,
                                reader = null,
                                modelIn = null,
                                evalOut = None,
                                maybeNFolds = None))
        }
        .text(
          "\tApp will evaluate a relation extraction model on gold-standard, labeled relations.\n" +
            "\tOne of three possible commands.")

      cmd("learn_eval")
        .optional()
        .action { (_, c) =>
          c.copy(
            cmd = LearnEvaluateCmd(labeledInput = null,
                                   reader = null,
                                   doCandGen = false,
                                   modelOut = null,
                                   maybeNFolds = None,
                                   cost = None,
                                   eps = None,
                                   sizeForFeatureHashing = None,
                                   sampleNeg = None))
        }

      cmd("extraction")
        .optional()
        .action { (_, c) =>
          c.copy(
            cmd = ExtractionCmd(rawInput = null, reader = null, modelIn = null, extractOut = None))
        }
        .text("\tApp will perform relation extraction using a previously learned model.\n" +
          "\tOne of three possible commands.")

      opt[File]("labeled_input")
        .optional()
        .abbr("li")
        .valueName("<file>")
        .action { (li, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(labeledInput = li)
            case cmd: EvaluationCmd => cmd.copy(labeledInput = li)
            case cmd: LearnEvaluateCmd => cmd.copy(labeledInput = li)
            case _ =>
              throw new IllegalStateException(
                s"labeled_input is invalid for command ${c.cmd.getClass}")
          })
        }
        .text("Input of labeled relation data.")

      opt[String]("input_format")
        .optional()
        .valueName("<special>")
        .text("Format of labeled input (e.g. conll)")
        .action { (readerStrInput, c) =>
          ReaderMap[File, LabeledSentence](readerStrInput) match {
            case Some(r) =>
              c.copy(cmd = c.cmd match {
                case cmd: LearningCmd => cmd.copy(reader = r)
                case cmd: EvaluationCmd => cmd.copy(reader = r)
                case cmd: LearnEvaluateCmd => cmd.copy(reader = r)
                case cmd: ExtractionCmd => cmd.copy(reader = r.asInstanceOf[Reader[Any, Any]])
                case _ =>
                  throw new IllegalStateException(s"Unknown Command type: ${c.cmd.getClass}")
              })

            case None =>
              throw new IllegalArgumentException(
                s"ERROR: Unrecognized labeled reader: $readerStrInput\n" +
                  s"command type: ${c.cmd.getClass}")
          }
        }

      opt[File]("model_output")
        .optional()
        .abbr("output")
        .valueName("<filepath>")
        .text("Path to where the trained relation classifier and pipeline config should be saved.")
        .action { (mo, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(modelOut = mo)
            case _ =>
              throw new IllegalStateException(
                s"model_output invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Boolean]("candidate_generation_train")
        .optional()
        .abbr("cg")
        .valueName("<boolean>")
        .text("Perform sentence-based candidate generation during training?\n" +
          "\tFalse means only use positively labeled things.")
        .action { (cg, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(doCandGen = cg)
            case cmd: LearnEvaluateCmd => cmd.copy(doCandGen = cg)
            case _ =>
              throw new IllegalStateException(
                s"candidate_generation_train invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Double]("cost")
        .optional()
        .abbr("c")
        .valueName("<float>")
        .text("Positive mis-classification cost for cost-sensitive learning.")
        .action { (cost, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(cost = Some(cost))
            case cmd: LearnEvaluateCmd => cmd.copy(cost = Some(cost))
            case _ =>
              throw new IllegalStateException(s"cost invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Double]("epsilon")
        .optional()
        .abbr("e")
        .valueName("<float>")
        .text("Stopping criterion for learning: when the parameter change between iterations\n" +
          "\tis less than eps, learning stops.")
        .action { (eps, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(eps = Some(eps))
            case cmd: LearnEvaluateCmd => cmd.copy(eps = Some(eps))
            case _ =>
              throw new IllegalStateException(s"epsilon invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Int]("feature_hash_size")
        .optional()
        .valueName("<int>")
        .text("If provided, the size of the hashed feature space.")
        .action { (featHashSize, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(sizeForFeatureHashing = Some(featHashSize))
            case cmd: LearnEvaluateCmd => cmd.copy(sizeForFeatureHashing = Some(featHashSize))
            case _ =>
              throw new IllegalStateException(
                s"feature_hash_size invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Double]("sample_neg")
        .optional()
        .valueName("<float>")
        .text("If provided, will sample this proportion of the non-positive relation candidates.")
        .action { (sampleNeg, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: LearningCmd => cmd.copy(sampleNeg = Some(sampleNeg))
            case cmd: LearnEvaluateCmd => cmd.copy(sampleNeg = Some(sampleNeg))
            case _ =>
              throw new IllegalStateException(s"sample_neg invalid for command: ${c.cmd.getClass}")
          })
        }

      opt[Int]("n_cv_folds")
        .optional()
        .abbr("cv")
        .valueName("<int>")
        .text("Number of cross validation folds: must be >= 2")
        .action { (nFolds, c) =>
          c.copy(cmd = c.cmd match {
            case cmd: EvaluationCmd => cmd.copy(maybeNFolds = Some(nFolds))
            case cmd: LearnEvaluateCmd => cmd.copy(maybeNFolds = Some(nFolds))
            case _ =>
              throw new IllegalStateException(s"n_cv_folds invalid for command: ${c.cmd.getClass}")
          })
        }

    }

  def main(args: Array[String]): Unit =
    parser.parse(args, RelConfig(cmd = null)) match {

      case Some(config) =>
        RelConfig.validate(config, parser)

        implicit val _ = new Random()
        import scala.concurrent.ExecutionContext.Implicits.global
        process_command(
          cmd = config.cmd,
          featurizer = CandidateFeatuerizer(
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
          ),
          verbose = true
        )

      case None =>
        // arguments are bad, error message will have been displayed
        println("Bad arguments. Exiting.")
        System.exit(1)
    }

  /** The application logic. Assumes configuration is valid. */
  def process_command(cmd: Command, featurizer: TextFeatuerizer[Candidate]#Fn, verbose: Boolean)(
      implicit rand: Random,
      ec: ExecutionContext): Unit =
    cmd match {

      case LearnEvaluateCmd(labeledInput,
                            reader,
                            doCG,
                            modelOut,
                            maybeNFolds,
                            cost,
                            eps,
                            fHash,
                            sampleNeg) =>
        val labeledData =
          createLabeledData(labeledInput, reader, doCG = doCG, sampleNeg, verbose = verbose)
        val rlearners = createRelationLearnerFuncs(createRelations(labeledData, verbose = verbose),
                                                   featurizer,
                                                   cost = cost,
                                                   eps = eps,
                                                   sizeForFeatureHashing = fHash,
                                                   verbose = verbose)

        val (dataTrainTest, nFolds) = maybeNFolds.fold {
          if (verbose) {
            println(s"Performing train-test with 75% train")
          }
          (trainTestSplit(labeledData, 0.75), 1)
        } { nf =>
          if (verbose) {
            println(s"Performing $nf-fold cross validation")
          }
          (mkCrossValid(labeledData, nf), nf)
        }

        dataTrainTest.toSeq.zipWithIndex
          .foreach {

            case ((train, test), fIndex) =>
              val start_fold = System.currentTimeMillis()

              val fold = fIndex + 1
              if (verbose) {
                val labelPropInfo =
                  labelCount(train)
                    .map {
                      case (label, count) =>
                        s"$count instances of $label " +
                          s"(${formatDecimalPoint((count.toDouble / train.size) * 100.0)}%)"
                    }
                    .mkString("\n")

                println(s"#$fold/$nFolds : Begin Training & testing :\n$labelPropInfo\n")
              }

              val estimators = trainEstimator(rlearners, labeledData = train, verbose = verbose)

              val cm =
                test
                  .foldLeft(new MutableConfusionMatrix()) {
                    case (mutConfMat, (instance, label)) =>
                      val estimatesPerRelation = estimators.map {
                        case (r, estimator) =>
                          (r, estimator(instance).result.head)
                      }
                      val predicted =
                        Learning.argmax(estimatesPerRelation)(Learning.TupleVal2[String])._1

                      if (label == noRelation && predicted == noRelation) {
                        // true negative
                        mutConfMat.increment(tn = 1L)

                      } else if (label == noRelation && predicted != noRelation) {
                        // false negative
                        mutConfMat.increment(fn = 1L)

                      } else if (label != noRelation && predicted == label) {
                        // true positive
                        mutConfMat.increment(tp = 1L)

                      } else if (label != noRelation && predicted != label) {
                        // false positive
                        mutConfMat.increment(fp = 1L)

                      } else {
                        throw new IllegalStateException(
                          s"Unexpected prediction. Label: $label Prediction: $predicted")
                      }
                  }

              val end_fold = System.currentTimeMillis()
              println(
                s"#$fold/$nFolds : consisting of ${test.size} examples was completed in " +
                  s"${Duration(end_fold - start_fold, TimeUnit.MILLISECONDS).toMinutes} minutes " +
                  s"(${end_fold - start_fold} ms)\n" +
                  s"${cm.truePositives() + cm.trueNegatives()} out of ${cm.total()} correct\n" +
                  s"accuracy:  ${formatDecimalPoint(cm.accuracy() * 100.0)} %\n" +
                  s"precision: ${formatDecimalPoint(cm.precision() * 100.0)} %\n" +
                  s"recall:    ${formatDecimalPoint(cm.recall() * 100.0)} %\n" +
                  s"F1:        ${formatDecimalPoint(cm.f1() * 100.0)} %\n" +
                  s"Confusion Matrix: $cm\n" +
                  s"\n")
          }

      case LearningCmd(labeledInput, reader, doCG, modelOut, cost, eps, fHash, sampleNeg) =>
        val labeledData =
          createLabeledData(labeledInput, reader, doCG, sampleNeg, verbose = verbose)
        val rlearners = createRelationLearnerFuncs(createRelations(labeledData, verbose = verbose),
                                                   featurizer,
                                                   cost = cost,
                                                   eps = eps,
                                                   sizeForFeatureHashing = fHash,
                                                   verbose = verbose)
        val estimators = trainEstimator(rlearners, labeledData, verbose = verbose)
        saveEstimators(modelOut, estimators)

      case _: EvaluationCmd =>
        throw new IllegalStateException("Evaluation unimplemented !!!")

      case _: ExtractionCmd =>
        throw new IllegalStateException("Extraction is unimplemented!!")

      case _ =>
        throw new IllegalStateException(s"ERROR: unknown command ${cmd.getClass}")
    }

  def formatDecimalPoint(value: Double): String =
    f"$value%3.2f"

  def labelCount(labeledData: Traversable[(Any, Label)]): Seq[(Label, Long)] =
    labeledData
      .foldLeft(Map.empty[Label, Long]) {
        case (accum, (_, label)) =>
          if (accum.contains(label)) {
            (accum - label) + (label -> (accum(label) + 1L))
          } else {
            accum + (label -> 1L)
          }
      }
      .toSeq

  def createLabeledData(labeledInput: File,
                        reader: Reader[File, LabeledSentence]#Fn,
                        doCG: Boolean,
                        sampleNeg: Option[Double],
                        verbose: Boolean = true)(implicit rand: Random): TrainingData = {

    val labeledSentences = reader(labeledInput)
    if (verbose) {
      println(s"Obtained ${labeledSentences.size} sentences")
      println(s"Of those, ${labeledSentences.count(_._2.nonEmpty)} are labeled")
    }

    val candgen = mkStdCandGen(labeledSentences)

    if (verbose) {
      println(s"Making training data with sentence-based candidate generation? $doCG")
    }

    val labeledData = {
      val all =
        if (doCG)
          mkTrainData(candgen, labeledSentences)
        else
          mkPositiveTrainData(labeledSentences)

      sampleNeg.fold(all) { propOfNegToAccept =>
        if (verbose)
          println(
            s"Sampling negative, no-relation candidates at " +
              s"${formatDecimalPoint(propOfNegToAccept * 100.0)} %")

        all.filter {
          case (_, label) =>
            // accept if the label is NOT "no_relation"
            // or accept if the label is "no_relation" and it passes our random filter
            label != noRelation || rand.nextDouble() <= propOfNegToAccept
        }
      }
    }

    if (verbose) {
      println(s"A total of ${labeledData.size} candidates, of which " +
        s"${formatDecimalPoint((labeledData.count(_._2 != noRelation) / labeledData.size.toDouble) * 100.0)} % " +
        s"are labeled.")
    }

    labeledData
  }

  def createRelations(labeledData: TrainingData,
                      verbose: Boolean = true): Set[RelationLearner.Label] = {
    val relations = labeledData.foldLeft(Set.empty[RelationLearner.Label]) {
      case (rs, (_, rel)) => rs + rel
    }
    if (verbose) {
      val rels = relations.filter {
        _ != noRelation
      }
      println(s"""There are ${rels.size} relations:\n\t${rels.mkString("\n\t")}""")
    }
    relations
  }

  def createRelationLearnerFuncs(relations: Set[RelationLearner.Label],
                                 featurizer: TextFeatuerizer[Candidate]#Fn,
                                 lossFunc: Option[SolverType] = None,
                                 cost: Option[Double] = None,
                                 eps: Option[Double] = None,
                                 sizeForFeatureHashing: Option[Int] = None,
                                 verbose: Boolean = true): MultiLearner = {
    // a single binary relation learner constructor:
    // when called, makes a new model that is able to learn a from +/- instances for a single
    // relation type
    val usingCost = cost.getOrElse(8.5)
    val usingEps = eps.getOrElse(0.001)
    val usingLoss = lossFunc.getOrElse(SolverType.L1R_L2LOSS_SVC)
    if (verbose) {
      println(
        s"Training linear SVM binary classifiers with " +
          s"cost: $usingCost , " +
          s"epsilon: $usingEps, " +
          s"loss function: $usingLoss")
      sizeForFeatureHashing.foreach { fsSize =>
        println(s"Also using feature hashing with a maximum dimension of: $fsSize")
      }
      println("")
    }
    val sourceRelationLearner = RelationLearner(
      LiblinearConfig(
        solverType = usingLoss,
        cost = usingCost,
        eps = usingEps,
        showDebug = false
      ),
      featurizer,
      sizeForFeatureHashing
    )

    val rlearners: MultiLearner =
      relations
        .filter {
          _ != noRelation
        }
        .map(r => (r, sourceRelationLearner))
        .toMap

    rlearners
  }

  def trainEstimator(rlearners: MultiLearner, labeledData: TrainingData, verbose: Boolean = true)(
      implicit ec: ExecutionContext): MultiEstimator = {
    val start = System.currentTimeMillis()
    val estimators = trainLearners(rlearners, labeledData)
    val end = System.currentTimeMillis()
    if (verbose) {
      println(
        s"finished training in ${Duration(end - start, TimeUnit.MILLISECONDS).toMinutes} " +
          s"minutes (${end - start} ms)")
    }
    estimators
  }

  def saveEstimators(model: File, estimator: MultiEstimator): Unit = {
    println(
      s"ERROR: Model serialization & deserialization is not implemented. " +
        s"NOT saving model to: $model")
  }

  def loadEstimators(model: File): MultiEstimator = {
    throw new IllegalStateException(
      s"WARNING: Model serialization & deserialization is not implemented. " +
        s"NOT saving model to: $model")
  }

}
