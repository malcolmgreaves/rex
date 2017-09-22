package org.rex.app

import java.io.File

import org.rex.io.Reader
import org.rex.io.UiucRelationFmt.LabeledSentence
import org.rex.relation_extract.RelationLearner
import scopt.OptionParser

case class RelConfig(cmd: Command)

object RelConfig {

  /** Validates a configuration: exits with status code 1 if invalid. */
  def validate(config: RelConfig,
               parser: OptionParser[RelConfig],
               doSysExit: Boolean = true): Unit = {

    def error() =
      if (doSysExit)
        System.exit(1)
      else
        throw new IllegalArgumentException("Failed to validate RelConfig")

    if (config == null) {
      println("Configuration cannot be null")
      error()
    }

    def costCheck(costOpt: Option[Double]) =
      costOpt.foreach { cost =>
        if (cost < 0) {
          println(s"error(): Cost value (C) must be positive, not $cost")
          error()
        }
      }

    def epsilonCheck(epsilonOpt: Option[Double]) =
      epsilonOpt.foreach { epsilon =>
        if (epsilon < 0) {
          println(s"error(): Epsilon must be positive, not $epsilon")
          error()
        }
      }

    def featureHashSizeCheck(sizeForFeatureHashing: Option[Int]) =
      sizeForFeatureHashing.foreach { fsSize =>
        if (fsSize <= 0) {
          println(s"error(): Feature hash size must be positive, not $fsSize")
          error()
        }
      }

    def modelInCheck(modelIn: File, cmdName: String) =
      if (modelIn == null) {
        println(s"error(): $cmdName needs model input")
        error()

      } else if (!modelIn.exists()) {
        println(s"error(): model input path does not exist: $modelIn")
        error()
      }

    def nFoldsCheck(maybeNFolds: Option[Int]) =
      maybeNFolds.foreach { nFolds =>
        if (nFolds <= 0) {
          println(s"error(): # of cross validation folds must be positive, not $nFolds")
          error()
        }
      }

    def proportionCheck(propOpt: Option[Double], name: String) =
      propOpt.foreach { proportion =>
        if (proportion < 0.0 || proportion > 1.0) {
          println(s"ERROR: $name must be in [0,1], it is instead: $proportion")
          error()
        }

      }

    config.cmd match {
      case lr: LearningCmd =>
        if (lr.modelOut == null) {
          println(
            "error(): Command is \"learning\" and " +
              "no model output path is specified.\n")
          parser.showUsage
          error()

        } else if (lr.modelOut.exists()) {
          println("error(): model_output already exists!")
          error()
        }

        costCheck(lr.cost)
        epsilonCheck(lr.eps)
        featureHashSizeCheck(lr.sizeForFeatureHashing)
        proportionCheck(lr.sampleNeg, "Negative candidate sampling acceptance")

      case ev: EvaluationCmd =>
        modelInCheck(ev.modelIn, "evaluation")
        nFoldsCheck(ev.maybeNFolds)

      case le: LearnEvaluateCmd =>
        costCheck(le.cost)
        epsilonCheck(le.eps)
        featureHashSizeCheck(le.sizeForFeatureHashing)
        nFoldsCheck(le.maybeNFolds)
        proportionCheck(le.sampleNeg, "Negative candidate sampling acceptance")

      case ex: ExtractionCmd =>
        modelInCheck(ex.modelIn, "extraction")

      case unk =>
        if (unk == null)
          parser.showUsage
        else
          print(s"error(): unknown command type: ${config.cmd}")
        error()

    }
  }
}

sealed trait Command

case class LearningCmd(labeledInput: File,
                       reader: Reader[File, LabeledSentence]#Fn,
                       doCandGen: Boolean = true,
                       modelOut: File,
                       cost: Option[Double],
                       eps: Option[Double],
                       sizeForFeatureHashing: Option[Int],
                       sampleNeg: Option[Double])
    extends Command

case class EvaluationCmd(labeledInput: File,
                         reader: Reader[File, LabeledSentence]#Fn,
                         modelIn: File,
                         evalOut: Option[File],
                         maybeNFolds: Option[Int])
    extends Command

case class LearnEvaluateCmd(labeledInput: File,
                            reader: Reader[File, LabeledSentence]#Fn,
                            doCandGen: Boolean = true,
                            modelOut: File,
                            maybeNFolds: Option[Int],
                            cost: Option[Double],
                            eps: Option[Double],
                            sizeForFeatureHashing: Option[Int],
                            sampleNeg: Option[Double])
    extends Command

case class ExtractionCmd(rawInput: File,
                         reader: Reader[Any, Any],
                         modelIn: File,
                         extractOut: Option[File])
    extends Command

case class DistSupExperimentCmd(
    processedTextDir: File,
    relations: File,
    distant_label_reader: DistSupExperimentCmd.RelationKb => Reader[File, LabeledSentence]#Fn,
    doCandGen: Boolean = true,
    maybeNFolds: Option[Int],
    cost: Option[Double],
    eps: Option[Double]) // extends Command

object DistSupExperimentCmd {
  type ArgumentPair = (String, String)
  type Positive = Seq[ArgumentPair]
  type Negative = Seq[ArgumentPair]
  type RelationKb = Map[RelationLearner.Label, (Positive, Negative)]
}
