package org.rex.relation_extract

import nak.NakContext._
import nak.core.{Classifier, FeaturizedClassifier, LiblinearClassifier}
import nak.data._
import nak.liblinear._
import org.rex.relation_extract.Learning.TupleVal1

import scala.language.implicitConversions
import scala.util.Try

object RelationLearner extends Learning[Candidate, String] {

  @inline
  def toNakExample(inst: Instance, label: Label): Example[Label, Instance] =
    Example(label, inst)

  import org.rex.relation_extract.CandidateFeatuerizer._

  def apply(conf: LiblinearConfig,
            tfeat: CandidateFeatuerizer.Fn,
            sizeForFeatureHashing: Option[Int] = None): Learner =
    (examples: Traversable[(Instance, Label)]) => {

      val nakFmtExamples =
        examples.map { case (instance, label) => Example(label, instance) }

      // Featurize and index the examples.

      val __nakClassifier =
        sizeForFeatureHashing match {

          case Some(featureSpaceSize) =>
            val indexer = new HashedExampleIndexer(featureSpaceSize)
            val primeNumberOfFeatures = indexer.highestFeatureIndex
            val trainingExamples =
              nakFmtExamples.map(_.map(tfeat)).map(indexer)
            val (lmap, fmap) = indexer.getMaps
            Classifier(
              trainModel(conf, trainingExamples, primeNumberOfFeatures + 1),
              lmap,
              fmap,
              tfeat
            )

          case None =>
            val indexer = new ExampleIndexer
            val trainingExamples =
              nakFmtExamples.map(_.map(tfeat)).map(indexer)
            val (lmap, fmap) = indexer.getMaps
            Classifier(
              trainModel(conf, trainingExamples, fmap.size),
              lmap,
              fmap,
              tfeat
            )
        }

      val nakClassifier = new LiblinearClassifier with FeaturizedClassifier[String, Candidate] {

        override def fmap: FeatureMap = __nakClassifier.fmap
        override val lmap: Map[String, Int] = __nakClassifier.lmap
        override val model: Model = __nakClassifier.model
        override val featurizer: Featurizer[Candidate, String] =
          __nakClassifier.featurizer

        override def apply(context: Array[(Int, Double)]): Array[Double] = {
          val ctxt =
            context.map(c => new FeatureNode(c._1, c._2).asInstanceOf[nak.liblinear.Feature])
          Try {
            val labelScores = Array.fill(numLabels)(0.0)
            Linear.predictProbability(model, ctxt, labelScores)
            labelScores

          } getOrElse {
            Array(Linear.predict(model, ctxt))
          }
        }

      }

      val label2index = nakClassifier.lmap
      val index2label = label2index.toList.sortBy(_._2).map(_._1).toSeq

      val estimator =
        if (index2label.size == 2)
          (c: Candidate) =>
            DistributionStr(
              index2label, {
                val p = nakClassifier.evalRaw(c).head
                Seq(p, 1.0 - p)
              }
          )
        else
          (c: Candidate) => DistributionStr(index2label, nakClassifier.evalRaw(c).toSeq)

      val classifier =
        if (index2label.size < 2)
          (c: Candidate) =>
            throw new IllegalStateException(
              s"Must have >= 2 labels present in training, not ${index2label.size}")
        else
          (c: Candidate) =>
            index2label(
              Learning
                .argmax(nakClassifier.evalRaw(c).zipWithIndex)(Int1TupleVal)
                ._2
          )

      (classifier, estimator)
    }

  private val Int1TupleVal = TupleVal1[Int]

}
