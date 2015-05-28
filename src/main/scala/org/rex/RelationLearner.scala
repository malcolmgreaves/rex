package org.rex

import nak.NakContext._
import nak.core.Classifier
import nak.data._
import nak.liblinear.LiblinearConfig
import org.rex.Learning.TupleVal1

import scala.language.implicitConversions

trait RelationLearner extends Learning[Candidate, String]

object RelationLearner {

  @inline def toNakExample(
    inst: RelationLearner#Instance,
    label: RelationLearner#Label): Example[RelationLearner#Label, Candidate] =

    Example(label, inst)

  type CandidateTextFeaturizer = Featurizer[Candidate, String]

  def apply(
    conf: LiblinearConfig,
    tfeat: CandidateTextFeaturizer,
    sizeForFeatureHashing: Option[Int] = None): RelationLearner#Learner =

    (examples: Seq[(RelationLearner#Instance, RelationLearner#Label)]) => {

      val nakFmtExamples =
        examples.map { case (instance, label) => Example(label, instance) }

      // Featurize and index the examples.

      val nakClassifier =
        sizeForFeatureHashing match {

          case Some(featureSpaceSize) =>
            val indexer = new HashedExampleIndexer(featureSpaceSize)
            val primeNumberOfFeatures = indexer.highestFeatureIndex
            val trainingExamples = nakFmtExamples.map(_.map(tfeat)).map(indexer)
            val (lmap, fmap) = indexer.getMaps
            Classifier(
              trainModel(conf, trainingExamples, primeNumberOfFeatures + 1),
              lmap,
              fmap,
              tfeat
            )

          case None =>
            val indexer = new ExampleIndexer
            val trainingExamples = nakFmtExamples.map(_.map(tfeat)).map(indexer)
            val (lmap, fmap) = indexer.getMaps
            Classifier(
              trainModel(conf, trainingExamples, fmap.size),
              lmap,
              fmap,
              tfeat
            )
        }

      val label2index = nakClassifier.lmap
      val index2label = label2index.toList.sortBy(_._2).map(_._1).toSeq

      val estimator =
        (c: Candidate) =>
          new Distribution[RelationLearner#Label] {

            private val result = nakClassifier.evalRaw(c)

            override def apply(i: Item): Probability =
              result(label2index(i))

            override lazy val asMap: Map[Item, Probability] =
              label2index.map { case (label, index) => (label, result(index)) }

            override def get(i: Item): Option[Probability] =
              label2index.get(i)
                .flatMap(index =>
                  if (index >= 0 && index < result.length)
                    Some(result(index))
                  else
                    None
                )
          }

      val classifier =
        if (index2label.size < 2)
          (c: Candidate) =>
            throw new IllegalStateException(s"Must have >= 2 labels present in training, not ${index2label.size}")
        else
          (c: Candidate) =>
            index2label(
              Learning.argmax(nakClassifier.evalRaw(c).zipWithIndex)(Int1TupleVal)
                ._2
            )

      (classifier, estimator)
    }

  private val Int1TupleVal = TupleVal1[Int]

}