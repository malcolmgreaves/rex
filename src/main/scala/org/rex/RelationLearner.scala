package org.rex

import nak.NakContext._
import nak.data._
import nak.liblinear.LiblinearConfig

import scala.language.implicitConversions

trait RelationLearner extends Learning[Candidate, String]

object RelationLearner {

  def toNakExample(
    inst: RelationLearner#Instance,
    label: RelationLearner#Label): Example[RelationLearner#Label, Candidate] =

    Example(label, inst)

  type TextFeaturizer = Featurizer[Candidate, String]

  def learner(
    c: LiblinearConfig,
    tfeat: TextFeaturizer): RelationLearner#Learner =

    (examples: Seq[(RelationLearner#Instance, RelationLearner#Label)]) => {

      val nakClassifier = trainClassifier(
        c,
        tfeat,
        examples map { case (instance, label) => Example(label, instance) }
      )

      val classifier =
        (c: Candidate) => {
          ???
        }

      val estimator =
        (c: Candidate) => {
          ???
        }

      (classifier, estimator)
    }

}