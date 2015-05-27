package org.rex

import scala.language.{ postfixOps, implicitConversions }

trait Learning[A, B] {

  type Instance = A
  type Label = B

  type Classifier = Instance => Label
  type Estimator = Instance => Distribution[Label]
  type Learner = Seq[(Instance, Label)] => (Classifier, Estimator)
}

object Learning {

  private val emptyEstimation =
    new IllegalStateException("Unexpected state: Estimator evalauted to empty Map.")

  def classifierFromEstimator[A, B](e: Learning[A, B]#Estimator): Learning[A, B]#Classifier =
    (instance: A) => {
      // do estimation, get it as mapping
      val dist = e(instance).asMap
      // classificaiton is the item with the maximum probability
      dist.size match {
        case 0 =>
          throw emptyEstimation

        case 1 =>
          dist.head._1

        case n =>
          dist.foldLeft(dist.head) {
            case (max @ (maxItem, maxProb), next @ (item, prob)) =>
              if (prob > maxProb)
                next
              else
                max
          }
            ._1
      }
    }

}

trait Distribution[A] {

  type Item = A

  type Probability = Double

  def apply(i: Item): Probability

  def get(i: Item): Option[Probability]

  def asMap: Map[Item, Probability]
}

object Distribution {

  def fromMap[A](m: Map[A, Double]): Distribution[A] =
    new Distribution[A] {

      override def asMap: Map[Item, Probability] =
        m

      override def apply(i: Item): Probability =
        m(i)

      override def get(i: Item): Option[Probability] =
        m.get(i)
    }

  def renormalize[A](dist: Distribution[A]): Distribution[A] = {
    val m = dist.asMap
    val total = m.foldLeft(0.0) { case (s, (_, v)) => s + v }
    fromMap(m.map { case (item, prob) => (item, prob / total) })
  }

}