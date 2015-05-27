package org.rex

import scala.language.implicitConversions

trait Learning[A, B] {

  type Instance = A
  type Label = B

  type Classifier = Instance => Label
  type Estimator = Instance => Distribution[Label]
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