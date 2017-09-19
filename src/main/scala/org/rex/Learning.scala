package org.rex

import scala.language.{postfixOps, implicitConversions}
import scala.util.Try

trait Learning[A, B] {

  type Instance = A

  type Label = B

  type Pair = (Instance, Label)

  type TrainingData = Traversable[Pair]

  type Classifier = Instance => Label

  type Estimator = Instance => Distribution[Label]

  type Learner = TrainingData => (Classifier, Estimator)
}

object Learning {

  private val emptyEstimation =
    new IllegalStateException(
      "Unexpected state: Estimator evalauted to empty Map.")

  private val empty =
    new IllegalStateException("Cannot call argmax on an empty sequence.")

  def classifierFromEstimator[A, B](
      e: Learning[A, B]#Estimator): Learning[A, B]#Classifier =
    (instance: A) => {
      // classificaiton is the item with the maximum probability
      val dist = e(instance)

      if (dist.labels isEmpty)
        throw emptyEstimation
      else if (dist.labels.size == 1)
        dist.labels.head
      else
        argmax(dist.zip)(TupleVal2[B])._1
    }

  trait Val[-A] {
    def valueOf(a: A): Double
  }

  trait TupleVal1[X] extends Val[(Double, X)] {
    override def valueOf(a: (Double, X)): Double =
      a._1
  }

  object TupleVal1 {
    def apply[X] = new TupleVal1[X] {}
  }

  trait TupleVal2[X] extends Val[(X, Double)] {
    override def valueOf(a: (X, Double)): Double =
      a._2
  }

  object TupleVal2 {
    def apply[X] = new TupleVal2[X] {}
  }

  def argmax[B: Val](xs: Traversable[B]): B =
    if (xs isEmpty)
      throw empty
    else if (xs.size == 1)
      xs.head
    else {
      val ev = implicitly[Val[B]]
      xs.foldLeft(xs.head) {
        case (max, next) =>
          if (ev.valueOf(next) > ev.valueOf(max))
            next
          else
            max
      }
    }

  def someArgmax[B: Val](xs: Traversable[B]): Option[B] =
    Try(argmax(xs)).toOption

}

trait Distribution[A] { self =>

  type Item = A

  type Probability = Double

  def labels: Seq[Item]

  def result: Seq[Probability]

  def zip: Seq[(Item, Probability)] =
    labels.zip(result)
}

case class DistributionStr(override val labels: Seq[String],
                           override val result: Seq[Double])
    extends Distribution[String] {

  assert(
    labels.size == result.size && labels.size > 0,
    s"""Labels and results sizes must be equal and positive, not: ${labels.size} and ${result.size}, respectively\nLABELS\n\t${labels
      .mkString("\n\t")}"""
  )

  override lazy val zip = super.zip
}

object Distribution {

  def renormalize[A](dist: Distribution[A]): Distribution[A] = {

    val total = dist.result.sum

    new Distribution[A] {

      override val result: Seq[Probability] =
        dist.result.map(x => x / total)

      override val labels: Seq[Item] =
        dist.labels
    }
  }

}
