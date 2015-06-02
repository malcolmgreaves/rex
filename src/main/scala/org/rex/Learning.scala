package org.rex

import scala.language.{ postfixOps, implicitConversions }
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
    new IllegalStateException("Unexpected state: Estimator evalauted to empty Map.")

  private val empty =
    new IllegalStateException("Cannot call argmax on an empty sequence.")

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
          argmax(dist)(TupleVal2[B])._1
      }
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
      xs
        .foldLeft(xs.head) {
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

trait Distribution[A] {

  def result: Array[Double]

  type Item = A

  type Probability = Double

  def apply(i: Item): Probability

  def get(i: Item): Option[Probability]

  def asMap: Map[Item, Probability]
}

object Distribution {

  def renormalize[A](dist: Distribution[A]): Distribution[A] = {
    val m = dist.asMap
    val total = m.foldLeft(0.0) { case (s, (_, v)) => s + v }
    val newM = m.map { case (item, prob) => (item, prob / total) }
    new Distribution[A] {

      override def asMap: Map[Item, Probability] =
        newM

      override def result: Array[Probability] =
        dist.result.map(_ / total)

      override def get(i: Item): Option[Probability] =
        newM.get(i)

      override def apply(i: Item): Probability =
        newM(i)
    }
  }

}