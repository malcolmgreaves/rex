package org.rex

import nak.data.{FeatureObservation, Featurizer}
import org.rex.AdjacentFeatures._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import spire.syntax.cfor._

trait TextFeatuerizer[T] {

  type Input = T

  type Fn = Input => Seq[FeatureObservation[String]]
}
