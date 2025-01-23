package org.rex.relation_extract

import nak.data.FeatureObservation

import scala.language.implicitConversions

trait TextFeatuerizer[T] {

  type Input = T

  type Fn = Input => Seq[FeatureObservation[String]]
}
