package org.rex

import nak.data.FeatureObservation

import scala.language.implicitConversions

trait RelationLearner extends Learning[(Candidate, Seq[FeatureObservation[String]]), String]

