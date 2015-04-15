package org.rex.spark

import nak.data.{ FeatureObservation, Featurizer }
import org.rex.{ Candidate, CandGen, TextProcessor, Document }

object SparkTextProcessor {

  type Id = String
  type Text = String

  def apply(serialized: KryoSerializationWrapper[TextProcessor])(input: (Id, Text)): Document =
    serialized.getValue.process(input._1, input._2)
}

object SparkCandGen {

  def apply(serialized: KryoSerializationWrapper[CandGen])(input: Document): Seq[Candidate] =
    serialized.getValue.candidates(input)
}

object SparkFeatuerizer {

  def apply(serialized: KryoSerializationWrapper[Featurizer[Candidate, String]])(input: Candidate): Seq[FeatureObservation[String]] =
    serialized.getValue.apply(input)
}