package org.rex

import nak.data.FeatureObservation

import scala.language.implicitConversions

object Pipeline {

  type Id = String
  type Text = String
  type Features = Seq[FeatureObservation[String]]

  type OfCandidates = (Id, Text) => Seq[Candidate]

  def apply(tp: TextProcessor, dk: DocumentChunker, cg: CandGen): OfCandidates =
    (id: Id, text: Text) =>
      cg.candidates(dk(tp.process(id, text)))

  type OfFeatsAndCands = (Id, Text) => Seq[(Candidate, Features)]

  def apply(tp: TextProcessor, dk: DocumentChunker, cg: CandGen, tf: TextFeatuerizer): OfFeatsAndCands =
    (id: Id, text: Text) =>
      cg.candidates(dk(tp.process(id, text)))
        .map(c => (c, aggregateFeatureObservations(tf(c))))

  @inline def aggregateFeatureObservations(featureObservations: Features): Features =
    featureObservations
      .foldLeft(Map.empty[String, Double])({
        case (mapping, fobs) => mapping.get(fobs.feature) match {

          case Some(existing) =>
            (mapping - fobs.feature) + (fobs.feature -> (existing + fobs.magnitude))

          case None =>
            mapping + (fobs.feature -> fobs.magnitude)
        }
      })
      .toSeq
      .map({
        case (feature, value) =>
          FeatureObservation(feature, value)
      })

}