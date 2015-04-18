package org.rex

import nak.data.FeatureObservation

import scala.language.implicitConversions

trait DataPipeline extends ((String, String) => Seq[(Candidate, Seq[FeatureObservation[String]])])

object DataPipeline {

  implicit class FnDataPipeline(
      f: (String, String) => Seq[(Candidate, Seq[FeatureObservation[String]])]) extends DataPipeline {
    override def apply(id: String, text: String) = f(id, text)
  }

  def apply(tp: TextProcessor)(dk: DocumentChunker)(cg: CandGen)(tf: TextFeatuerizer): DataPipeline =
    (id: String, text: String) =>
      cg.candidates(dk(tp.process(id, text)))
        .map(c => (c, aggregateFeatureObservations(tf(c))))

  def aggregateFeatureObservations(featureObservations: Seq[FeatureObservation[String]]): Seq[FeatureObservation[String]] =
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
