package org.rex

import nak.data.FeatureObservation

import scala.language.implicitConversions

trait DataPipeline extends ((String, String) => Seq[FeatureObservation[String]])

object DataPipeline {

  implicit class FnDataPipeline(f: (String, String) => Seq[FeatureObservation[String]]) extends DataPipeline {
    override def apply(id: String, text: String): Seq[FeatureObservation[String]] = f(id, text)
  }

  def apply(tp: TextProcessor)(cg: CandGen)(tf: TextFeatuerizer): DataPipeline =
    (id: String, text: String) =>
      aggregateFeatureObservations(cg.candidates(tp.process(id, text)).map(tf))

  type FeatureValues = Seq[FeatureObservation[String]]

  def aggregateFeatureObservations(x: Seq[FeatureValues]): FeatureValues =
    x
      .foldLeft(Map.empty[String, Double])({

        case (m, featureObservations) =>
          featureObservations
            .foldLeft(m)({

              case (mapping, fobs) => mapping.get(fobs.feature) match {

                case Some(existing) =>
                  (mapping - fobs.feature) + (fobs.feature -> (existing + fobs.magnitude))

                case None =>
                  mapping + (fobs.feature -> fobs.magnitude)
              }
            })
      })
      .toSeq
      .map({
        case (feature, value) =>
          FeatureObservation(feature, value)
      })

}
