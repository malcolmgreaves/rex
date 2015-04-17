package org.rex.spark

import nak.data.FeatureObservation
import org.apache.spark.rdd.RDD
import org.rex.{ TextFeatuerizer, CandGen, TextProcessor }
import org.rex.spark.SparkModules.{ SparkTextProcessor, SparkCandGen }

import scala.language.implicitConversions

trait SparkDataPipeline extends (RDD[(String, String)] => RDD[(String, Seq[FeatureObservation[String]])])

object SparkDataPipeline {

  type IdFeatures = (String, Seq[FeatureObservation[String]])

  implicit class FnSparkDataPipeline(
      f: RDD[(String, String)] => RDD[IdFeatures]) extends SparkDataPipeline {
    override def apply(data: RDD[(String, String)]): RDD[IdFeatures] = f(data)
  }

  import org.rex.DataPipeline.aggregateFeatureObservations

  def apply(tp: TextProcessor)(cg: CandGen)(tf: TextFeatuerizer): SparkDataPipeline =
    apply(SparkTextProcessor(KryoSerializationWrapper(tp)))(SparkCandGen(KryoSerializationWrapper(cg)))(tf)

  @inline private def apply(stp: SparkTextProcessor.Type)(scg: SparkCandGen.Type)(tf: TextFeatuerizer): SparkDataPipeline =
    (data: RDD[(String, String)]) =>
      scg(stp(data))
        .map({
          case (id, candidates) =>
            (id, aggregateFeatureObservations(candidates.map(tf)))
        })

}
