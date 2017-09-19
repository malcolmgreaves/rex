package org.rex.spark

import nak.data.FeatureObservation
import org.apache.spark.rdd.RDD
import org.rex._
import org.rex.spark.SparkModules.{SparkTextProcessor, SparkCandGen}

import scala.language.implicitConversions

trait SparkDataPipeline
    extends (RDD[(String, String)] => RDD[
      (String, Seq[(Candidate, Seq[FeatureObservation[String]])])])

object SparkDataPipeline {

  implicit class FnSparkDataPipeline(
      f: RDD[(String, String)] => RDD[
        (String, Seq[(Candidate, Seq[FeatureObservation[String]])])])
      extends SparkDataPipeline {
    override def apply(data: RDD[(String, String)]) = f(data)
  }

  import org.rex.Pipeline.aggregateFeatureObservations

  def apply(tp: TextProcessor)(dk: DocumentChunker.Fn)(cg: CandGen.Fn)(
      tf: CandidateFeatuerizer.Fn): SparkDataPipeline =
    apply(SparkTextProcessor(KryoSerializationWrapper(tp)))(dk)(
      SparkCandGen(KryoSerializationWrapper(cg)))(tf)

  @inline private def apply(stp: SparkTextProcessor.Fn)(dk: DocumentChunker.Fn)(
      scg: SparkCandGen.Fn)(tf: CandidateFeatuerizer.Fn): SparkDataPipeline =
    (data: RDD[(String, String)]) =>
      scg(stp(data).map(dk))
        .map({
          case (id, candidates) =>
            (
              id,
              candidates
                .map(c => (c, aggregateFeatureObservations(tf(c))))
            )
        })

}
