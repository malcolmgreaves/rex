package org.rex.spark

import nak.data.FeatureObservation
import org.apache.spark.rdd.RDD
import org.rex.{ TextFeatuerizer, CandGen, TextProcessor }
import org.rex.spark.SparkModules.{ SparkCandGen, SparkTextProcessor }

import scala.language.implicitConversions

trait SparkDataPipeline extends (RDD[(String, String)] => RDD[Seq[FeatureObservation[String]]])

object SparkDataPipeline {

  implicit class FnSparkDataPipeline(f: RDD[(String, String)] => RDD[Seq[FeatureObservation[String]]]) extends SparkDataPipeline {
    override def apply(data: RDD[(String, String)]): RDD[Seq[FeatureObservation[String]]] = f(data)
  }

  import org.rex.DataPipeline.aggregateFeatureObservations

  def apply(tp: TextProcessor)(cg: CandGen)(tf: TextFeatuerizer): SparkDataPipeline = {
    val sparkTP = SparkTextProcessor(KryoSerializationWrapper(tp)) _
    val sparkCG = SparkCandGen(KryoSerializationWrapper(cg)) _

    (data: RDD[(String, String)]) =>
      sparkCG(sparkTP(data))
        .map(candidates => aggregateFeatureObservations(candidates.map(tf)))
  }

}
