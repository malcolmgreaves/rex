package org.rex.spark

import nak.data.{ FeatureObservation, Featurizer }
import org.apache.spark.rdd.RDD
import org.rex.{ Candidate, CandGen, TextProcessor, Document }

object SparkModules {

  object SparkTextProcessor {

    type Id = String
    type Text = String

    def apply(serialized: KryoSerializationWrapper[TextProcessor])(data: RDD[(Id, Text)]): RDD[Document] =
      data
        .mapPartitions(partition => {
          val tp = serialized.getValue
          partition.map({ case (id, text) => tp.process(id, text) })
        })
  }

  object SparkCandGen {

    def apply(serialized: KryoSerializationWrapper[CandGen])(data: RDD[Document]): RDD[Seq[Candidate]] =
      data
        .mapPartitions(partition => {
          val candGen = serialized.getValue
          partition.map(candGen.candidates)
        })
  }

}