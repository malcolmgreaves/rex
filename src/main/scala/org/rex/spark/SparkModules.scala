package org.rex.spark

import nak.data.{ FeatureObservation, Featurizer }
import org.apache.spark.rdd.RDD
import org.rex.{ Candidate, CandGen, TextProcessor, Document }

object SparkModules {

  object SparkTextProcessor {

    type Id = String
    type Text = String

    type Type = RDD[(Id, Text)] => RDD[Document]

    def apply(serialized: KryoSerializationWrapper[TextProcessor]): Type =
      (data: RDD[(Id, Text)]) =>
        data
          .mapPartitions(partition => {
            val tp = serialized.getValue
            partition
              .map({
                case (id, text) =>
                  tp.process(id, text)
              })
          })
  }

  object SparkCandGen {

    type Id = String
    type Type = RDD[Document] => RDD[(Id, Seq[Candidate])]

    def apply(serialized: KryoSerializationWrapper[CandGen]): Type =
      (data: RDD[Document]) =>
        data
          .mapPartitions(
            partition => {
              val candGen = serialized.getValue
              partition.map(
                doc =>
                  (doc.id, candGen.candidates(doc))
              )
            }
          )
  }

}