package org.rex.spark

import nak.data.{FeatureObservation, Featurizer}
import org.apache.spark.rdd.RDD
import org.rex.relation_extract.{CandGen, Candidate}
import org.rex.text.TextProcessor
import org.rex.Document

object SparkModules {

  object SparkTextProcessor {

    type Id = String
    type Text = String

    type Fn = RDD[(Id, Text)] => RDD[Document]

    def apply(serialized: KryoSerializationWrapper[TextProcessor]): Fn =
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

    type Fn = RDD[Document] => RDD[(Id, Seq[Candidate])]

    def apply(serialized: KryoSerializationWrapper[CandGen.Fn]): Fn =
      (data: RDD[Document]) =>
        data
          .mapPartitions(
            partition => {
              val candGen = serialized.getValue
              partition.map(
                doc => (doc.id, candGen(doc))
              )
            }
        )
  }

}
