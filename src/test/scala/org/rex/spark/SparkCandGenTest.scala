package org.rex.spark

import org.rex._
import org.apache.spark.rdd.RDD
import nak.data.{ FeatureObservation, Featurizer }
import org.scalatest.FunSuite

class SparkCandGenTest extends SparkTestSuite {

  import CandGenTest._

  sparkTest("Spark Candidate Generation") {

    val data = sc.parallelize(Seq(insurgentsDoc))

    val sparkCandGen = SparkCandGen(KryoSerializationWrapper(SentenceCandGen(passthruWordFilter))) _

    val createdCandidatesRDD = data
      .map(sparkCandGen)
      .filter(_.forall(_.isInstanceOf[CandidateSentence]))
      .map(_.map(_.asInstanceOf[CandidateSentence]))

    assert(createdCandidatesRDD.count() == 1)

    val createdCandidates = createdCandidatesRDD.collect().head.toSet

    val diff = insurgentsCandidatesSentence.diff(createdCandidates)
    val intersection = insurgentsCandidatesSentence.intersect(createdCandidates)

    val test = diff.size == 0 && intersection.size == insurgentsCandidatesSentence.size && intersection.size == createdCandidates.size
    assert(test,
      s"""Candidates did not match. Expecting ${insurgentsCandidatesSentence.size} actually have ${createdCandidates.size} candidates.\n""" +
        s"""Difference: "${diff.mkString(" : ")}"\nIntersection: "${intersection.mkString(" : ")}""""
    )
  }

  ignoreSparkTest("Spark Coreference-based Candidate Generation") {
    ???
  }

}