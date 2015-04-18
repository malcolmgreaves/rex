package org.rex.spark

import org.rex._

import SparkModules._

class SparkCandGenTest extends SparkTestSuite {

  import CandGenTest._

  sparkTest("Spark Candidate Generation") {

    val data = sc.parallelize(Seq(insurgentsDoc))

    val createdCandidatesRDD =
      SparkCandGen(KryoSerializationWrapper(sentenceCandGenNoKnownPunct))(data)
        .map(_._2)
        .filter(_.forall(_.isInstanceOf[CandidateSentence]))
        .map(_.map(_.asInstanceOf[CandidateSentence]))

    assert(createdCandidatesRDD.count() == 1)

    val createdCandidates = createdCandidatesRDD.collect().head.toSet

    val diff = insurgentsCandidatesSentence.toSet.diff(createdCandidates)
    val intersection = insurgentsCandidatesSentence.toSet.intersect(createdCandidates)

    val test =
      diff.size == 0 &&
        intersection.size == insurgentsCandidatesSentence.size &&
        intersection.size == createdCandidates.size

    assert(test,
      s"""Candidates did not match. Expecting ${insurgentsCandidatesSentence.size} actually have ${createdCandidates.size} candidates.\n""" +
        s"""Difference: "${diff.mkString(" : ")}"\nIntersection: "${intersection.mkString(" : ")}""""
    )
  }

  ignoreSparkTest("Spark Coreference-based Candidate Generation") {

  }

}