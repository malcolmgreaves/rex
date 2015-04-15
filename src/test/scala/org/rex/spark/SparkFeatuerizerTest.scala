package org.rex.spark

import org.rex._

class SparkFeatuerizerTest extends SparkTestSuite {

  import TextFeatuerizerTest._

  sparkTest("Spark k-skip n-gram feature generation code (InsideFeatures)") {

    val sparkFeatuerizer = {
      val serialized = KryoSerializationWrapper(InsideFeatures(InsideFeatures(2, 2)) _)
      (input: Seq[String]) =>
        serialized.getValue.apply(input)
    }

    val data = sc.parallelize(Seq(insurgentsSeq))

    val createdFeaturesRdd = data.map(sparkFeatuerizer)

    assert(createdFeaturesRdd.count() == 1)

    val expectedFeatures = insurgents2skipBigrams ++ insurgentsUnigrams ++ insurgentsBigrams
    val actualFeatures = createdFeaturesRdd.collect().head.toSet

    val diff = expectedFeatures.diff(actualFeatures)
    val intersection = expectedFeatures.intersect(actualFeatures)

    assert(diff.size == 0 && intersection.size == expectedFeatures.size && intersection.size == actualFeatures.size,
      s"""Features did not match\nDifference: ${diff.mkString(" : ")}\nIntersection: ${intersection.mkString(" : ")}"""
    )
  }

  ignoreSparkTest("Spark full-on Featuerizer using candidates") {
    ???
  }

}