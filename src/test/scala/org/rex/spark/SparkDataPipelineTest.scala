package org.rex.spark

import org.rex.{ TextProcessorTest, DataPipelineTest }

class SparkDataPipelineTest extends SparkTestSuite {

  import DataPipelineTest._
  import TextProcessorTest.makeProcessor
  import org.rex.TextFeatuerizerTest.featuerizer2skip2gram2gram

  sparkTest("spark data pipeline test") {

    val pipeline = SparkDataPipeline(makeProcessor())(sentCGNoKnownPunct)(featuerizer2skip2gram2gram)

    val errors =
      checkPipelineOutput(
        pipeline(sc.parallelize(idTextData))
          .filter(_._2.nonEmpty)
          .collect().toSeq,
        idFeatureObs
      )
    val noErrorsTest = errors.isEmpty
    assert(
      noErrorsTest,
      s"""Errors in pipeline outputs:\n ${errors.mkString("\n")}"""
    )
  }

}

