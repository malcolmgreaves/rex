package org.rex.spark

import org.rex.{ TextProcessorTest, DataPipelineTest }

class SparkDataPipelineTest extends SparkTestSuite {

  import DataPipelineTest._
  import TextProcessorTest.makeProcessor

  sparkTest("spark data pipeline test") {

    val pipeline = SparkDataPipeline(makeProcessor())(sentCGNoKnownPunct)(feat4skip2gramNoPeriods)

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

