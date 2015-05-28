package org.rex.spark

import org.rex._

class SparkPipelineTest extends SparkTestSuite {

  import PipelineTest._
  import TextProcessorTest.makeProcessor
  import org.rex.TextFeatuerizerTest.featuerizer2skip2gram2gram

  sparkTest("spark data pipeline test") {

    val pipeline = SparkDataPipeline(makeProcessor())(IdentityDocChunker)(sentCGNoKnownPunct)(featuerizer2skip2gram2gram)

    val errors =
      checkPipelineOutput(
        pipeline(sc.parallelize(idTextData))
          .map({
            case (id, x) =>
              (
                id,
                Pipeline.aggregateFeatureObservations(
                  x.map(_._2).flatten
                )
              )
          })
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

