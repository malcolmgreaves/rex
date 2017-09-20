package org.rex.spark

import org.rex.relation_extract.Pipeline
import org.rex.text.{IdentityDocChunker, TextProcessorTest}
import org.rex.relation_extract.PipelineTest.{
  sentCGNoKnownPunct,
  checkPipelineOutput,
  idTextData,
  idFeatureObs
}
import org.rex.relation_extract.TextFeatuerizerTest.featuerizer2skip2gram2gram

class SparkPipelineTest extends SparkTestSuite {

  sparkTest("spark data pipeline test") {

    val pipeline = SparkDataPipeline(TextProcessorTest)(IdentityDocChunker)(sentCGNoKnownPunct)(
      featuerizer2skip2gram2gram)

    val errors= checkPipelineOutput(
      pipeline(sc.parallelize(idTextData))
        .map {
          case (id, x) =>
            (
              id,
              Pipeline.aggregateFeatureObservations(
                x.flatMap { _._2 }
              )
            )
        }
        .filter { _._2.nonEmpty }
        .collect()
        .toSeq,
      idFeatureObs
    )
    val noErrorsTest = errors.isEmpty
    assert(
      noErrorsTest,
      s"""Errors in pipeline outputs:\n ${errors.mkString("\n")}"""
    )
  }

}
