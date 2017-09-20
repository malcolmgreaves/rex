package org.rex.relation_extract

import nak.data.FeatureObservation
import org.rex.text.{IdentityDocChunker, TextProcessorTest, WordFilter}
import org.scalatest.FunSuite

import org.rex.SharedTestingData._
import org.rex.relation_extract.PipelineTest.{
  sentCGNoKnownPunct,
  checkPipelineOutput,
  idTextData,
  idFeatureObs
}
import org.rex.relation_extract.TextFeatuerizerTest.featuerizer2skip2gram2gram
class PipelineTest extends FunSuite {

  test("data pipeline test") {
    val pipeline = Pipeline(
      TextProcessorTest,
      IdentityDocChunker,
      sentCGNoKnownPunct,
      featuerizer2skip2gram2gram
    )
    val errors =
      checkPipelineOutput(
        idTextData
          .map {
            case (id, text) =>
              (
                id,
                Pipeline.aggregateFeatureObservations(
                  pipeline(id, text).flatMap { _._2 }
                )
              )
          }
          .filter { _._2.nonEmpty },
        idFeatureObs
      )
    val noErrorsTest = errors.isEmpty
    assert(
      noErrorsTest,
      s"""Errors in pipeline outputs:\n ${errors.mkString("\n")}"""
    )
  }
}

object PipelineTest {

  import TextFeatuerizerTest._

  val sentCGNoKnownPunct = SentenceCandGen(WordFilter.noKnownPunct)

  val idTextData = Seq(
    "1" -> insurgentsText
  )

  val idFeatureObs = Seq(
    "1" ->
      Pipeline.aggregateFeatureObservations(
        expectedFeaturesForCandGenTestInsurgentCandidatesSentence.toSeq
          .flatMap {
            case (_, answerMap) =>
              answerMap.toSeq
                .flatMap {
                  case (_, features) =>
                    features
                      .map { feature =>
                        FeatureObservation(feature, 1.0)
                      }
                }
          }
      )
  )

  type TestData = Seq[(String, Seq[FeatureObservation[String]])]

  def checkPipelineOutput(actual: TestData, expected: TestData): Seq[Error] =
    if (actual.size == expected.size) {
      actual
        .zip(expected)
        .foldLeft(Seq.empty[Error]) {

          case (errors, (a, e)) =>
            if (a._1 == e._1)
              if (a._2 == e._2)
                errors
              else
                errors :+ s"""Content mis-match on id: ${e._1},\nexpecting (${e._2.size}):\n${e._2
                  .mkString("\n")}\nactual (${a._2.size}):\n${a._2.mkString("\n")}"""
            else
              errors :+ s"Id mis-match, expecting: ${e._1} actual: ${a._1}"
        }
    } else {
      Seq(s"size mis-match, expecting: ${expected.size} actual: ${actual.size}")
    }

}
