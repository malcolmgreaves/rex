package org.rex

import org.scalatest.FunSuite
import org.rex.TextProcessorTest._
import scala.Some

class NPChunkingTest extends FunSuite {

  import NPChunkingTest._
  import TextProcessorTest._

  test("NP chunking from text") {
    import NamedEntitySet.Default4Class._
    testChunk(johnSmithSentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
  }

}

object NPChunkingTest {

  /**
   * Relies upon testing assertions.
   * If it successeds, then it return Unit.
   * Otherwise it will fail the test
   */
  def testChunk(testPairs: Seq[(Sentence, Option[Seq[String]])])(implicit entSet: NamedEntitySet): Unit = {

    val errors =
      testPairs.foldLeft(List.empty[String])({
        case (agg, (sentence, correctResponse)) =>
          val result = Sentence.chunkTokens(sentence)
          if (result != correctResponse)
            agg :+ s"""Sentence failed, (Chunked: ${result.mkString(" ")}) (Actual: ${correctResponse.mkString(" ")})"""
          else
            agg
      })

    assert(
      errors.nonEmpty,
      s"""Errors from NP chunking: ${errors.mkString(" ; ")}"""
    )
  }

}