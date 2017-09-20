package org.rex.text

import org.scalatest.FunSuite

class NPChunkingTest extends FunSuite {

  import NPChunkingTest._
  import TextProcessorTest._

  test("NP chunking from text") {
    import NeTagSet.Default4Class._
    testChunk(johnSmithSentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
  }

}

object NPChunkingTest {

  /**
    * Relies upon testing assertions.
    * If it successeds, then it return Unit.
    * Otherwise it will fail the test
    */
  def testChunk(testPairs: Seq[(Sentence, Option[Seq[String]])])(implicit entSet: NeTagSet): Unit = {

    val chunker = NerSentChunker(entSet)

    val errors =
      testPairs.foldLeft(List.empty[String])({
        case (agg, (sentence, correctResponse)) =>
          val result = chunker(sentence)._1.tokens
          if (result != correctResponse)
            agg :+ s"""Sentence failed, (Chunked: ${result
              .mkString(" ")}) (Actual: ${correctResponse.mkString(" ")})"""
          else
            agg
      })

    assert(
      errors.nonEmpty,
      s"""Errors from NP chunking: ${errors.mkString(" ; ")}"""
    )
  }

}
