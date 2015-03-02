package org.rex

import org.scalatest.FunSuite
import org.rex.TextProcessorTest._
import scala.Some

class NPChunking extends FunSuite  {

  import NPChunking._
  import TextProcessorTest._

  test("NP chunking from text"){
    import NamedEntitySet.Default4Class._
    val errors = testChunk(johnSmithSentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
    if (errors.size > 0)
      fail(errors.mkString("\n"))
  }

}

object NPChunking {

  def testChunk(testPairs: Seq[(Sentence, Option[Seq[String]])])(implicit entSet: NamedEntitySet): List[Error] =
    testPairs.foldLeft(List.empty[String])({
      case (agg, (sentence, correctResponse)) =>
        val result = Sentence.chunkTokens(sentence)
        if (result != correctResponse)
          agg :+ s"""Sentence failed, (Chunked: ${result.mkString(" ")}) (Actual: ${correctResponse.mkString(" ")})"""
        else
          agg
    })

}


