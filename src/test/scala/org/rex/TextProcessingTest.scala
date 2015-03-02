package org.rex

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

class TextProcessingTest extends FunSuite with BeforeAndAfterAll {

  import TextProcessingTest._

  private var textProcessor: TextProcessor = null

  override protected def beforeAll() = {
    System.gc()
    textProcessor = makeTextProcessor()
  }

  override protected def afterAll() = {
    textProcessor = null
    System.gc()
  }

  ignore("text processing insurgents sentence") {
    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences(0).tokens, "Tokenization doesn't match expected")
    assert(insurgentsEntities == insurgentsDoc.sentences(0).entities.get, "NER doesn't match")
    assert(insurgentsTags == insurgentsDoc.sentences(0).tags.get, "POS tags don't match")
  }

  ignore("john smith sentences") {
    val johnSmithDoc = textProcessor.process("", johnSmithText)
    assert(johnSmithDoc.sentences.size == johnSmithTokens.size, "expecting 2 for John Smith.")
    johnSmithTokens.zipWithIndex.foreach({
      case (expected, index) =>
        assert(expected == johnSmithDoc.sentences(index).tokens,
          s"[John Smith sentence $index] expecting tokens to match up")
    })
    johnSmithEntites.zipWithIndex.foreach({
      case (expected, index) =>
        assert(expected == johnSmithDoc.sentences(index).entities.get,
          s"[John Smith sentence $index] expecting named entities to match up")
    })
    johnSmithTags.zipWithIndex.foreach({
      case (expected, index) =>
        assert(expected == johnSmithDoc.sentences(index).tags.get,
          s"[John Smith sentence $index] expecting part-of-speech tags to match up")
    })


    import NamedEntitySet.Default4Class._
    val errors = testChunk(johnSmithDoc.sentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
    if (errors.size > 0)
      fail(errors.mkString("\n"))
  }

  test("NP chunking"){
    import NamedEntitySet.Default4Class._
    testChunk(johnSmithSentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
  }

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

object TextProcessingTest {

  type Error = String

  def makeTextProcessor(): TextProcessor =
    TextProcessor(
      ProcessingConf(Some(NamedEntitySet.Default4Class.entSet), None),
      new CoreNLPProcessor(withDiscourse = false)
    )

  val insurgentsText = TextFeatuerizerTest.insurgentsText
  val insurgentsTokens = Seq("Insurgents", "killed", "in", "ongoing", "fighting", ".")
  val insurgentsEntities = insurgentsTokens.map(ignore => "O")
  val insurgentsTags = Seq("NNS", "VBN", "IN", "JJ", "NN", ".")

  val johnSmithText = "John Smith went to China. He visited Beijing, on January 10th, 2013."
  val johnSmithTokens = Seq(
    Seq("John", "Smith", "went", "to", "China", "."),
    Seq("He", "visited", "Beijing", ",", "on", "January", "10th", ",", "2013", ".")
  )
  val johnSmithEntites = Seq(
    Seq("PERSON", "PERSON", "O", "O", "LOCATION", "O"),
    Seq("O", "O", "LOCATION", "O", "O", "DATE", "DATE", "DATE", "DATE", "O")
  )
  val johnSmithChunked = Seq(
    Seq("John Smith", "went", "to", "China", "."),
    Seq("He", "visited", "Beijing", ",", "on", "January 10th, 2013", ".")
  )
  val johnSmithTags = Seq(
    Seq("NNP", "NNP", "VBD", "TO", "NNP", "."),
    Seq("PRP", "VBD", "NNP", ",", "IN", "NNP", "JJ", ",", "CD", ".")
  )
  val johnSmithSentences = (0 until johnSmithTokens.size).map(index =>
      Sentence(johnSmithTokens(index), Some(johnSmithEntites(index)), Some(johnSmithTags(index)))
  )
}
