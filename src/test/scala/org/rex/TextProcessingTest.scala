package org.rex

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

class TextProcessingTest extends FunSuite with BeforeAndAfterAll {

  import TextProcessingTest._

  private var textProcessor:TextProcessor = null

  override protected def beforeAll() = {
    System.gc()
    textProcessor = makeTextProcessor()
  }

  override protected def afterAll() = {
    textProcessor = null
    System.gc()
  }

  test("text processing insurgents sentence + john smith sentences"){

    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences(0).tokens, "Tokenization doesn't match expected")
    assert(insurgentsEntities == insurgentsDoc.sentences(0).entities.get, "NER doesn't match")
    assert(insurgentsTags == insurgentsDoc.sentences(0).tags.get, "POS tags don't match")

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
  }



}

object TextProcessingTest {

  def makeTextProcessor():TextProcessor =
    TextProcessor(
      ProcessingConf(None, None),
      new CoreNLPProcessor(withDiscourse = true)
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

  val johnSmithTags = Seq(
    Seq("NNP", "NNP", "VBD", "TO", "NNP", "."),
    Seq("PRP", "VBD", "NNP", ",", "IN", "NNP", "JJ", ",", "CD", ".")
  )

}
