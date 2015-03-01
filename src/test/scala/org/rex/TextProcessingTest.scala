package org.rex

import org.scalatest.FunSuite
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

class TextProcessingTest extends FunSuite {

  import TextProcessingTest._


  private val textProcessor =
    TextProcessor(
      ProcessingConf(None, None),
      new CoreNLPProcessor(withDiscourse = true)
    )



  test("tokenize-based text processing") {
    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences(0).tokens, "Tokenization doesn't match expected")

    val johnSmithDoc = textProcessor.process("", johnSmithText)
    println(johnSmithDoc)
  }

//  test("entity")

}

object TextProcessingTest {

  val insurgentsText = TextFeatuerizerTest.insurgentsText
  val insurgentsTokens = Seq("Insurgents", "killed", "in", "ongoing", "fighting", ".")

  val johnSmithText = "John Smith went to China. He visited Beijing, on January 10th, 2013."
  val johnSmithTokens = Seq("John", "Smith", "went", "to", "China", ".", "He", "visited", "Beijing", ",", "on", "January 10th, 2013", ".")


}
