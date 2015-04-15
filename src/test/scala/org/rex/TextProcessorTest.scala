package org.rex

import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

class TextProcessorTest extends FunSuite with BeforeAndAfterAll {

  import TextProcessorTest._

  private var textProcessor: TextProcessor = null

  override protected def beforeAll() = {
    System.gc()
    textProcessor = TextProcessingUtil.make()
  }

  override protected def afterAll() = {
    textProcessor = null
    System.gc()
  }

  test("text processing insurgents sentence") {
    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences(0).tokens, "Tokenization doesn't match expected")
    assert(insurgentsEntities == insurgentsDoc.sentences(0).entities.get, "NER doesn't match")
    assert(insurgentsTags == insurgentsDoc.sentences(0).tags.get, "POS tags don't match")
  }

  test("john smith sentences") {
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
    NPChunking.testChunk(johnSmithDoc.sentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
    ()
  }

}

object TextProcessorTest {

  /**
   * Relies upon testing assertions.
   * If it successeds, then it return Unit.
   * Otherwise it will fail the test
   */
  def testDocument(expectedDoc: Document, processedDoc: Document): Unit = {

    assert(
      expectedDoc.sentences.size == processedDoc.sentences.size,
      "sentence count mismatch"
    )

    val sentenceErrors =
      expectedDoc.sentences.zip(processedDoc.sentences)
        .zipWithIndex
        .foldLeft(List.empty[String])({

          case (errors, ((sExpected, sProcessed), index)) =>

            val newErrors = List(
              // tokens
              if (sExpected.tokens != sProcessed.tokens)
                Some(s"expecting sentences to match up on # $index")
              else
                None,
              // NE tags
              if (sExpected.entities.isDefined && sProcessed.entities.isDefined)
                if (sExpected.entities.get != sProcessed.entities.get)
                Some(s"expecting entities to match up on # $index")
              else
                None
              else
                Some(s"expecting entities to both be defined or undefined for sentence # $index"),
              // POS tags
              if (sExpected.tags.isDefined && sProcessed.tags.isDefined)
                if (sExpected.tags.get != sProcessed.tags.get)
                Some(s"expecting tags to match up on # $index")
              else
                None
              else
                Some(s"expecting tags to both be defined or undefined for sentence # $index")
            )

            errors ++ newErrors.flatten
        })

    assert(
      sentenceErrors.isEmpty,
      s"""expecting no errors in sentence tokens/entities/tags, actually found these ${sentenceErrors.size}: ${sentenceErrors.mkString(" ; ")}"""
    )
  }

  type Error = String

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
    Seq("He", "visited", "Beijing", ",", "on", "January 10th , 2013", ".")
  )
  val johnSmithTags = Seq(
    Seq("NNP", "NNP", "VBD", "TO", "NNP", "."),
    Seq("PRP", "VBD", "NNP", ",", "IN", "NNP", "JJ", ",", "CD", ".")
  )
  val johnSmithSentences = (0 until johnSmithTokens.size).map(index =>
    Sentence(johnSmithTokens(index), Some(johnSmithTags(index)), Some(johnSmithEntites(index)))
  )
  val johnSmithDoc = Document("john smith sentences", johnSmithSentences)
}

object TextProcessingTest extends org.scalatest.Tag("com.rex.TextProcessingTest")

trait TextProcessingTestSuite extends Serializable {

  var tp: TextProcessor = _

  def textProcessingTest(body: => Any): Unit = {
    tp = TextProcessingUtil.make()

    try {
      body

    } finally {
      tp = null
      System.gc()
    }

    ()
  }

}

object TextProcessingUtil {

  def make(entSet: NamedEntitySet = NamedEntitySet.Default4Class.entSet): TextProcessor =
    TextProcessor(
      ProcessingConf(Some(entSet), None),
      new CoreNLPProcessor(withDiscourse = false)
    )

}