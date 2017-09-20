package org.rex

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import org.rex.relation_extract.ProcessingConf
import org.rex.text.TextProcessor

class TextProcessorTest extends FunSuite {

  import TextProcessorTest._
  import NPChunkingTest._

  private val textProcessor = TextProcessorTest

  test("text processing insurgents sentence") {
    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences.head.tokens, "Tokenization doesn't match expected")
    assert(insurgentsEntities == insurgentsDoc.sentences.head.entities.get, "NER doesn't match")
    assert(insurgentsTags == insurgentsDoc.sentences.head.tags.get, "POS tags don't match")
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

    import org.rex.relation_extract.NeTagSet.Default4Class._
    testChunk(johnSmithDoc.sentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
  }

  test("NP chunking") {
    import org.rex.relation_extract.NeTagSet.Default4Class._
    testChunk(johnSmithSentences.zipWithIndex.map(x => (x._1, Some(johnSmithChunked(x._2)))))
  }

}

object TextProcessorTest extends TextProcessor {

  override val conf = ProcessingConf.DefaultProcConf.procConf

  private lazy val sharedCoreNlpProcessor = CoreNlpTextProcessor(conf)

  override def process(id: String, text: String): Document =
    synchronized {
      sharedCoreNlpProcessor.process(id, text)
    }

  /**
   * Relies upon testing assertions.
   * If it successeds, then it return Unit.
   * Otherwise it will fail the test
   */
  def testDocument(expectedDoc: Document, processedDoc: Document): Unit = {

    assert(
      expectedDoc.sentences.size == processedDoc.sentences.size,
      s"sentence count mismatch: expecting ${expectedDoc.sentences.size} actual ${processedDoc.sentences.size}"
    )

    def checkSeqs(seq1: Seq[String], seq2: Seq[String], message: String): List[Error] =
      if (seq1.size == seq2.size)
        seq1.zip(seq2)
          .foldLeft(List.empty[Error])({

            case (errors, (s1, s2)) =>
              if (s1 != s2)
                errors :+ s"[$message] didn't match: $s1 vs. $s2"
              else
                errors
          })
      else
        List(s"[$message] Sequences did not match because they have different sizes: ${seq1.size} vs ${seq1.size}")

    val sentenceErrors =
      expectedDoc.sentences.zip(processedDoc.sentences)
        .zipWithIndex
        .foldLeft(List.empty[Error])({

          case (errors, ((sExpected, sProcessed), index)) =>

            val newErrors = List(
              checkSeqs(
                sExpected.tokens,
                sProcessed.tokens,
                "tokens"
              ),
              checkSeqs(
                sExpected.entities.getOrElse(List.empty[String]),
                sProcessed.entities.getOrElse(List.empty[String]),
                "named entities"
              ),
              checkSeqs(
                sExpected.tags.getOrElse(List.empty[String]),
                sProcessed.tags.getOrElse(List.empty[String]),
                "pos tags"
              )
            )

            errors ++ newErrors.flatten
        })

    assert(
      sentenceErrors.isEmpty,
      s"""expecting no errors in sentence tokens/entities/tags, actually found these ${sentenceErrors.size}:
         |${sentenceErrors.mkString(" ; ")}""".stripMargin
    )
  }

  type Error = String

  /////////////////////////////////////////////////////////////////////////////////////////////////

  val insurgentsText = TextFeatuerizerTest.insurgentsText

  val insurgentsTokens = Seq("Insurgents", "killed", "in", "ongoing", "fighting", ".")

  val insurgentsEntities = insurgentsTokens.map(ignore => "O")

  val insurgentsTags = Seq("NNS", "VBN", "IN", "JJ", "NN", ".")

  /////////////////////////////////////////////////////////////////////////////////////////////////

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

  val johnSmithSentences = (0 until johnSmithTokens.size).map(index =>
    Sentence(johnSmithTokens(index), Some(johnSmithTags(index)), Some(johnSmithEntites(index)))
  )

  val johnSmithDoc = Document("john smith sentences", johnSmithSentences)

  val johnSmithChunked = Seq(
    Seq("John Smith", "went", "to", "China", "."),
    Seq("He", "visited", "Beijing", ",", "on", "January 10th, 2013", ".")
  )

  /////////////////////////////////////////////////////////////////////////////////////////////////
}
