package org.rex.text

import org.rex.SharedTestingData._
import org.scalatest.FunSuite

class TextProcessorTest extends FunSuite {

  import NPChunkingTest._

  private val textProcessor = TextProcessorTest

  test("text processing insurgents sentence") {
    val insurgentsDoc = textProcessor.process("", insurgentsText)
    assert(insurgentsDoc.sentences.size == 1, "expecting 1 sentence")
    assert(insurgentsTokens == insurgentsDoc.sentences.head.tokens,
           "Tokenization doesn't match expected")
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

    import NeTagSet.Default4Class._
    testChunk(johnSmithDoc.sentences.zipWithIndex.map { x =>
      (x._1, Some(johnSmithChunked(x._2)))
    })
  }

  test("NP chunking") {
    import NeTagSet.Default4Class._
    testChunk(johnSmithSentences.zipWithIndex.map { x =>
      (x._1, Some(johnSmithChunked(x._2)))
    })
  }

}

object TextProcessorTest extends TextProcessor {

  override val conf: ProcessingConf = ProcessingConf.DefaultProcConf.procConf

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
      s"sentence count mismatch: expecting ${expectedDoc.sentences.size} " +
        s"actual ${processedDoc.sentences.size}"
    )

    def checkSeqs(seq1: Seq[String], seq2: Seq[String], message: String): List[Error] =
      if (seq1.size == seq2.size)
        seq1
          .zip(seq2)
          .foldLeft(List.empty[Error])({

            case (errors, (s1, s2)) =>
              if (s1 != s2)
                errors :+ s"[$message] didn't match: $s1 vs. $s2"
              else
                errors
          })
      else
        List(
          s"[$message] Sequences did not match because they have different sizes: " +
            s"${seq1.size} vs ${seq1.size}")

    val sentenceErrors =
      expectedDoc.sentences
        .zip(processedDoc.sentences)
        .foldLeft(List.empty[Error]) {
          case (errors, (sExpected, sProcessed)) =>
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
        }

    assert(
      sentenceErrors.isEmpty,
      s"""expecting no errors in sentence tokens/entities/tags, actually found these ${sentenceErrors.size}:
         |${sentenceErrors.mkString(" ; ")}""".stripMargin
    )
  }
}
