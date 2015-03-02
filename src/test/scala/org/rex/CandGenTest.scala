package org.rex


import org.scalatest.FunSuite

class CandGenTest extends FunSuite {

  import CandGenTest._

  test("Simple Sentence Candidate Generation") {

    val createdCandidates = SentenceCandGen(passthruWordFilter).candidates(insurgentsDoc).toSet

    val diff = insurgentsCandidatesSentence.diff(createdCandidates)
    val intersection = insurgentsCandidatesSentence.intersect(createdCandidates)

    val test = diff.size == 0 && intersection.size == insurgentsCandidatesSentence.size && intersection.size == createdCandidates.size
    assert(test,
      s"""Candidates did not match. Expecting ${insurgentsCandidatesSentence.size} actually have ${createdCandidates.size} candidates.\n""" +
        s"""Difference: ${diff.mkString(" : ")}\nIntersection: ${intersection.mkString(" : ")}"""
    )
  }

  test("Coreference-based Candidate Generation") {

    val doc = TextProcessorTest.makeTextProcessor().process("", TextProcessorTest.johnSmithText)

    val mentions = doc.corefMentions.getOrElse(Seq.empty[Coref])
    mentions.map(
      _.mentions
        .map(mention => s"${mention.sentenceNum} [${mention.from}:${mention.until}] :: ${doc.sentences(mention.sentenceNum).tokens.slice(mention.from, mention.until)}")
        .mkString(" ; ")
    ).foreach(println)

    val candidates = {
      import NamedEntitySet.Default4Class.entSet
      CorefCandGen(WordFilter.default).candidates(doc)
    }
    println(s"${candidates.size} candidates")
    candidates.foreach(cd =>
      println(s"Query: ${cd.queryW} , Answer: ${cd.answerW} :: inside: ${cd.inner}")
    )
  }

}

object CandGenTest {

  val passthruWordFilter = new WordFilter {
    override def apply(s: Sentence)(i: Int): Boolean = true
  }

  import TextFeatuerizerTest._

  val insurgentsSentence = Sentence(insurgentsSeq)

  val insurgentsDoc = Document("insurgents", IndexedSeq(insurgentsSentence))

  val insurgentsCandidatesSentence = Set(
    CandidateSentence(insurgentsSentence, 0, 1),
    CandidateSentence(insurgentsSentence, 0, 2),
    CandidateSentence(insurgentsSentence, 0, 3),
    CandidateSentence(insurgentsSentence, 0, 4),
    CandidateSentence(insurgentsSentence, 1, 0),
    CandidateSentence(insurgentsSentence, 1, 2),
    CandidateSentence(insurgentsSentence, 1, 3),
    CandidateSentence(insurgentsSentence, 1, 4),
    CandidateSentence(insurgentsSentence, 2, 0),
    CandidateSentence(insurgentsSentence, 2, 1),
    CandidateSentence(insurgentsSentence, 2, 3),
    CandidateSentence(insurgentsSentence, 2, 4),
    CandidateSentence(insurgentsSentence, 3, 0),
    CandidateSentence(insurgentsSentence, 3, 1),
    CandidateSentence(insurgentsSentence, 3, 2),
    CandidateSentence(insurgentsSentence, 3, 4),
    CandidateSentence(insurgentsSentence, 4, 0),
    CandidateSentence(insurgentsSentence, 4, 1),
    CandidateSentence(insurgentsSentence, 4, 2),
    CandidateSentence(insurgentsSentence, 4, 3)
  )

}
