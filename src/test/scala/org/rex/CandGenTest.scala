package org.rex

import org.scalatest.FunSuite

class CandGenTest extends FunSuite {

  import CandGenTest._

  test("Simple Sentence Candidate Generation") {

    val createdCandidates = sentenceCandGenAllWord.candidates(insurgentsDoc).toSet

    val diff = insurgentsCandidatesSentence.toSet.diff(createdCandidates)
    val intersection = insurgentsCandidatesSentence.toSet.intersect(createdCandidates)

    val test = diff.size == 0 && intersection.size == insurgentsCandidatesSentence.size && intersection.size == createdCandidates.size
    assert(test,
      s"""Candidates did not match. Expecting ${insurgentsCandidatesSentence.size} actually have ${createdCandidates.size} candidates.\n""" +
        s"""Difference: ${diff.mkString(" : ")}\nIntersection: ${intersection.mkString(" : ")}"""
    )
  }

  ignore("Coreference-based Candidate Generation") {

    // TODO CLEAN UP println STATEMENTS !!

    val doc = TextProcessorTest.makeProcessor().process("", TextProcessorTest.johnSmithText)

    val mentions = doc.corefMentions.getOrElse(Seq.empty[Coref])
    mentions.map(
      _.mentions
        .map(mention => s"${mention.sentenceNum} [${mention.from}:${mention.until}] :: ${doc.sentences(mention.sentenceNum).tokens.slice(mention.from, mention.until)}")
        .mkString(" ; ")
    ).foreach(println)

    //    val candidates = {
    //      import NamedEntitySet.Default4Class.entSet
    //      CorefCandGen(WordFilter.default).candidates(doc)
    //    }
    //    println(s"${candidates.size} candidates")
    //    candidates.foreach(cd =>
    //      println(s"Query: ${cd.queryW} , Answer: ${cd.answerW} :: inside: ${cd.inner}")
    //    )
    ???
  }

}

object CandGenTest {

  val sentenceCandGenAllWord = SentenceCandGen(WordFilter.permitAll)

  import TextFeatuerizerTest._

  val insurgentsSentence = Sentence(insurgentsSeq)

  val insurgentsDoc = Document("insurgents", IndexedSeq(insurgentsSentence))

  val insurgentsCandidatesSentence = Seq(
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
