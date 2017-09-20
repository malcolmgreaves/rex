package org.rex.relation_extract

import org.rex.text._
import org.scalatest.FunSuite
import org.rex.SharedTestingData._

class CandGenTest extends FunSuite {

  import CandGenTest._

  test("Simple Sentence Candidate Generation") {

    val createdCandidates = sentenceCandGenAllWord(insurgentsDoc).toSet

    val diff = insurgentsCandidatesSentence.toSet.diff(createdCandidates)
    val intersection = insurgentsCandidatesSentence.toSet.intersect(createdCandidates)

    val test = diff.isEmpty &&
      intersection.size == insurgentsCandidatesSentence.size &&
      intersection.size == createdCandidates.size

    assert(
      test,
      s"""Candidates did not match.
         |Expecting ${insurgentsCandidatesSentence.size} actually have ${createdCandidates.size} candidates.
         |Difference: ${diff.mkString(" : ")}\nIntersection: ${intersection
           .mkString(" : ")}""".stripMargin
    )
  }

  test("Coreference-based Candidate Generation") {

    val doc = TextProcessorTest.process("JohnJudy", johnJudyText)

    val mentions = doc.corefMentions.getOrElse(Seq.empty[Coref])

    val candidates = CorefCandGen(WordFilter.noKnownPunct, candidateFilter)(doc)

    val actual = candidates.map(c => (c.queryW, c.answerW, c.inner)).toSet

    val errors = checkCandidates(actual, expectedJohnJudyCandidates)

    if (errors.nonEmpty)
      fail(s"""Incorrect actual candidates:\n${errors.mkString("\n")}""")
  }
}

object CandGenTest {

  val sentenceCandGenAllWord = SentenceCandGen(WordFilter.permitAll)

  lazy val candidateFilter: WordFilter.Fn =
    (s: Sentence) =>
      (i: Int) =>
        WordFilter.noKnownPunct(s)(i) && (pronounOnlyFilter(s)(i) || nounOnlyfilter(s)(i))

  lazy val nounOnlyfilter: WordFilter.Fn =
    (s: Sentence) =>
      (i: Int) =>
        s.tags.exists(tags =>
          tags(i) == "NN" || tags(i) == "NNS" || tags(i) == "NNP" || tags(i) == "NNPS")

  lazy val pronounOnlyFilter: WordFilter.Fn =
    (s: Sentence) => (i: Int) => s.tags.exists(tags => tags(i) == "PRP")

  lazy val sentenceCandGenNoKnownPunct = SentenceCandGen(WordFilter.noKnownPunct)

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

  val johnJudyText = "John drove to Judy’s house. He made her dinner."

  type SimpleCandSet = Set[(String, String, Seq[String])]

  val expectedJohnJudyCandidates = Set(
    ("John", "dinner", Seq("made", "her")),
    ("dinner", "John", Seq("made", "her")),
    ("He", "Judy", Seq("drove", "to")),
    ("Judy", "He", Seq("drove", "to")),
    ("He", "house", Seq("drove", "to", "Judy", "'s")),
    ("house", "He", Seq("drove", "to", "Judy", "'s"))
  )

  def checkCandidates(actual: SimpleCandSet, expected: SimpleCandSet): Seq[Error] = {

    val diff = actual.diff(expected)
    val inter = actual.intersect(expected)

    val diffErr =
      if (diff.nonEmpty)
        Seq(s"""Difference: ${diff.mkString(" : ")}""")
      else
        Seq.empty[String]

    val interErr =
      if (inter.size != expected.size)
        Seq(s"""Intersection: ${inter.mkString(" : ")}""")
      else
        Seq.empty[String]

    diffErr ++ interErr
  }

}
