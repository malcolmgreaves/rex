package org.rex

import nak.data.FeatureObservation
import org.scalatest.FunSuite

class TextFeatuerizerTest extends FunSuite {

  import TextFeatuerizerTest._

  ////////////////////////////////////////////////////////////////////////////////

  def testInsideFeatures(expected: Set[String], actual: Set[String]): Unit = {
    val diff = expected.diff(actual)
    val intersection = expected.intersect(actual)
    assert(
      diff.size == 0
        && intersection.size == expected.size
        && intersection.size == actual.size,
      s"""Features did not match\nDifference: ${diff.mkString(" : ")}\nIntersection: ${intersection.mkString(" : ")}"""
    )
  }

  test("inside features: 2-skip 2-gram on sentence") {
    testInsideFeatures(
      insurgents2skipBigrams ++ insurgentsUnigrams ++ insurgentsBigrams,
      InsideFeatures(insideConf2skip2gram)(insurgentsSeq).toSet
    )
  }

  test("inside features: on empty seq") {
    testInsideFeatures(
      Set.empty[String],
      InsideFeatures(insideConf2skip2gram)(Seq.empty[String]).toSet
    )
  }

  test("inside features: on seq of size 1") {
    val word = "hello"
    testInsideFeatures(
      Set(word),
      InsideFeatures(insideConf2skip2gram)(Seq(word)).toSet
    )
  }

  test("inside features: on an undersized seq for 2-skip 2-gram") {
    testInsideFeatures(
      undersizedGrams,
      InsideFeatures(insideConf2skip2gram)(undersizedSeq).toSet
    )
  }

  ////////////////////////////////////////////////////////////////////////////////

  def testAdjacentFeatures(expectedFeatures: Seq[String], actualFeatures: Seq[String]): Unit =
    assert(
      actualFeatures == expectedFeatures,
      s"""Expecting "${expectedFeatures.mkString(",")}" Actual: "${actualFeatures.mkString(",")}""""
    )

  test("adjacent features [left]: on full sentence word sequence") {
    testAdjacentFeatures(
      Seq("insurgents", "killed"),
      AdjacentFeatures.left(insurgentsSeq.map(_.toLowerCase), 2)(adjacentConf2gram.ngramWidth)
    )
  }

  test("adjacent features [left]: on undersized seq") {
    testAdjacentFeatures(
      Seq(undersizedSeq.head),
      AdjacentFeatures.left(undersizedSeq, 1)(adjacentConf2gram.ngramWidth)
    )
  }

  test("adjacent features [left]: degenerate cases") {
    Seq(-162161, 0)
      .foreach(index =>
        testAdjacentFeatures(
          Seq.empty[String],
          AdjacentFeatures.left(insurgentsSeq.map(_.toLowerCase), index)(adjacentConf2gram.ngramWidth)
        )
      )
  }

  test("adjacent features [right]: on full sentence word sequence") {
    testAdjacentFeatures(
      Seq("killed", "in"),
      AdjacentFeatures.right(insurgentsSeq.map(_.toLowerCase), 0)(adjacentConf2gram.ngramWidth)
    )
  }

  test("adjacent features [right]: on undersized seqemce") {
    testAdjacentFeatures(
      Seq(undersizedSeq.reverse.head),
      AdjacentFeatures.right(undersizedSeq, 0)(adjacentConf2gram.ngramWidth)
    )
  }

  test("adjacent features [right]: degenerate cases") {
    Seq(insurgentsSeq.size - 1, insurgentsSeq.size, insurgentsSeq.size * 10)
      .foreach(index =>
        testAdjacentFeatures(
          Seq.empty[String],
          AdjacentFeatures.right(insurgentsSeq.map(_.toLowerCase), index)(adjacentConf2gram.ngramWidth)
        )
      )
  }

  ////////////////////////////////////////////////////////////////////////////////

  import CandGenTest.insurgentsCandidatesSentence

  test("featuerizer on simple undersized sentence case (inside 4-skip 2-grams + adjacent 2-grams)") {

    val candidate =
      insurgentsCandidatesSentence
        .filter(cand => cand.queryW == "Insurgents" && cand.answerW == "killed")
        .head

    val features = featuerizer4skip2gram2gram(candidate)

    checkCandidate(expectedFeaturesForCandGenTestInsurgentCandidatesSentence)(
      candidate.queryW.toLowerCase, candidate.answerW.toLowerCase, featuresAsStrSet(features)) match {

        case Some(error) =>
          fail(s"Candidate: $candidate = (${candidate.queryW}, ${candidate.answerW}) Featurization Error:\n$error")

        case None => () // success!
      }
  }

  test("full-on Featuerizer using candidates (inside 2-skip 2-grams + adjacent 2-grams)") {

    val featuerized =
      insurgentsCandidatesSentence
        .map(cand => (cand, featuerizer2skip2gram2gram(cand)))
        .toList
        .sortBy(_._1.queryW)

    val errors = {
      val checker = checkCandidate(expectedFeaturesForCandGenTestInsurgentCandidatesSentence) _
      featuerized
        .flatMap({
          case (cand, features) =>
            checker(cand.queryW.toLowerCase, cand.answerW.toLowerCase, featuresAsStrSet(features))
        })
    }

    val noErrors = errors.isEmpty
    assert(
      noErrors,
      s"""Featurization Errors:\n${errors.mkString("\n")}"""
    )
  }

}

object TextFeatuerizerTest {
  ////////////////////////////////////////////////////////////////////////////////

  val insurgentsText = "Insurgents killed in ongoing fighting."
  val insurgentsSeq = Seq("Insurgents", "killed", "in", "ongoing", "fighting")

  val insurgentsUnigrams = Set("Insurgents", "killed", "in", "ongoing", "fighting")
  val insurgentsBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting")
  val insurgentsTrigrams = Set("Insurgents,killed,in", "killed,in,ongoing", "in,ongoing,fighting")

  val insurgents1skipBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting", "Insurgents,in",
    "killed,ongoing", "in,fighting")
  val insurgents2skipBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting", "Insurgents,in",
    "killed,ongoing", "in,fighting", "Insurgents,ongoing", "killed,fighting")

  ////////////////////////////////////////////////////////////////////////////////

  val undersizedText = "hello world"
  val undersizedSeq = Seq("hello", "world")

  val undersizedGrams = Set("hello", "world", "hello,world")

  ////////////////////////////////////////////////////////////////////////////////

  val insideConf2skip2gram = InsideFeatures(2, 2)
  val insideConf4skip2gram = InsideFeatures(2, 4)

  val adjacentConf2gram = AdjacentFeatures(2)

  val wordFilter = WordFilter.noKnownPunct
  val wordView = WordView.lowercase

  val featuerizer2skip2gram2gram = TextFeatuerizer(
    Some((adjacentConf2gram, SentenceViewFilter.noKnownPunctLowercase)),
    Some((insideConf2skip2gram, wordFilter, wordView))
  )

  val featuerizer4skip2gram2gram = TextFeatuerizer(
    Some((adjacentConf2gram, SentenceViewFilter.noKnownPunctLowercase)),
    Some((insideConf4skip2gram, wordFilter, wordView))
  )

  ////////////////////////////////////////////////////////////////////////////////

  def featuresAsStrSet(fs: Seq[FeatureObservation[String]]): Set[String] =
    fs.map(_.feature).toSet

  // query -> answer -> Set(FeatureObservation)
  // for sentence text "Insurgents killed in ongoing fighting."
  val expectedFeaturesForCandGenTestInsurgentCandidatesSentence = Map(
    "insurgents" -> Map(
      ("killed", Seq("in", "in,ongoing")),
      ("in", Seq("killed", "ongoing", "ongoing,fighting")),
      ("ongoing", Seq("killed", "killed,in", "in", "fighting")),
      ("fighting", Seq("killed", "killed,in", "killed,ongoing", "in", "in,ongoing", "ongoing"))
    ),
    "killed" -> Map(
      ("insurgents", Seq("in", "in,ongoing")),
      ("in", Seq("insurgents", "ongoing", "ongoing,fighting")),
      ("ongoing", Seq("insurgents", "in", "fighting")),
      ("fighting", Seq("insurgents", "in", "ongoing", "in,ongoing"))
    ),
    "in" -> Map(
      ("insurgents", Seq("killed", "ongoing", "ongoing,fighting")),
      ("killed", Seq("insurgents", "ongoing", "ongoing,fighting")),
      ("ongoing", Seq("insurgents,killed", "killed", "fighting")),
      ("fighting", Seq("insurgents,killed", "killed", "ongoing"))
    ),
    "ongoing" -> Map(
      ("insurgents", Seq("killed", "killed,in", "in", "fighting")),
      ("killed", Seq("insurgents", "in", "fighting")),
      ("in", Seq("insurgents,killed", "killed", "fighting")),
      ("fighting", Seq("killed,in", "in"))
    ),
    "fighting" -> Map(
      ("insurgents", Seq("killed", "killed,in", "killed,ongoing", "in", "in,ongoing", "ongoing")),
      ("killed", Seq("insurgents", "in", "ongoing", "in,ongoing")),
      ("in", Seq("insurgents,killed", "killed", "ongoing")),
      ("ongoing", Seq("killed,in", "in"))
    )
  )

  type Error = String

  def checkCandidate(groundTruth: Map[String, Map[String, Seq[String]]])(
    query: String, answer: String, actual: Set[String]): Option[Error] = {

    groundTruth.get(query) match {

      case None =>
        Some(s"no ground truth for query: $query")

      case Some(answerFeaturesMap) => answerFeaturesMap.get(answer) match {

        case None =>
          Some(s"no ground truth for query: $query and answer: $answer")

        case Some(expectedFeatures) =>
          if (actual != expectedFeatures.toSet)
            Some(s"""For ($query,$answer)\nexpecting: "${expectedFeatures.mkString(";")}"\nactual:    "${actual.mkString(";")}"""")
          else
            None
      }
    }

  }

}