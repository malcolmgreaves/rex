package org.rex

import java.time.Duration

import nak.liblinear.LiblinearConfig
import org.scalatest.FunSuite

import scala.util.Try

class RelationLearnerTest extends FunSuite {

  import TextFeatuerizerTest._
  import RelationLearnerTest._

  test("basic relation learning test: memorization") {

    val rlearner = RelationLearner(
      LiblinearConfig(),
      CandidateFeatuerizer(
        Some((adjacentConf2gram, sentenceViewFilter)),
        Some((insideConf4skip2gram, relWordFilter, wordView))
      )
    )

    val (classifier, _) = rlearner(stupidTrainingData)

    checkInstanceLabelClassifiations(classifier, stupidTrainingData)(None)
  }

  ignore("relation extraction & learning pipeline") {
    val textProcessor = TextProcessorTest

    { // so that we can force the loading of the CoreNLP stuff
      textProcessor.process("_", "Hello world.")
    }

    val candidatePipeln = Pipeline(
      textProcessor,
      NerDocChunker(NeTagSet.Default4Class.entSet),
      CorefCandGen(notPunct, notPunct)
    )

    val startC = System.currentTimeMillis()
    val candidates = candidatePipeln("bernie", bernieText)
    val endC = System.currentTimeMillis()
    println(s"Took ${Duration.ofMillis(endC - startC).getSeconds} seconds (${endC - startC} ms) to generate ${candidates.size} candidates from ${bernieSentences.size} sentences.")

    def clean(s: String) = s.replaceAll(" ", "")

    val candLabelMap =
      stupidTrainingData
        .filter(_._2 != nothingLabel)
        .flatMap { case (c, r) => List(((c.queryW, c.answerW), r), ((c.answerW, c.queryW), r)) }
        .map { case ((q, a), r) => ((clean(q), clean(a)), r) }
        .toMap[(String, String), String]

    val trainingData =
      candidates
        .map(cand => {
          val qa = (clean(cand.queryW), clean(cand.answerW))

          println(s"${cand.queryW} | ${cand.answerW}")

          if (candLabelMap.contains(qa))
            (cand, candLabelMap(qa))
          else
            (cand, nothingLabel)
        })

    trainingData.filter(_._2 != nothingLabel).foreach {
      case (c, r) => println(s"(${c.queryW}|${c.answerW}) :: $r")
    }
    println(s"# nothings: ${trainingData.count(_._2 == nothingLabel)}")

    val tf = CandidateFeatuerizer(
      Some((adjacentConf2gram, sentenceViewFilter)),
      Some((insideConf4skip2gram, notPunct, WordView.lowercase))
    )

    val rlearner = RelationLearner(
      LiblinearConfig(cost = 50.0),
      tf
    )

    val (classifier, _) = rlearner(trainingData)

    checkInstanceLabelClassifiations(classifier, trainingData)(Some(tf))
  }

  def checkInstanceLabelClassifiations(
    classifier: Learning[Candidate, String]#Classifier,
    d: Learning[Candidate, String]#TrainingData)(tf: Option[CandidateFeatuerizer.Fn]): Unit = {

    val errors =
      d
        .foldLeft(Seq.empty[String]) {
          case (errs, (instance, label)) =>
            val clazz = classifier(instance)
            if (clazz == label)
              errs
            else {
              val features = tf.map(f => f(instance))
              errs :+ s"classifying (${instance.queryW} , ${instance.answerW}) as $clazz actually a $label\n$features"
            }
        }

    if (errors.nonEmpty)
      fail(s"""Errors in simple classification:\n${errors.mkString("\n")}\n""")
  }

}

object RelationLearnerTest {

  val ts = TextProcessorTest.conf.tagSet.get

  val noun =
    (w: String) => ts.nouns.contains(w)

  val pronoun =
    (w: String) => ts.pronouns.contains(w)

  val nounOrPronoun =
    (w: String) => noun(w) || pronoun(w)

  val relWordFilter: WordFilter.Fn =
    (s: Sentence) => (i: Int) => Try(nounOrPronoun(s.tags.get(i))).toOption.getOrElse(false)

  val notPunct: WordFilter.Fn =
    (s: Sentence) => (i: Int) => Try(!ts.punctuation(s.tags.get(i))).toOption.getOrElse(false)

  val sentenceViewFilter: SentenceViewFilter.Fn =
    SentenceViewFilter(
      WordView.lowercase,
      notPunct
    )

  val bernieText = "Sanders is an American politician. He was born on September 8, 1941. An independent politician since 1979, Sanders is associated with the Vermont Progressive Party and was a member of the Liberty Union Party from 1971 to 1979."

  val bernieSentences =
    Seq(
      Sentence(Seq("Sanders", "is", "an", "American", "politician", ".")),
      Sentence(Seq("He", "was", "born", "on", "September 8, 1941", ".")),
      Sentence(Seq("An", "independent", "politician", "since", "1979", ",", "Sanders", "is", "associated", "with", "the", "Vermont", "Progressive", "Party", "and", "was", "a", "member", "of", "the", "Liberty Union Party", "from", "1971", "to", "1979", "."))
    )

  val bernieDoc = Document("bernie", bernieSentences)

  val nothingLabel = "nothing"
  val bornInLabel = "born_in"

  val stupidTrainingData = Seq(
    (CandidateSentence(bernieSentences(0), 0, 4), nothingLabel),
    (CandidateSentence(bernieSentences(1), 0, 4), bornInLabel),
    (CandidateSentence(bernieSentences(2), 1, 4), nothingLabel),
    (
      CandidateCorefQuery(
        doc = bernieDoc,
        query = WordTarget(0, 0),
        sharedSentNum = 1,
        queryCorefWordIndex = 0,
        answerWordIndex = 4
      ),
        bornInLabel
    )
  )

}

