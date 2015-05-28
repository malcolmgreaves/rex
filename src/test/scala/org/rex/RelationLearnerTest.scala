package org.rex

import java.time.Duration

import nak.liblinear.LiblinearConfig
import org.scalatest.FunSuite

class RelationLearnerTest extends FunSuite {

  import RelationLearnerTest._
  import TextFeatuerizerTest._

  test("basic relation learning test") {

    val candidatePipeln = Pipeline(
      TextProcessorTest.makeProcessor(NamedEntitySet.Default4Class.entSet),
      NerDocChunker(NamedEntitySet.Default4Class.entSet),
      CorefCandGen(WordFilter.noKnownPunct, CandGenTest.candidateFilter)
    )

    val start = System.currentTimeMillis()
    val candidates = candidatePipeln("bernie", bernieText)
    val end = System.currentTimeMillis()
    println(s"Took ${Duration.ofMillis(end - start).getSeconds} seconds (${end - start} ms) to generate ${candidates.size} candidates from ${bernieSentences.size} sentences.")

    val rlearner = RelationLearner(
      LiblinearConfig(),
      featuerizer4skip2gram2gram
    )

    candidates.foreach(c => {
      println(s"${c.queryW}\t${c.answerW}")
    })

  }

}

object RelationLearnerTest {

  val bernieSentences = Seq(
    "Bernard Sanders is an American politician.",
    "He was born on September 8, 1941.",
    "He also goes by Bernie.",
    "The junior United States Senator from Vermont, Sanders has been a U.S. Senator since 2007.",
    "An independent politician since 1979, Sanders is associated with the Vermont Progressive Party and was a member of the Liberty Union Party from 1971 to 1979.",
    "Sanders announced his intentions to seek the Democratic Party's nomination for President on April 30, 2015, in an address on the Capitol lawn."
  )

  val bernieText = bernieSentences.mkString(" ")

}

