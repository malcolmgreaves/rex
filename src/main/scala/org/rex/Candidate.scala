package org.rex

sealed trait Candidate {

  def innerFromSentence:Sentence

  def startInnerIndex:Int

  def endInnerIndex:Int

  def inner: Seq[String] =
    innerFromSentence.tokens.slice(startInnerIndex, endInnerIndex)

  def queryW: String

  def answerW: String
}

case class CandidateSentence(s: Sentence, queryIndex: Int, answerIndex: Int) extends Candidate {

  override lazy val innerFromSentence =
    s

  private val (start, end) =
    if (queryIndex < answerIndex)
      (queryIndex, answerIndex)
    else
      (answerIndex, queryIndex)

  override val startInnerIndex = start
  override val endInnerIndex = end

  override lazy val queryW =
    s.tokens(queryIndex)

  override lazy val answerW =
    s.tokens(answerIndex)
}

sealed trait CandidateDocument extends Candidate {

  def doc: Document

  def sharedSentNum: Int

  override final lazy val innerFromSentence =
    doc.sentences(sharedSentNum)

  @inline protected def inner_h(start: Int, finish: Int): Seq[String] =
    doc.sentences(sharedSentNum).tokens.slice(start, finish)

  @inline protected def word_h(sentNum: Int, index: Int): String =
    doc.sentences(sentNum).tokens(index)

  @inline protected def word_h(t: WordTarget): String =
    doc.sentences(t.sentNum).tokens(t.wordIndex)
}

case class WordTarget(sentNum: Int, wordIndex: Int)

case class CandidateCorefQuery(doc: Document,
                               query: WordTarget,
                               sharedSentNum: Int,
                               queryCorefWordIndex: Int,
                               answerWordIndex: Int) extends CandidateDocument {

  private val (start, end) =
    if (queryCorefWordIndex < answerWordIndex)
      (queryCorefWordIndex, answerWordIndex)
    else
      (answerWordIndex, queryCorefWordIndex)

  override val startInnerIndex = start
  override val endInnerIndex = end

  override lazy val queryW =
    word_h(query)

  override lazy val answerW =
    word_h(sharedSentNum, answerWordIndex)
}

case class CandidateCorefAnswer(doc: Document,
                                queryWordIndex: Int,
                                sharedSentNum: Int,
                                answerCorefWordIndex: Int,
                                answer: WordTarget) extends CandidateDocument {

  private val (start, end) =
    if (queryWordIndex < answerCorefWordIndex)
      (queryWordIndex, answerCorefWordIndex)
    else
      (answerCorefWordIndex, queryWordIndex)

  override val startInnerIndex = start
  override val endInnerIndex = end

  override lazy val queryW =
    word_h(sharedSentNum, queryWordIndex)

  override lazy val answerW =
    word_h(answer)
}
