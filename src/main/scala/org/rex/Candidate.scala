package org.rex

sealed trait Candidate {

  def inner: Seq[Word]

  def queryW: Word

  def answerW: Word
}

case class CandidateSentence(s: Sentence, queryIndex: Int, answerIndex: Int) extends Candidate {

  override lazy val inner =
    if (queryIndex < answerIndex)
      s.words.slice(queryIndex, answerIndex)
    else
      s.words.slice(answerIndex, queryIndex)

  override lazy val queryW =
    s.words(queryIndex)

  override lazy val answerW =
    s.words(answerIndex)
}

sealed trait CandidateDocument extends Candidate {

  def doc: Document

  def sharedSentNum: Int

  @inline protected def inner_h(start: Int, finish: Int): Seq[Word] =
    doc.sentences(sharedSentNum).words.slice(start, finish)

  @inline protected def word_h(sentNum: Int, index: Int): Word =
    doc.sentences(sentNum).words(index)

  @inline protected def word_h(t: WordTarget): Word =
    doc.sentences(t.sentNum).words(t.wordIndex)
}

case class WordTarget(sentNum: Int, wordIndex: Int)

case class CandidateCorefQuery(
    doc: Document,
    query: WordTarget,
    sharedSentNum: Int,
    queryCorefWordIndex: Int,
    answerWordIndex: Int) extends CandidateDocument {

  override lazy val inner =
    if (queryCorefWordIndex < answerWordIndex)
      inner_h(queryCorefWordIndex, answerWordIndex)
    else
      inner_h(answerWordIndex, queryCorefWordIndex)

  override lazy val queryW =
    word_h(query)

  override lazy val answerW =
    word_h(sharedSentNum, answerWordIndex)
}

case class CandidateCorefAnswer(
    doc: Document,
    queryWordIndex: Int,
    sharedSentNum: Int,
    answerCorefWordIndex: Int,
    answer: WordTarget) extends CandidateDocument {

  override lazy val inner =
    if (queryWordIndex < answerCorefWordIndex)
      inner_h(queryWordIndex, answerCorefWordIndex)
    else
      inner_h(answerCorefWordIndex, queryWordIndex)

  override lazy val queryW =
    word_h(sharedSentNum, queryWordIndex)

  override lazy val answerW =
    word_h(answer)
}

case class CandidateCorefBoth(
    doc: Document,
    query: WordTarget,
    queryCorefWordIndex: Int,
    sharedSentNum: Int,
    answerCorefWordIndex: Int,
    answer: WordTarget) extends CandidateDocument {

  override lazy val inner =
    if (queryCorefWordIndex < answerCorefWordIndex)
      inner_h(queryCorefWordIndex, answerCorefWordIndex)
    else
      inner_h(answerCorefWordIndex, queryCorefWordIndex)

  override lazy val queryW =
    word_h(sharedSentNum, queryCorefWordIndex)

  override lazy val answerW =
    word_h(sharedSentNum, answerCorefWordIndex)

}