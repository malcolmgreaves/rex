package org.rex

object CandGen {
  type Fn = Document => Seq[Candidate]
}

/**
 * Creates candidates using tokens within each Sentence only.
 */
case class SentenceCandGen(wordFilter: WordFilter.Fn) extends CandGen.Fn {

  override def apply(doc: Document): Seq[CandidateSentence] =
    doc.sentences.flatMap(s => {

      val wf = wordFilter(s)

      val filtWordIndices = s.tokens.zipWithIndex
        .flatMap({
          case (word, index) =>
            if (wf(index))
              Some(index)
            else
              None
        })

      filtWordIndices.flatMap(queryIndex =>
        filtWordIndices.flatMap(answerIndex =>
          if (queryIndex != answerIndex)
            Some(CandidateSentence(s, queryIndex, answerIndex))
          else
            None
        )
      )
    })
}

/**
 * Generates candidates using co-reference resolution information in the Document.
 */
case class CorefCandGen(mentionFilt: WordFilter.Fn, candFilt: WordFilter.Fn) extends CandGen.Fn {

  private val candidateHelper = CorefCandGen.candidates_h(candFilt) _

  override def apply(doc: Document): Seq[CandidateDocument] =
    doc.corefMentions match {

      case Some(corefMentions) =>

        corefMentions
          .filter(_.mentions.size >= 2)
          .flatMap(coref => {

            val validMentions =
              coref.mentions
                .filter(mention =>
                  (mention.until - mention.from) == 1 && mentionFilt(doc.sentences(mention.sentenceNum))(mention.from)
                )

            if (validMentions.size >= 2)
              (0 until validMentions.size)
                .map(i =>
                  (i + 1 until validMentions.size)
                    .map(j =>
                      (validMentions(i), validMentions(j))
                    )
                )
                .flatten
                .filter({
                  case (m1, m2) => m1.sentenceNum != m2.sentenceNum
                })
                .map({
                  case (m1, m2) =>
                    val f = candidateHelper(doc)
                    f(m2.sentenceNum, m1, m2.from) ++ f(m1.sentenceNum, m2, m1.from)
                })
                .flatten

            else
              Seq.empty[CandidateDocument]

          })

      case None =>
        Seq.empty[CandidateDocument]
    }
}

private object CorefCandGen {

  def candidates_h(wf: WordFilter.Fn)(doc: Document)(sIndex: Int, m: Mention, corefIndex: Int): Seq[CandidateDocument] = {

    val query = WordTarget(m.sentenceNum, m.from)

    val answer = WordTarget(m.sentenceNum, m.from)

    val s = doc.sentences(sIndex)

    (0 until s.tokens.size)
      .filter(index => index != corefIndex && wf(s)(index))
      .map(index =>
        Seq(
          // let's treat index here as something that is an ANSWER
          CandidateCorefQuery(
            doc,
            query,
            sIndex,
            corefIndex,
            index
          ),
          // let's treat index here as something that is a QUERY
          CandidateCorefAnswer(
            doc,
            index,
            sIndex,
            corefIndex,
            answer
          )
        )
      )
      .flatten
  }

}
