package org.rex

trait CandGen {
  def candidates(doc: Document): Seq[Candidate]
}

case class SentenceCandGen(wordFilter: WordFilter) extends CandGen {

  override def candidates(doc: Document): Seq[CandidateSentence] =
    doc.sentences.flatMap(s => {

      val wf = wordFilter(s) _

      val filtWordIndices = s.tokens.zipWithIndex
        .flatMap({
          case (word, index) =>
            if(wf(index))
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

case class CorefCandGen(wf: WordFilter) extends CandGen {

  override def candidates(doc: Document): Seq[CandidateDocument] = {

    // (0) need to have corefresolution thing in scope!

    // apply coref resolution to entire document
    // => iterator of matches

    // for each match in iterator:
    //

    ???
  }

}