package org.rex

trait CandGen {
  def candidates(doc: Document): Seq[Candidate]
}

case class SentenceCandGen(wf: WordFilter) extends CandGen {

  override def candidates(doc: Document): Seq[CandidateSentence] =
    doc.sentences.flatMap(s => {

      val filtWordIndices = s.words.zipWithIndex.filter({ case (word, _) => wf(word) })

      (0 until filtWordIndices.size).flatMap(queryIndex =>
        (0 until filtWordIndices.size).flatMap(answerIndex =>
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