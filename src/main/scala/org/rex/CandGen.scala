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

case class CorefCandGen(wf: WordFilter)(implicit entSet: NamedEntitySet) extends CandGen {

  override def candidates(doc: Document): Seq[CandidateDocument] =
    doc.corefMentions match {

      case Some(corefMentions) =>

        corefMentions
          .filter(_.mentions.size >= 2)
          .flatMap(coref =>
            (0 until coref.mentions.size-1)
              .map(i => (i, i + 1))
              .flatMap({
                case (mi, mj) =>
                  val original = coref.mentions(mi)
                  val referent = coref.mentions(mj)

                  if (original.sentenceNum != referent.sentenceNum)
                    Some(
                      doc.sentences(referent.sentenceNum)
                        .tokens.zipWithIndex
                        .map(_._2)
                        .flatMap(index =>
                          if (index < referent.from || index >= referent.until)
                            Seq(
                              CandidateCorefQuery(
                                doc,
                                WordTarget(original.sentenceNum, original.from),
                                referent.sentenceNum,
                                referent.from,
                                index
                              ),
                              CandidateCorefAnswer(
                                doc,
                                index,
                                referent.sentenceNum,
                                referent.from,
                                WordTarget(original.sentenceNum, original.from)
                              )
                            )
                          else
                            Seq.empty[CandidateDocument]
                        )
                    )
                  else
                    None
            })
        ).flatten

      case None =>
        Seq.empty[CandidateDocument]
    }
}