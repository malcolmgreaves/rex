package org.rex

object CandGen {

  def create(): CandGen = ???

}

trait CandGen {

  def candidates(doc: Document): Seq[Candidate]
}
