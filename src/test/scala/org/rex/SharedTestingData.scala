package org.rex

import org.rex.text.{Document, Sentence}

object SharedTestingData {

  type Error = String

  //
  // "undersized" examples
  //

  val undersizedText = "hello world"
  val undersizedSeq = Seq("hello", "world")

  val undersizedGrams = Set("hello", "world", "hello,world")

  //
  // "insurgents" examples
  //

  val insurgentsText = "Insurgents killed in ongoing fighting."

  val insurgentsSeq = Seq("Insurgents", "killed", "in", "ongoing", "fighting")

  val insurgentsTokens = Seq("Insurgents", "killed", "in", "ongoing", "fighting", ".")

  val insurgentsEntities = Seq("O", "O", "O", "O", "O", "O")

  val insurgentsTags = Seq("NNS", "VBN", "IN", "JJ", "NN", ".")

  val insurgentsUnigrams = Set("Insurgents", "killed", "in", "ongoing", "fighting")

  val insurgentsBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting")

  val insurgentsTrigrams = Set("Insurgents,killed,in", "killed,in,ongoing", "in,ongoing,fighting")

  val insurgents1skipBigrams = Set("Insurgents,killed",
                                   "killed,in",
                                   "in,ongoing",
                                   "ongoing,fighting",
                                   "Insurgents,in",
                                   "killed,ongoing",
                                   "in,fighting")

  val insurgents2skipBigrams = Set("Insurgents,killed",
                                   "killed,in",
                                   "in,ongoing",
                                   "ongoing,fighting",
                                   "Insurgents,in",
                                   "killed,ongoing",
                                   "in,fighting",
                                   "Insurgents,ongoing",
                                   "killed,fighting")

  //
  // "john smith" examples
  //

  val johnSmithText = "John Smith went to China. He visited Beijing, on January 10th, 2013."

  val johnSmithTokens = Seq(
    Seq("John", "Smith", "went", "to", "China", "."),
    Seq("He", "visited", "Beijing", ",", "on", "January", "10th", ",", "2013", ".")
  )

  val johnSmithEntites = Seq(
    Seq("PERSON", "PERSON", "O", "O", "LOCATION", "O"),
    Seq("O", "O", "LOCATION", "O", "O", "DATE", "DATE", "DATE", "DATE", "O")
  )

  val johnSmithTags = Seq(
    Seq("NNP", "NNP", "VBD", "TO", "NNP", "."),
    Seq("PRP", "VBD", "NNP", ",", "IN", "NNP", "JJ", ",", "CD", ".")
  )

  val johnSmithSentences: Seq[Sentence] = johnSmithTokens.indices.map { index =>
    Sentence(tokens = johnSmithTokens(index),
             tags = Some(johnSmithTags(index)),
             entities = Some(johnSmithEntites(index)))
  }

  val johnSmithDoc = Document("john smith sentences", johnSmithSentences)

  val johnSmithChunked = Seq(
    Seq("John Smith", "went", "to", "China", "."),
    Seq("He", "visited", "Beijing", ",", "on", "January 10th, 2013", ".")
  )

}
