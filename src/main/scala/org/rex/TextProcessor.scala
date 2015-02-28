package org.rex

import scala.collection.IndexedSeq

object TextProcessor {

  def create(): TextProcessor =
    new TextProcessor {

      override def processSentence(text: String): Sentence = ???

      override def sentencesOf(text: String): Seq[String] = ???
    }

}

trait TextProcessor {

  final def process(text: String): Document =
    process(None, text)

  final def process(id: Option[String], text: String): Document = {
    val docId = id.getOrElse("")
    val segmentedSentences = sentencesOf(text)
    val processedSentences = segmentedSentences.map(processSentence)
    Document(docId, processedSentences)
  }

  def sentencesOf(text: String): Seq[String]

  def processSentence(text: String): Sentence

}


trait Document {
  def id:String
  def sentences:Seq[Sentence]
}

object Document {

  def apply(docId: String, docSentences: Seq[Sentence]): Document =
    new Document {
      override val id = docId
      override val sentences = docSentences
    }

}


trait Sentence {
  def words:Seq[Word]
}

object Sentence {

  def apply(sentWords:Seq[Word]):Sentence =
    new Sentence {
      val words = sentWords
    }

}


trait Word {
  def text:String
  def posTag:String
  def nerTag:String
}

object Word {

  def apply(wText:String,wPosTag:String="", wNerTag:String=""):Word =
    new Word {
      override val text = wText
      override val posTag = wPosTag
      override val nerTag = wNerTag
    }

}
