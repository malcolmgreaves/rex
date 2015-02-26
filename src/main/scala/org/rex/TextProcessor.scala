package org.rex

import scala.collection.IndexedSeq

object TextProcessor {

  def create(): TextProcessor =
    new TextProcessor {

      override def processSentence(text: String): Sentence = ???

      override def sentencesOf(text: String): IndexedSeq[String] = ???
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

  def sentencesOf(text: String): IndexedSeq[String]

  def processSentence(text: String): Sentence

}

case class Document(id: String, sentences: IndexedSeq[Sentence])

case class Sentence(words: IndexedSeq[Word])

case class Word(text: String, posTag: String, nerTag: String) {

  override def toString(): String =
    s"$text/$posTag/$nerTag"
}
