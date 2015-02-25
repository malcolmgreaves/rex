package org.rex

import scala.collection.IndexedSeq

object TextProcessor {

	

}

trait TextProcessor {

	def apply(text:String):Document

}


case class Document(id:String, sentences:IndexedSeq[Sentence])

case class Sentence(words:IndexedSeq[Word])

case class Word(text:String, posTag:Option[String], nerTag:Option[String])