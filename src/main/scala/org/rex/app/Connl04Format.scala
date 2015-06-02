package org.rex.app

import java.io.File

import org.rex.Sentence

import scala.io.Source
import scala.language.{ existentials, implicitConversions, postfixOps }
import scala.util.Try

object Connl04Format {

  //
  // Line :: types that represent each possible line in the format
  //

  sealed trait Line

  case object Break extends Line

  case class TokenLine(sentence: Int, neTag: String, token: Int, posTag: String, word: String) extends Line

  case class RelationLine(arg1: Int, arg2: Int, relation: String) extends Line

  object Line {
    def apply(s: String): Option[Line] = {
      val bits = s.split("\t")
      Try {
        bits.length match {

          case 9 =>
            TokenLine(
              bits(0).toInt,
              bits(1).toUpperCase,
              bits(2).toInt,
              bits(4),
              bits(5)
            )

          case 3 =>
            RelationLine(
              bits(0).toInt,
              bits(1).toInt,
              bits(2)
            )

          case 1 if s == "" =>
            Break
        }
      } toOption
    }
  }

  //
  // ConnlReader :: Implementation for obtaining labeled sentences from a File in the Connl 2004 format.
  //

  type LabeledSentence = (Sentence, Seq[RelationLine])

  val read: Reader[File, LabeledSentence]#Fn =
    (inFi: File) => {
      println(s"Reading input from:\n$inFi")
      val input = Source.fromFile(inFi)

      val rawLines = input.getLines().flatMap(Line.apply).toSeq
      println(s"Obtained ${rawLines.size} individual lines")

      val groupedLines = lineAggregate(rawLines)
      println(s"Obtained ${groupedLines.size} grouped lines")

      groupedLines
        .map(x => x.copy(_1 = sentenceFrom(x._1)))
        .toIterator
    }

  def cleanWord(word: String): String =
    word.replaceAll("/", " ")

  type LabeledLines = (Seq[TokenLine], Seq[RelationLine])

  def lineAggregate(lines: Traversable[Line]): Seq[LabeledLines] =
    lines
      .foldLeft((Seq.empty[LabeledLines], Seq.empty[TokenLine], Seq.empty[RelationLine], false)) {
        case ((labeled, workingToks, workingRels, brokenBefore), l) => l match {

          case Break =>
            if (!brokenBefore)
              (labeled, workingToks, workingRels, true)
            else
              (
                labeled :+ ((workingToks, workingRels)),
                Seq.empty[TokenLine],
                Seq.empty[RelationLine],
                false
              )

          case tl: TokenLine =>
            (labeled, workingToks :+ tl, workingRels, brokenBefore)

          case rl: RelationLine =>
            (labeled, workingToks, workingRels :+ rl, brokenBefore)
        }
      }._1

  def sentenceFrom(tls: Seq[TokenLine]): Sentence =
    Sentence(
      tls.map(_.word).map(cleanWord),
      Some(tls.map(_.posTag)),
      Some(tls.map(_.neTag))
    )

}

