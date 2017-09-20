package org.rex.io

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}

import org.rex.text.Sentence

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.{existentials, implicitConversions, postfixOps}

object UiucRelationFmt {

  //
  // Line :: types that represent each possible line in the format
  //

  /** The Line type represents each possible shape of a line of text in the format. */
  sealed trait Line

  /** Represents a blank line. */
  case object Break extends Line

  /** A line that contains token information, including results from text processing. */
  case class TokenLine(neTag: String, tokenIndex: Int, posTag: String, word: String) extends Line

  /** A line that contains the labeled relation that is present in the preceeding sentence. */
  case class RelationLine(arg1TokenIndex: Int, arg2TokenIndex: Int, relation: String) extends Line

  /** A labeled example: a sentence along with all applicable, positive relations. */
  type LabeledSentence = (Sentence, Seq[RelationLine])

  /** Function that produces labeled sentences from a plaintext file. */
  lazy val read_file: Reader[File, LabeledSentence]#Fn =
    inFi => {
      println(s"Reading input from:\n$inFi")
      val stream = new BufferedInputStream(new FileInputStream(inFi))
      try {
        read_stream(stream)
      } finally {
        stream.close()
      }
    }

  /** Function that produces labeled sentences from a plaintext stream. */
  lazy val read_stream: Reader[InputStream, LabeledSentence]#Fn =
    stream => {
      val input = Source.fromInputStream(stream)

      val rawLines = input.getLines().map(parseLine).toIndexedSeq
      println(s"Obtained ${rawLines.size} individual lines")

      val groupedIntoLabeledSentences = labeledSentencesFromLines(rawLines)
      println(s"Obtained ${groupedIntoLabeledSentences.size} labeled sentences")

      groupedIntoLabeledSentences
    }

  /** Given a line of text, parses it and converts it into the appropriate Line type.
    *
    * If there is an error in processing the input string, then the exception will be propigated
    * to the caller.
    * Importantly, the underlying format assumes that lines are either:
    * - empty
    * - have 9 tab-separated values (a TokenLine)
    * - have 3 tab-separated values (a RelationLine)
    * If one of the above 3 conditions does _not_ apply, an IllegalArgumentException is thrown.
    * */
  def parseLine(line: String): Line =
    if (line.isEmpty)
      Break
    else {
      val bits = line.split("\t")
      bits.length match {

        case 9 =>
          TokenLine(
            neTag = bits(1),
            tokenIndex = bits(2).toInt,
            posTag = bits(4),
            word = bits(5)
          )

        case 3 =>
          RelationLine(
            arg1TokenIndex = bits(0).toInt,
            arg2TokenIndex = bits(1).toInt,
            relation = bits(2)
          )

        case unknownFieldNumber =>
          throw new IllegalArgumentException(
            s"Expecting either 3 or 9 tab-separated parts, " +
              s"not $unknownFieldNumber.\n" +
              s"""Offending line: "$line"""")
      }
    }

  def labeledSentencesFromLines(lines: Traversable[Line]): Traversable[LabeledSentence] = {

    class MutableAccumulator(val tokenLines: ArrayBuffer[TokenLine] = ArrayBuffer.empty,
                             val relationLines: ArrayBuffer[RelationLine] = ArrayBuffer.empty,
                             val finished: ArrayBuffer[LabeledSentence] = ArrayBuffer.empty,
                             var seenOneBreak: Boolean = false)

    val result = lines.foldLeft(new MutableAccumulator()) {
      case (mutableAccum, line) =>
        line match {

          case Break =>
            if (mutableAccum.seenOneBreak) {
              // we've already seen one break before, this means we're AFTER the relation line
              // therefore we've finished the sentence
              val sentence = sentenceFrom(mutableAccum.tokenLines)
              val relationsOf = mutableAccum.relationLines.toIndexedSeq
              mutableAccum.finished.append((sentence, relationsOf))
              // reset state
              mutableAccum.tokenLines.clear()
              mutableAccum.relationLines.clear()
              mutableAccum.seenOneBreak = false

            } else {
              // first time seeing a break for the current sentence & relation label processing,
              // this means that we're in-between the sentence's token lines and the relation lines
              mutableAccum.seenOneBreak = true
            }

          case tl: TokenLine =>
            mutableAccum.tokenLines.append(tl)

          case rl: RelationLine =>
            mutableAccum.relationLines.append(rl)
        }
        mutableAccum
    }

    if (result.tokenLines.nonEmpty && result.relationLines.nonEmpty) {
      // get the last one, if there's still something left in the buffer
      val sentence = sentenceFrom(result.tokenLines)
      val relationsOf = result.relationLines.toIndexedSeq
      result.finished.append((sentence, relationsOf))
    }

    result.finished.toIndexedSeq
  }

  def sentenceFrom(tls: Seq[TokenLine]): Sentence = {

    val (tokens, tags, entities) =
      tls.foldLeft((Seq.empty[String], Seq.empty[String], Seq.empty[String])) {
        case ((tks, tgs, ents), tokenLine) =>
          (tks :+ cleanWord(tokenLine.word), tgs :+ tokenLine.posTag, ents :+ tokenLine.neTag)
      }
    Sentence(tokens = tokens, tags = Some(tags), entities = Some(entities))
  }

  def cleanWord(word: String): String = {
    word
      .replaceAll("/,/", ", ")
      .replaceAll("/", " ")
  }

}
