package org.rex.app

import java.io.{ File, FileWriter, BufferedWriter }

import org.rex.Sentence

import scala.io.Source
import scala.language.{ existentials, implicitConversions, postfixOps }
import scala.util.Try

object ConnlFormatProcessor extends App {

  // 1       The     the     DT      O       _       _

  def onlyLowerWord(conllLine: String): String = 
    conllLine.trim().split("\t")(2)

  def group(
    lineParser: String => String,
    lines: Iterable[String]
  ): Iterable[Seq[String]] = {

    val (global, last) = lines
      .foldLeft((Seq.empty[Seq[String]], Seq.empty[String])) { 
        case ((globalAccum, current), line) =>
          if(line.trim().isEmpty)
            (globalAccum :+ current, Seq.empty[String])
          else
            (globalAccum, current :+ lineParser(line))
      }

    if(last.nonEmpty)
      global :+ last
    else
      global
  }

  val inFi = new File(args(0))
  val outFi = new File(args(1))
  println(s"Reading CONLL format:             $inFi")
  println(s"Writing sentences, \\n separated: $outFi")

  val asSentences = 
    group(
      onlyLowerWord _,
      Source
        .fromFile(outFi)
        .getLines()
        .toIterable
    )

  val w = new BufferedWriter(new FileWriter(outFi))
  asSentences.foreach { tokens => 
      val s = tokens.mkString(" ")
      w.write(s)
      w.newLine()
  }
  w.close()

}