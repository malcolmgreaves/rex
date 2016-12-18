package org.rex.app

import java.io.{ File, FileWriter, BufferedWriter }

import org.rex.Sentence

import scala.io.Source
import scala.language.{ existentials, implicitConversions, postfixOps }
import scala.util.Try

object Connl04FormatProcessor extends App {

  val outFi = new File(args(1))
  val grouped = Connl04Format.read(new File(args.head))

  println(s"Writing ${grouped.size} sentences to: $outFi")
  val w = new BufferedWriter(new FileWriter(outFi))
  grouped.foreach { 
    case (sent @ Sentence(tokens,_, _), _) => 
      val s = tokens.mkString(" ")
      w.write(s)
      w.newLine()
  }
  w.close()

}