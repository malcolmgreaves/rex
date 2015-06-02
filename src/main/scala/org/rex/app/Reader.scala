package org.rex.app

import java.io.File

import scala.language.{ existentials, implicitConversions, postfixOps }
import scala.reflect.ClassTag

trait Reader[I, T] {

  type InputSource = I
  type Element = T

  type Readable = Traversable[Element]

  type Fn = InputSource => Readable
}

object ReaderMap {

  def apply[I: ClassTag, T: ClassTag](s: String): Option[Reader[I, T]#Fn] = {

    val inputClz = implicitly[ClassTag[I]].runtimeClass
    val outputClz = implicitly[ClassTag[T]].runtimeClass

    s.trim.toLowerCase match {
      case "conll" =>

        if (inputClz.equals(classOf[File]) && outputClz.equals(classOf[Connl04Format.LabeledSentence]))
          Some(Connl04Format.read.asInstanceOf[Reader[I, T]#Fn])
        else
          throw new RuntimeException(s"Expecting an input for a Reader Fn of type ")

      case _ =>
        None
    }
  }

}