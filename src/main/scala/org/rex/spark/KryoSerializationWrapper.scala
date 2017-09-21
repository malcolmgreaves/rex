package org.rex.spark

import scala.reflect.ClassTag

import java.nio.ByteBuffer

import org.apache.spark.serializer.{KryoSerializer => SparkKryoSerializer}
import org.apache.spark.{SparkConf, SparkEnv}

/**
  * Wraps a value of an unserialized type T in a KryoSerializationWrapper[T],
  * which gives one a way to serialize T.
  *
  *
  * NOTE:
  *  The vast majority of this code is copied / based off of the classes with the same
  *  name in the Apache Shark project.
  *
  *  Original file is here:
  *  https://github.com/amplab/shark/blob/master/src/main/scala/shark/execution/serialization/KryoSerializationWrapper.scala
  */
object KryoSerializationWrapper {

  def apply[T: ClassTag](value: T): KryoSerializationWrapper[T] =
    new KryoSerializationWrapper[T](value, true) {}
}

/**
  * A wrapper around some unserializable objects that make them both Java
  * serializable. Internally, Kryo is used for serialization.
  *
  * Use KryoSerializationWrapper(value) to create a wrapper.
  *
  * Note that the value contained in the wrapper is mutable. It must be
  * initialized using Java Serialization (which calls a private readObject
  * method that does the byte-by-byte deserialization).
  *
  * Also note that this class is both abstract and sealed. The only valid place
  * to create such a wrapper is the companion object's apply method.
  */
sealed abstract class KryoSerializationWrapper[T: ClassTag] extends Serializable {
  // initialValue
  // MUST BE TRANSIENT SO THAT IT IS NOT SERIALIZED

  /**
    * The only valid constructor. For safety, do not use the no-arg constructor.
    */
  def this(initialValue: T, serializeImmediately: Boolean = true) = {
    this()
    setValue(initialValue, serializeImmediately)
  }

  // the wrapped value
  // MUST BE TRANSIENT SO THAT IT IS NOT SERIALIZED
  @transient private var value: T = _

  // indicates whether or not doDeserializeValue() has been called
  // since performing setValue(T)
  // MUST BE TRANSIENT SO THAT IT IS NOT SERIALIZED
  @transient private var doneSerialization = false

  // our serialized representation of the wrapped value.
  // MUST NOT BE TRANSIENT AS IT IS THE SERIALIZED VALUE
  private var valueSerialized: Array[Byte] = _

  def doSerializeValue(): Unit = {
    valueSerialized = KryoSerializer.serialize(value)
    doneSerialization = true
  }

  def doDeserializeValue(): Unit =
    value = KryoSerializer.deserialize[T](valueSerialized)

  def getValue: T =
    value

  def setValue(v: T, serializeImmediately: Boolean = true): Unit = {
    value = v
    if (serializeImmediately)
      doSerializeValue()
    else
      doneSerialization = false
  }

  /**
    * Gets the currently serialized value as a Sequence of bytes.
    *
    * If the sequence is empty, then it means that one has not called doSerializeValue().
    * Or the internal value may be null.
    */
  def getValueSerialized: Seq[Byte] =
    if (!doneSerialization || valueSerialized == null) {
      Seq.empty[Byte]
    } else {
      valueSerialized.toSeq
    }

  // Used for Java serialization.
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    if (!doneSerialization) {
      doSerializeValue()
    }
    out.defaultWriteObject()
  }

  // Used for Java Deserialization
  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    doDeserializeValue()
  }
}

/**
  * Java object serialization using Kryo. This is much more efficient, but Kryo
  * sometimes is buggy to use. We use this mainly to serialize the object
  * inspectors.
  *
  *
  * NOTE:
  *  The vast majority of this code is copied / based off of the classes with the same
  *  name in the Apache Shark project.
  *
  *  Original file is here:
  *  https://github.com/amplab/shark/blob/master/src/main/scala/shark/execution/serialization/KryoSerializationWrapper.scala
  */
object KryoSerializer {

  @transient lazy val ser: SparkKryoSerializer = {
    val sparkConf = Option(SparkEnv.get).map(_.conf).getOrElse(new SparkConf())
    new SparkKryoSerializer(sparkConf)
  }

  def serialize[T: ClassTag](o: T): Array[Byte] = {
    ser.newInstance().serialize(o).array()
  }

  def deserialize[T: ClassTag](bytes: Array[Byte]): T = {
    ser.newInstance().deserialize[T](ByteBuffer.wrap(bytes))
  }
}
