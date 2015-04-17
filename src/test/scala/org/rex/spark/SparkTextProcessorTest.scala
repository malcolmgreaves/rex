package org.rex.spark

import org.rex.TextProcessorTest

import SparkModules._

class SparkTextProcessorTest extends SparkTestSuite {

  import TextProcessorTest._

  sparkTest("Spark Text Processing") {

    val data = sc.parallelize(Seq(("JohnSmithSentence", johnSmithText)))

    val processedRdd = SparkTextProcessor(KryoSerializationWrapper(makeProcessor()))(data)

    assert(processedRdd.count == data.count)

    testDocument(johnSmithDoc, processedRdd.collect().head)
  }

}