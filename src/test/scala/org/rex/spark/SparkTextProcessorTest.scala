package org.rex.spark

import SparkModules._
import org.rex.text.TextProcessorTest

class SparkTextProcessorTest extends SparkTestSuite {

  import TextProcessorTest._

  sparkTest("Spark Text Processing") {

    val data = sc.parallelize(Seq(("JohnSmithSentence", johnSmithText)))

    val processedRdd = SparkTextProcessor(KryoSerializationWrapper(TextProcessorTest))(data)

    assert(processedRdd.count == data.count)

    testDocument(johnSmithDoc, processedRdd.collect().head)
  }

}