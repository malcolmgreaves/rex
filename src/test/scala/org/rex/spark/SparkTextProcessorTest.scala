package org.rex.spark

import SparkModules._
import org.rex.SharedTestingData._
import org.rex.text.TextProcessorTest

class SparkTextProcessorTest extends SparkTestSuite {

  sparkTest("Spark Text Processing") {

    val data = sc.parallelize(Seq(("JohnSmithSentence", johnSmithText)))

    val processedRdd = SparkTextProcessor(KryoSerializationWrapper(TextProcessorTest))(data)

    assert(processedRdd.count == data.count)

    TextProcessorTest.testDocument(johnSmithDoc, processedRdd.collect().head)
  }

}
