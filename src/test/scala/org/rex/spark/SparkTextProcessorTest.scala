package org.rex.spark

import org.rex.{ TextProcessingUtil, TextProcessorTest }

class SparkTextProcessorTest extends SparkTestSuite {

  import TextProcessorTest._

  sparkTest("Spark Text Processing") {

    val data = sc.parallelize(Seq(("JohnSmithSentence", johnSmithText)))

    val sparkTextProcessor = SparkTextProcessor(KryoSerializationWrapper(TextProcessingUtil.make())) _

    val processedRdd = data.map(sparkTextProcessor)

    assert(processedRdd.count == data.count)

    testDocument(johnSmithDoc, processedRdd.collect().head)
  }

}