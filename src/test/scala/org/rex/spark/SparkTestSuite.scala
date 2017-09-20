package org.rex.spark

import java.io.File

import org.apache.log4j.helpers.LogLog
import org.apache.log4j.{LogManager, Logger, Level}
import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.FunSuite

object SparkTest extends org.scalatest.Tag("com.rex.spark.SparkTest")

trait SparkTestSuite extends FunSuite {

  var sc: SparkContext = _

  /**
    * convenience method for tests that use spark.  Creates a local spark context, and cleans
    * it up even if your test fails.  Also marks the test with the tag SparkTest, so you can
    * turn it off
    *
    * By default, it turn off spark logging, b/c it just clutters up the test output.  However,
    * when you are actively debugging one test, you may want to turn the logs on
    *
    * @param name the name of the test
    * @param silenceSpark true to turn off spark logging
    */
  def sparkTest(name: String, silenceSpark: Boolean = true)(body: => Unit) =
    test(name, SparkTest) {

      SparkUtil.silenceLogging()

      sc = new SparkContext(
        new SparkConf()
          .setMaster("local[2]")
          .setAppName(name)
          .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
      )

      try {

        body

      } finally {
        sc.stop()
        sc = null
        // To avoid Akka rebinding to the vim bu  ame port, since it doesn't unbind immediately on shutdown
        System.clearProperty("spark.master.port")
        System.gc()
      }
    }

  def ignoreSparkTest(name: String, logIgnored: Boolean = true)(bodyIgnored: => Unit) =
    ignore(name, SparkTest) { /* test is ignored, so doesn't matter what we do! */ }

}

object SparkUtil {

  lazy val logFileCompletePath =
    Seq("src", "main", "resources", "log4j.properties")
      .foldLeft(new File("."))({ case (fi, part) => new File(fi, part) })
      .getCanonicalPath

  def silenceLogging(): Unit = {
    org.slf4j.LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[ch.qos.logback.classic.Logger]
      .setLevel(ch.qos.logback.classic.Level.WARN)
  }

}
