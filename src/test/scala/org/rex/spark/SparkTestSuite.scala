package org.rex.spark

import org.apache.log4j.{ Logger, Level }
import org.apache.spark.{ SparkConf, SparkContext }
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
      val origLogLevels = Option(if (silenceSpark) SparkUtil.silenceSpark() else null)

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
        // To avoid Akka rebinding to the same port, since it doesn't unbind immediately on shutdown
        System.clearProperty("spark.master.port")
        origLogLevels.foreach(SparkUtil.setLogLevels)
        System.gc()
      }
    }

  def ignoreSparkTest(name: String, ignored: Boolean = true)(body: => Unit) =
    ignore(name, SparkTest) {
      try { body }
    }

}

object SparkUtil {

  def silenceSpark(): Map[String, Level] =
    setLogLevels(Level.WARN, Seq("spark", "org.eclipse.jetty", "akka"))

  def setLogLevels(level: org.apache.log4j.Level, loggers: TraversableOnce[String]): Map[String, Level] =
    loggers.map {
      loggerName =>
        val logger = Logger.getLogger(loggerName)
        println(s"logger for $loggerName: $logger")
        val prevLevel = logger.getLevel
        logger.setLevel(level)
        loggerName -> prevLevel
    }.toMap

  def setLogLevels(loggerAndLevel: Map[String, Level]): Unit =
    loggerAndLevel.foreach({
      case (loggerName, level) => Logger.getLogger(loggerName).setLevel(level)
    })

}