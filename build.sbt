name := "rex"

version := "0.1"

organization := "mwg"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Twitter Repository" at "http://maven.twttr.com/"
)

libraryDependencies ++= Seq(
  // JSON
  "com.propensive" %% "rapture-core" % "1.0.0",
  "com.propensive" %% "rapture-json-jackson" % "1.0.8",
  "com.propensive" %% "rapture-uri" % "1.0.0",
  "com.propensive" %% "rapture-codec" % "1.0.0",
  "com.propensive" %% "rapture-net" % "0.10.0",
  // NLP
  "edu.arizona.sista" % "processors" % "3.3",
  "edu.arizona.sista" % "processors" % "3.3" classifier "models",
  // "org.scalanlp" % "chalk" % "1.2.0",
  // Concurrent 
  "org.apache.spark" %% "spark-core" % "1.2.0",
  // ML
  "org.apache.spark" %% "spark-mllib" % "1.2.0",
  "org.scalanlp" % "breeze-core_2.10" % "0.4",
  "org.scalanlp" % "breeze-math_2.10" % "0.4",
  "org.scalanlp" % "nak" % "1.1.3",
  // Testing
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

testOptions in Test += Tests.Argument("-oF")

instrumentSettings

CoverallsPlugin.coverallsSettings

packAutoSettings

fork := false

