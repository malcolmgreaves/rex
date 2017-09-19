organization := "io.malcolmgreaves"
name         := "rex"
version      := "0.0.1"

scalaVersion := "2.11.8"
val jvm = "1.8"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Twitter Repository" at "http://maven.twttr.com/"
)

val breeze  = "0.11.2"
val rapture = "1.1.0"
val nak     = "1.3"
val spire   = "0.10.1"
val spark   = "2.0.2"
val sista   = "5.2"

libraryDependencies ++= Seq(
  // Visualization
  "com.quantifind" %% "wisp" % "0.0.1",
  // NLP
  "edu.arizona.sista" %% "processors" % sista,
  "edu.arizona.sista" %% "processors" % sista classifier "models",
  // Concurrent and Distributed 
  "org.apache.spark" %% "spark-core" % spark,
  // Math
  "org.spire-math" %% "spire" % spire,
  "org.scalanlp" %% "breeze" % breeze,
  "org.scalanlp" %% "breeze-natives" % breeze,
  // ML
  "org.scalanlp" %% "nak" % nak,
  // Util
  "com.github.scopt" %% "scopt" % "3.3.0",
  "com.propensive" %% "rapture-core" % rapture,
  "com.propensive" %% "rapture-json-jackson" % rapture,
  // Testing
  "org.scalatest" %% "scalatest" % "2.2.5" % Test
)

scalacOptions ++= Seq(
  s"-target:jvm-$jvm",
  "-optimize",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:postfixOps",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Yinline-warnings"
)

testOptions         += Tests.Argument(TestFrameworks.JUnit, "-v")
testOptions in Test += Tests.Argument("-oF")

fork in Test              := false
parallelExecution in Test := false
