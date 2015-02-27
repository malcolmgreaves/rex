package org.rex


import org.scalatest.FunSuite


object TextFeatuerizerTest {

  val insurgentsText = "Insurgents killed in ongoing fighting."
  val insurgentsSeq = Seq("Insurgents", "killed", "in", "ongoing", "fighting")

  val insurgentsUnigrams = Set("Insurgents", "killed", "in", "ongoing", "fighting")
  val insurgentsBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting")
  val insurgentsTrigrams = Set("Insurgents,killed,in", "killed,in,ongoing", "in,ongoing,fighting")

  val insurgents1skipBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting", "Insurgents,in",
    "killed,ongoing", "in,fighting")
  val insurgents2skipBigrams = Set("Insurgents,killed", "killed,in", "in,ongoing", "ongoing,fighting","Insurgents,in",
    "killed,ongoing", "in,fighting","Insurgents,ongoing", "killed,fighting")
}

class TextFeatuerizerTest extends FunSuite {

  import TextFeatuerizerTest._


  test("k-skip n-gram feature generation code (InsideFeatures)") {
    val expectedFeatures = insurgents2skipBigrams ++ insurgentsUnigrams ++ insurgentsBigrams
    val actualFeatures = InsideFeatures(InsideFeatures(2, 2))(insurgentsSeq).toSet

    val diff = expectedFeatures.diff(actualFeatures)
    val intersection = expectedFeatures.intersect(actualFeatures)

    assert(diff.size == 0 && intersection.size == expectedFeatures.size && intersection.size == actualFeatures.size,
      s"""Features did not match\nDifference: ${diff.mkString(" : ")}\nIntersection: ${intersection.mkString(" : ")}"""
    )
  }

}
