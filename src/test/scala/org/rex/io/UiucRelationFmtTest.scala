package org.rex.io

import java.io.{ByteArrayInputStream, File, InputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import org.rex.io.UiucRelationFmt.{LabeledSentence, RelationLine}
import org.rex.text.textOf
import org.scalatest.FunSuite

class UiucRelationFmtTest extends FunSuite {

  import UiucRelationFmtTest._

  def helperTestAgainstExample(labeledSentences: Seq[LabeledSentence],
                               expectedResults: Seq[(String, Int, Seq[RelationLine])]): Unit = {

    assert(labeledSentences.size == expectedResults.size)
    labeledSentences.zip(expectedResults).foreach {
      case ((aSentence, aRelations), (eSentenceText, eTokenLen, eRelations)) =>
        assert(aSentence.tokens.size === eTokenLen)
        assert(textOf(aSentence) === eSentenceText)

        assert(aRelations.size == eRelations.size)
        aRelations.zip(eRelations).foreach {
          case (aRelationLine, eRelationLine) =>
            assert(aRelationLine === eRelationLine)
        }
    }
  }

  test("Handles specified format correclty") {
    helperTestAgainstExample(
      labeledSentences = UiucRelationFmt.read_stream(stringAsStream(exampleFormattedData)).toSeq,
      expectedResults = expectedTextRelationInfo
    )
  }

  test("Fails on invalid format data") {
    try {
      UiucRelationFmt.read_stream(stringAsStream("invalid format\nshould\tnot\twork"))
      fail("Supposed to throw exception!")
    } catch {
      case e: Exception =>
        assert(true)
    }
  }

  test("Does nothing on empty input") {
    assert(
      UiucRelationFmt.read_stream(stringAsStream("")).isEmpty,
      "Expecting empty list of labeled sentences from an input empty string"
    )
  }

  test("Handles files") {
    val tempFiWithExample = writeToTempFile(exampleFormattedData)

    helperTestAgainstExample(
      labeledSentences = UiucRelationFmt.read_file(tempFiWithExample).toSeq,
      expectedResults = expectedTextRelationInfo
    )

  }
}

object UiucRelationFmtTest {

  /** Creates a temporary file, writes the input string to the file, and the file handle.
    *
    * NOTE: This funciton uses the createTempFile function from the File class. The prefix and
    * suffix must be at least 3 characters long, otherwise this function throws an
    * IllegalArgumentException.
    */
  def writeToTempFile(contents: String,
                      prefix: Option[String] = None,
                      suffix: Option[String] = None): File = {
    val tempFi = File.createTempFile(prefix.getOrElse("prefix-"), suffix.getOrElse("-suffix"))
    tempFi.deleteOnExit()
    new PrintWriter(tempFi) {
      // Any statements inside the body of a class in scala are executed on construction.
      // Therefore, the following try-finally block is executed immediately as we're creating
      // a standard PrinterWriter (with its implementation) and then using it.
      // Alternatively, we could have created the PrintWriter, assigned it a name,
      // then called .write() and .close() on it. Here, we're simply opting for a terser
      // representation.
      try {
        write(contents)
      } finally {
        close()
      }
    }
    tempFi
  }

  def stringAsStream(content: String): InputStream =
    new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8.name()))

  val exampleFormattedData: String =
    """O	O	0	O	IN	In	NOFUNC	x	O
      |O	O	1	NP	CD	1969	NOFUNC	x	O
      |O	O	2	O	,	,	NOFUNC	x	O
      |Arg1	B-Peop	3	NP	NNP/NNP/NNP	James/Earl/Ray	NOFUNC	x	O
      |O	O	4	O	VBD	pleaded	NOFUNC	x	O
      |O	O	5	O	JJ	guilty	NOFUNC	x	O
      |O	O	6	O	IN	in	NOFUNC	x	O
      |O	B-Loc	7	NP	NNP/,/NNP	Memphis/,/Tenn.	NOFUNC	x	O
      |O	O	8	O	,	,	NOFUNC	x	O
      |O	O	9	O	TO	to	NOFUNC	x	O
      |O	O	10	NP	DT	the	NOFUNC	x	O
      |O	O	11	NP	NN	assassination	NOFUNC	x	O
      |O	O	12	O	IN	of	NOFUNC	x	O
      |O	O	13	NP	JJ	civil	NOFUNC	x	O
      |O	O	14	NP	NNS	rights	NOFUNC	x	O
      |O	O	15	NP	NN	leader	NOFUNC	x	O
      |Arg2	B-Peop	16	NP	NNP/NNP/NNP/NNP	Martin/Luther/King/Junior	NOFUNC	x	O
      |O	O	17	O	.	.	NOFUNC	x	O
      |
      |3	16	kill
      |
      |O	O	0	NP	DT	The	NOFUNC	x	O
      |O	B-Unknown	1	NP	NNP/NNP	Warren/Commission	NOFUNC	x	O
      |O	O	2	O	VBD	determined	NOFUNC	x	O
      |O	O	3	O	IN	that	NOFUNC	x	O
      |Arg1	B-Peop	4	NP	NNP/NNP/NNP	Lee/Harvey/Oswald	NOFUNC	x	O
      |O	O	5	O	VBD	fired	NOFUNC	x	O
      |O	O	6	NP	DT	a	NOFUNC	x	O
      |O	O	7	NP	JJ	high-powered	NOFUNC	x	O
      |O	O	8	NP	NN	rifle	NOFUNC	x	O
      |O	O	9	O	IN	at	NOFUNC	x	O
      |Arg2	B-Peop	10	NP	NNP	Kennedy	NOFUNC	x	O
      |O	O	11	O	IN	from	NOFUNC	x	O
      |O	O	12	NP	DT	the	NOFUNC	x	O
      |O	O	13	NP	JJ	sixth	NOFUNC	x	O
      |O	O	14	NP	NN	floor	NOFUNC	x	O
      |O	O	15	O	IN	of	NOFUNC	x	O
      |O	O	16	NP	DT	the	NOFUNC	x	O
      |O	O	17	NP	NN	building	NOFUNC	x	O
      |O	O	18	O	WRB	where	NOFUNC	x	O
      |O	O	19	NP	PRP	he	NOFUNC	x	O
      |O	O	20	O	VBD	worked	NOFUNC	x	O
      |O	O	21	O	IN	on	NOFUNC	x	O
      |O	B-Unknown	22	NP	NNP	Nov.	NOFUNC	x	O
      |O	O	23	NP	CD	22	NOFUNC	x	O
      |O	O	24	NP	,	,	NOFUNC	x	O
      |O	O	25	NP	CD	1963	NOFUNC	x	O
      |O	O	26	O	.	.	NOFUNC	x	O
      |
      |4	10	kill
      |
      |Arg1	B-Peop	0	NP	NNP/NNP	Leroy/Ivy	NOFUNC	x	O
      |O	O	1	O	IN	of	NOFUNC	x	O
      |O	B-Loc	2	NP	NNP	Oxford	NOFUNC	x	O
      |O	O	3	O	CC	and	NOFUNC	x	O
      |O	O	4	NP	PP$	his	NOFUNC	x	O
      |O	O	5	NP	NN	brother	NOFUNC	x	O
      |Arg1	B-Peop	6	NP	NNP/NNP/NNP	John/Henry/Ivy	NOFUNC	x	O
      |O	O	7	O	IN	of	NOFUNC	x	O
      |O	B-Loc	8	NP	NNP	Tupelo	NOFUNC	x	O
      |O	O	9	O	VBD	were	NOFUNC	x	O
      |O	O	10	O	VBN	indicted	NOFUNC	x	O
      |O	O	11	NP	DT	this	NOFUNC	x	O
      |O	O	12	NP	NN	month	NOFUNC	x	O
      |O	O	13	O	IN	on	NOFUNC	x	O
      |O	O	14	NP	DT	a	NOFUNC	x	O
      |O	O	15	NP	NN	charge	NOFUNC	x	O
      |O	O	16	O	IN	of	NOFUNC	x	O
      |O	O	17	NP	NN	conspiracy	NOFUNC	x	O
      |O	O	18	O	TO	to	NOFUNC	x	O
      |O	O	19	NP	NN	murder	NOFUNC	x	O
      |Arg2	B-Peop	20	NP	NNP/NNP/NNP/NNP/NNP/NNP/NNP	Lee/County/Circuit/Judge/Thomas/Gardner/III.	NOFUNC	x	O
      |
      |1	20	kill
      |6	20	kill
      |
      |""".stripMargin

  val expectedTextRelationInfo: Seq[(String, Int, Seq[RelationLine])] = Seq(
    (
      "In 1969, James Earl Ray pleaded guilty in Memphis, Tenn. to the assassination of civil rights leader Martin Luther King Junior.",
      18,
      Seq(RelationLine(arg1TokenIndex = 3, arg2TokenIndex = 16, relation = "kill"))
    ),
    (
      "The Warren Commission determined that Lee Harvey Oswald fired a high-powered rifle at Kennedy from the sixth floor of the building where he worked on Nov. 22, 1963.",
      27,
      Seq(RelationLine(arg1TokenIndex = 4, arg2TokenIndex = 10, relation = "kill"))
    ),
    (
      "Leroy Ivy of Oxford and his brother John Henry Ivy of Tupelo were indicted this month on a charge of conspiracy to murder Lee Counter Circuit Judge Thomas Gardner III.",
      21,
      Seq(
        RelationLine(arg1TokenIndex = 1, arg2TokenIndex = 20, relation = "kill"),
        RelationLine(arg1TokenIndex = 6, arg2TokenIndex = 20, relation = "kill")
      )
    )
  )

}
