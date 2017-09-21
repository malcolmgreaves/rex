import org.rex.relation_extract.MutableConfusionMatrix
import org.scalatest.FunSuite

class MutableConfusionMatrixTest extends FunSuite {

  test("increment works with positive values") {
    val cm = new MutableConfusionMatrix(tp = 10, fp = 10, tn = 10, fn = 10)
    cm.increment(tp = 1, fp = 2, tn = 3, fn = 4)
    assert(cm.truePositives() === 11L)
    assert(cm.falsePositives() === 12L)
    assert(cm.trueNegatives() === 13L)
    assert(cm.falseNegatives() === 14L)
  }

  test("increment ignores negative and zero values") {
    val cm = new MutableConfusionMatrix()
    cm.increment(tp = -1, fp = -5, tn = -9000, fn = 0)
    assert(cm.truePositives() === 0L)
    assert(cm.falsePositives() === 0L)
    assert(cm.trueNegatives() === 0L)
    assert(cm.falseNegatives() === 0L)
  }

  test("constructor throws IllegalArgumentException with invalid initial values") {
    try {
      new MutableConfusionMatrix(tp = -1)
      fail("Should have thrown IllegalArgumentException !!!")
    } catch {
      case _: IllegalArgumentException => assert(true)
    }

    try {
      new MutableConfusionMatrix(fp = -1)
      fail("Should have thrown IllegalArgumentException !!!")
    } catch {
      case _: IllegalArgumentException => assert(true)
    }

    try {
      new MutableConfusionMatrix(tn = -1)
      fail("Should have thrown IllegalArgumentException !!!")
    } catch {
      case _: IllegalArgumentException => assert(true)
    }

    try {
      new MutableConfusionMatrix(fn = -1)
      fail("Should have thrown IllegalArgumentException !!!")
    } catch {
      case _: IllegalArgumentException => assert(true)
    }
  }

  test("accuracy") {
    val cm = new MutableConfusionMatrix()
    assert(cm.accuracy() === 0.0)

    cm.increment(tp = 1, fp = 1, tn = 1, fn = 1)
    assert(cm.accuracy() === 0.5)
  }

  test("precision") {
    val cm = new MutableConfusionMatrix()
    assert(cm.precision() === 0.0)

    cm.increment(tp = 1, fp = 3)
    assert(cm.precision() === 0.25)
  }

  test("recall") {
    val cm = new MutableConfusionMatrix()
    assert(cm.recall() === 0.0)

    cm.increment(tp = 1, fn = 4)
    assert(cm.recall() === 0.2)
  }

  test("f1") {
    val cm = new MutableConfusionMatrix()
    assert(cm.f1() === 0.0)

    cm.increment(tp = 2, fp = 1, fn = 3)
    assert(cm.f1() === 0.5)
  }

}
