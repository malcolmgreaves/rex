package org.rex.relation_extract

class MutableConfusionMatrix() extends Mutable {

  private[this] var tp: Long = 0L
  private[this] var fp: Long = 0L
  private[this] var tn: Long = 0L
  private[this] var fn: Long = 0L

  /** Assigns each supplied value to its respective internal one.
    *
    * @throws IllegalArgumentException If any input is negative.
    */
  def this(tp: Long = 0L, fp: Long = 0L, tn: Long = 0L, fn: Long = 0L) {
    this()

    this.tp = tp
    if (this.tp < 0L)
      throw new IllegalArgumentException(s"Cannot have negative value for true positive: $tp")

    this.fp = fp
    if (this.fp < 0L)
      throw new IllegalArgumentException(s"Cannot have negative value for false positive: $fp")

    this.tn = tn
    if (this.tn < 0L)
      throw new IllegalArgumentException(s"Cannot have negative value for true negative: $tn")

    this.fn = fn
    if (this.fn < 0L)
      throw new IllegalArgumentException(s"Cannot have negative value for false negative: $fn")
  }

  /** Mutates the internal state by incrementing internal values with the supplied ones.
    *
    * Only updates if the supplied value is positive.
    * Returns a reference to this.
    */
  def increment(tp: Long = 0L, fp: Long = 0L, tn: Long = 0L, fn: Long = 0L): this.type = {

    if (tp > 0L) {
      this.tp += tp
    }

    if (fp > 0L) {
      this.fp += fp
    }

    if (tn > 0L) {
      this.tn += tn
    }

    if (fn > 0L) {
      this.fn += fn
    }

    this
  }

  def truePositives(): Long =
    tp

  def falsePositives(): Long =
    fp

  def trueNegatives(): Long =
    tn

  def falseNegatives(): Long =
    fn

  def accuracy(): Double = {
    val denominator = tp + fp + tn + fp
    if (denominator == 0L)
      0.0
    else
      (tp + tn).toDouble / denominator.toDouble
  }

  def precision(): Double = {
    val denominator = tp + fp
    if (denominator == 0L)
      0.0
    else
      tp.toDouble / denominator.toDouble
  }

  def recall(): Double = {
    val denominator = tp + fn
    if (denominator == 0L)
      0.0
    else
      tp.toDouble / denominator.toDouble
  }

  def f1(): Double = {
    val denominator = precision() + recall()
    if (denominator == 0.0)
      0.0
    else
      (2.0 * precision() * recall()) / denominator
  }

  override def toString: String = {
    s"""
       |                  Truth
       |              +           -
       |
       |         +   $tp         $fp
       |Predict
       |         -   $fn         $tn
       |
       |""".stripMargin
  }

}
