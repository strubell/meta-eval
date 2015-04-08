package org.allenai.scholar.metrics

// Precision/recall measurement where either may not be defined
case class PR(precision: Option[Double], recall: Option[Double]) {
  def f1: Double = {
    val r = recall.getOrElse(0.0)
    val p = precision.getOrElse(0.0)
    if (r != 0.0 && p != 0.0) 2 * ((p * r) / (p + r)) else -1.0
  }
  override def toString: String = "PR: precision=" + precision.getOrElse("<none>") + " recall=" + recall.getOrElse("<none>") + " f1=" + f1.toString
}
