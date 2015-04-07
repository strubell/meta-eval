package org.allenai.scholar.metrics

// Precision/recall measurement where either may not be defined
case class PR(precision: Option[Double], recall: Option[Double]) {
  override def toString: String = "PR: precision=" + precision.getOrElse("<none>") + " recall=" + recall.getOrElse("<none>")
}
