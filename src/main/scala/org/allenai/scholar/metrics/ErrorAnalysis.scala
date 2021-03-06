package org.allenai.scholar.metrics

import org.allenai.scholar.metrics.PrecisionRecall._

case class ErrorAnalysis(
    metricName: String,
    pr: PR,
    details: Iterable[(String, Example[_])]
) {
  override def toString: String = s"ErrorAnalysis: $metricName ${pr.toString}"
}

object ErrorAnalysis {
  def computeMetrics[T](
    truth: Seq[(String, T)],
    predicted: Map[String, T],
    metrics: (String, T => Iterable[_])*
  ) = {

    val examples: Iterable[(String, (String, Example[_]))] =
      for {
        (id, trueData) <- truth
        predictedData = predicted.get(id)
        (metric, extract) <- metrics
      } yield {
        val trueLabels = extract(trueData)
        val predictedLabels: Iterable[_] = predictedData match {
          case Some(p) => extract(p)
          case _ => None
        }
        (metric, (id, Example(trueLabels, predictedLabels)))
      }
    val groupedExamples = examples.groupBy(_._1).mapValues(_.map(_._2))
    for ((metric, examples) <- groupedExamples) yield {
      val pr = measurePR(examples.map(_._2))
      ErrorAnalysis(metric, pr, examples)
    }
  }
}

