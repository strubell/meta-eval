package org.allenai.scholar.metrics.metadata

import java.io.File

import org.allenai.scholar.metrics.{ ErrorAnalysis, PR }
import org.allenai.scholar.{ Author, MetadataAndBibliography, PaperMetadata }

import scala.io.Source

/** Eval objects define evaluations of different metadata extraction algorithms.
  * @param algoName The extraction algorithm's name.
  * @param taggedFiles Raw extracted files output by the algorithm.
  * @param taggedFileParser Function that parses an extracted file to produce core metadata.
  */
case class Eval(
    algoName: String,
    taggedFiles: Array[File],
    taggedFileParser: File => Option[MetadataAndBibliography]
) {
  def computeEval(
    groundTruthMetadata: Map[String, PaperMetadata],
    groundTruthBibs: Map[String, Map[String, PaperMetadata]],
    idFilter: String => Boolean
  ): Iterable[ErrorAnalysis] = {
    println(s"DEBUG: Eval.computeEval for $algoName ....")
    val predictions = for {
      f <- taggedFiles
      id = f.getName.split('.')(0)
      if idFilter(id)
      predicted <- taggedFileParser(f)
    } yield (id, predicted)

    val filteredFiles = taggedFiles.filter { file =>
      val nameParts = file.getName.split('.')
      assert(nameParts.length > 0)
      val id = nameParts(0)
      idFilter(id)
    }
    println(s"DEBUG: Eval.computeEval: filteredFiles size = ${filteredFiles.length}")
    val predictions2 = filteredFiles.map(file => taggedFileParser(file))
    println(s"DEBUG: Eval.computeEval: predictions2 size = ${predictions2.length}")

    println(s"DEBUG: Eval.computeEval: predictions size = ${predictions.size}")
    predictions.foreach {
      case (id, predicted) => println(s"DEBUG: Eval.computeEval: id=$id predicted=$predicted")
    }
    val goldMetadata: Map[String, PaperMetadata] = groundTruthMetadata.filterKeys(idFilter)
    println(s"DEBUG: goldMetadata size = ${goldMetadata.size}")
    goldMetadata.foreach {
      case (s, pm) => println(s"DEBUG: Eval.computeEval: goldMetadata entry: $s ${pm.toString}")
    }
    val predictedMetadata = predictions.toMap.mapValues(_.metadata)
    val metadataMetrics = MetadataErrorAnalysis.computeMetrics(goldMetadata, predictedMetadata)
    val predictedBibs = predictions.toMap.mapValues(_.bibs.toSet)
    val goldBibs = groundTruthBibs.filterKeys(idFilter).mapValues(_.values.toSet)
    val bibliographyMetrics = BibliographyErrorAnalysis.computeMetrics(goldBibs, predictedBibs)
    println(s"DEBUG: metadataMetrics=${metadataMetrics.toString()}")
    println(s"DEBUG: bibliographyMetrics=${bibliographyMetrics.toString()}")
    metadataMetrics ++ bibliographyMetrics
  }

  /** Run evaluation, print out summary, and save match data to Tableau format.
    * @param groundTruthMetadata map paper ids to ground truth core metadata.
    * @param groundTruthBibs map of paper ids to a map of "bibKey" to cited core metadata
    * @param idFilter only keep paper ids matching this filter
    */
  def run(
    groundTruthMetadata: Map[String, PaperMetadata],
    groundTruthBibs: Map[String, Map[String, PaperMetadata]],
    idFilter: String => Boolean
  ): Unit = {
    println(s"DEBUG: run2() ......")
    val analysis: Iterable[ErrorAnalysis] = computeEval(groundTruthMetadata, groundTruthBibs, idFilter)
    for (a <- analysis) {
      println(a.toString)
    }
    writeToFile(s"${algoName}-summary.txt") { w =>
      w.println("Metric\tPrecision\tRecall")
      for (ErrorAnalysis(metric, PR(p, r), _) <- analysis) {
        println("DEBUG: Eval: got: p=" + p.toString + " r=" + r.toString)
        w.println(s"""$metric\t${p.getOrElse("")}\t${r.getOrElse("")}""")
      }
    }
    val detailsDir = new File(s"${algoName}-details")
    detailsDir.mkdirs()
    def format(a: Any): String = a match {
      case a: Author => a.productIterator.map(format).filter(_.size > 0).mkString(" ")
      case m: PaperMetadata => s"${m.authors.map(_.lastName).mkString(" & ")} ${m.year}"
      case p: Product =>
        p.productIterator.map(format).mkString(",")
      case i: Iterable[_] => i.map(format).mkString(" ")
      case _ => a.toString
    }
    for (ErrorAnalysis(metric, _, examples) <- analysis) {
      writeToFile(new File(detailsDir, s"$metric.txt").getCanonicalPath) { w =>
        w.println("id\tPrecision\tRecall\tTruth\tPredicted")
        for ((id, ex) <- examples) {
          val truth = ex.trueLabels.map(format).mkString("|")
          val predictions = ex.predictedLabels.map(format).mkString("|")
          val PR(p, r) = ex.precisionRecall
          w.println(s"""$id\t${p.getOrElse("")}\t${r.getOrElse("")}\t$truth\t$predictions""")
        }
      }
    }
  }

  def run(
    groundTruthMetadataFile: String,
    groundTruthCitationEdgesFile: String,
    idWhiteListFile: Option[String] = None
  ): Unit = {
    println(s"DEBUG: run1() ......")
    import PaperMetadata._
    println(s"DEBUG: idWhiteListFile=$idWhiteListFile")
    val groundTruthMetadata = fromJsonLinesFile(groundTruthMetadataFile)
    val citationEdges = for {
      line <- Source.fromFile(groundTruthCitationEdgesFile).getLines.toIterable
      s = line.split('\t')
      if s.length > 1
    } yield {
      (s(0), s(1))
    }
    val bibs = MetadataAndBibliography.edgesToBibKeyMap(citationEdges, groundTruthMetadata)
    idWhiteListFile match {
      case Some(fn) if new File(fn).exists =>
        val whiteList = Source.fromFile(fn).getLines.toSet
        run(groundTruthMetadata, bibs, whiteList.contains(_))
      case _ => run(groundTruthMetadata, bibs, id => true)
    }
  }
}
