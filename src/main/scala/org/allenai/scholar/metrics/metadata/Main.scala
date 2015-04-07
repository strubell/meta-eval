package org.allenai.scholar.metrics.metadata

import java.io.File
import java.nio.file.{ Files, Paths }

object Main extends App {
  import Config._
  /** Run only Grobid's processHeader for now, not fullText.
    * https://github.com/kermitt2/grobid/wiki/Grobid-batch-quick-start
    */
  def runGrobid(): Unit = {
    val processCmd = s"""java -Xmx4096m
                         -jar $grobidJar -gH $grobidHome
                         -gP $grobidProperties
                         -dIn $aclPdfDir
                         -dOut $grobidAclExtracted
                         -exe processFullText"""
    runProcess(processCmd)
  }

  def evalGrobid(): Unit = {
    Eval(
      algoName = "Grobid",
      taggedFiles = new File(grobidAclExtracted).listFiles,
      taggedFileParser = GrobidParser.parseCoreMetadata
    ).run(aclMetadata, aclCitationEdges, Some(aclIdWhiteList))
  }

  def runPsToText(): Unit = {
    def psToTextOutputFile(input: File): String = s"$pstotextAclExtracted/${input.getName}.xml"
    def processCmd(input: File): String =
      s"""$pstotextHome/bin/pstotext
           -output ${psToTextOutputFile(input)}
           -ligatures $pstotextHome/bin/ligatures.txt
           ${input.getAbsolutePath}"""

    val startTime = System.currentTimeMillis()
    val inputs = new File(aclPdfDir).listFiles
    inputs.foreach(input => runProcess(processCmd(input), time = false))
    println(s"Time elapsed in milliseconds: ${System.currentTimeMillis() - startTime}")
  }

  def runMetatagger(): Unit = {
    val inputs = new File(pstotextAclExtracted).listFiles
    val metataggerInput = inputs.flatMap { input =>
      val output = s"$metataggerAclExtracted/${input.getName}.tagged.xml"

      // skip if output file exists
      if (Files.exists(Paths.get(output))) None else Some(s"${input.getPath} -> $output")
    }

    runProcess(
      s"bin/runcrf",
      cwd = Some(metataggerHome),
      input = Some(metataggerInput.mkString("\n"))
    )
  }

  def evalMetatagger(): Unit = {
    Eval(
      algoName = "Metatagger",
      taggedFiles = new File(metataggerAclExtracted).listFiles,
      taggedFileParser = MetataggerParser.parseCoreMetadata
    ).run(aclMetadata, aclCitationEdges, Some(aclIdWhiteList))
  }

  def runIeslPdfToText(): Unit = {
    def processCmd(input: String, output: String) = s"$ieslPdfToTextHome/bin/run.js --svg -i $input -o $output"
    val inputs = new File(aclPdfDir).listFiles
    for (inputFile <- inputs) {
      val outputFile = s"$ieslPdfToTextExtracted/${inputFile.getName}.xml"
      val cmd = processCmd(inputFile.getPath, outputFile)
      println("--> running: " + cmd)
      runProcess(cmd, cwd = Some(ieslPdfToTextHome))
    }
  }

  def runRPP() = {
    val cmd = s"./batchrun.sh $rppHome file://$rppLexicons $ieslPdfToTextExtracted $rppExtracted"
    runProcess(cmd, cwd = Some(rppHome))
  }

  def evalRPP(): Unit = {
    val filesToEval = new File(rppExtracted).listFiles.map(_.getPath)
    println("evaluating:")
    filesToEval.foreach(println)
//    Eval(
//      algoName = "RPP",
//      taggedFiles = new File(rppExtracted).listFiles,
//      taggedFileParser = RppParser.parseCoreMetadata
//    ).run(aclMetadata, aclCitationEdges, Some(aclIdWhiteList))
  }

  val cmds = this.getClass.getDeclaredMethods.map(m => m.getName -> m).toMap

  cmds get args(0) match {
    case Some(m) =>
      println(s"Invoking ${m.getName}")
      m.invoke(this)
    case _ => println(s"Unrecognized cmd: ${args(0)}")
  }
}
