package edu.umass.cs.iesl.eval

import cc.factorie.app.chain.SegmentEvaluation
import cc.factorie.app.nlp.{ Document, Token }
import cc.factorie.variable.{ LabeledCategoricalVariable, CategoricalDomain }

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Reads in a tagged file output by Grobid, evaluates it using out evaluation
  */
object HeaderEval extends App {
  val opts = new EvalOpts()
  opts.parse(args)

  object HeaderLabelDomain extends CategoricalDomain[String]

  class HeaderLabel(labelname: String, val token: Token) extends LabeledCategoricalVariable(labelname) {
    def domain = HeaderLabelDomain
  }

  /* Load Grobid owpl output as tagged Factorie document */
  def loadGrobid(lines: Iterator[String]): IndexedSeq[Document] = {
    val docs = ArrayBuffer[Document]()
    var doc = new Document()
    var lastGold = ""
    var lastTagged = ""
    while (lines.hasNext) {
      val line = lines.next()
      if (line.trim == "") {
        docs += doc
        doc = new Document()
      } else {
        val splitLine = line.split("\t")
        val tok = new Token(doc, splitLine(0))
        val gold = splitLine(splitLine.length - 2).replaceAll("<|>", "")
        val tagged = splitLine.last.replaceAll("<|>", "")
        val goldBI = if (gold(0) == 'I') "B" + gold.drop(1) else if (gold != lastGold) "B-" + gold else "I-" + gold
        val taggedBI = if (tagged(0) == 'I') "B" + tagged.drop(1) else if (tagged != lastTagged) "B-" + tagged else "I-" + tagged
        HeaderLabelDomain += goldBI
        try {
          val label = new HeaderLabel(goldBI, tok)
          label.set(HeaderLabelDomain.index(taggedBI))(null)
          tok.attr += label
        } catch {
          case e: IndexOutOfBoundsException => throw new Error(s"Label $taggedBI or $goldBI not in domain")
        }
        doc.asSection += tok
        lastGold = goldBI.drop(2)
        lastTagged = taggedBI.drop(2)
      }
    }
    docs += doc
    docs
  }

  /* Load IESL owpl output as tagged Factorie document */
  def loadIESL(lines: Iterator[String]): IndexedSeq[Document] = {
    val docs = ArrayBuffer[Document]()
    var doc = new Document()
    var lastGold = ""
    var lastTagged = ""
    while (lines.hasNext) {
      val line = lines.next()
      //    println(s"line: |$line|")
      if (line.trim == "") {
        docs += doc
        doc = new Document()
      } else {
        val splitLine = line.split("\t")
        val tok = new Token(doc, splitLine(0))
        val gold = splitLine(splitLine.length - 2)
        val tagged = splitLine.last
        val goldBI = if (gold.drop(2) != lastGold) "B" + gold.drop(1) else gold
        val taggedBI = if (tagged.drop(2) != lastTagged) "B" + tagged.drop(1) else tagged
        try {
          val label = new HeaderLabel(goldBI, tok)
          label.set(HeaderLabelDomain.index(taggedBI))(null)
          tok.attr += label
        } catch {
          case e: IndexOutOfBoundsException => throw new Error(s"Label $taggedBI or $goldBI not in domain")
        }
        doc.asSection += tok
        lastGold = goldBI.drop(2)
        lastTagged = taggedBI.drop(2)
      }
    }
    docs += doc
    docs
  }

  val grobidLines = io.Source.fromFile(opts.grobidFile.value, "iso-8859-1").getLines()
  val ieslLines = io.Source.fromFile(opts.ieslFile.value, "utf-8").getLines()

  val grobidDocs = loadGrobid(grobidLines)
  val ieslDocs = loadIESL(ieslLines)

  println(s"Grobid doc count = ${grobidDocs.length}; IESL doc count = ${ieslDocs.length}")
  println(s"Grobid tok count = ${grobidDocs.flatMap(_.tokens).length}; IESL tok count = ${ieslDocs.flatMap(_.tokens).length}")

  var i = 0
  grobidDocs.flatMap(_.tokens).zip(ieslDocs.flatMap(_.tokens)).foreach {
    case (gTok, iTok) => {
      i += 1
      assert(
        gTok.attr[HeaderLabel].target.categoryValue == iTok.attr[HeaderLabel].target.categoryValue,
        s"$i grobid: ${gTok.attr[HeaderLabel].target.categoryValue}; iesl: ${iTok.attr[HeaderLabel].target.categoryValue}"
      )
    }
  }

  println("GROBID")
  val grobidEval = new SegmentEvaluation[HeaderLabel]("B-", "I-", HeaderLabelDomain, grobidDocs.flatMap(_.tokens).map(_.attr[HeaderLabel]))
  println(grobidEval)
  println(f"${grobidEval.f1 * 100}%2.2f, ${grobidEval.precision * 100}%2.2f, ${grobidEval.recall * 100}%2.2f")
  HeaderLabelDomain.categories.map(_.drop(2)).toSet.map { c: String => (c, grobidEval(c)) }.foreach { case (c, e) => println(f"$c, ${e.f1 * 100}%2.2f, ${e.precision * 100}%2.2f, ${e.recall * 100}%2.2f") }

  println()
  println("IESL")
  val ieslEval = new SegmentEvaluation[HeaderLabel]("B-", "I-", HeaderLabelDomain, ieslDocs.flatMap(_.tokens).map(_.attr[HeaderLabel]))
  println(ieslEval)

  println(f"OVERALL, ${ieslEval.f1 * 100}%2.2f, ${ieslEval.precision * 100}%2.2f, ${ieslEval.recall * 100}%2.2f")
  HeaderLabelDomain.categories.map(_.drop(2)).toSet.map { c: String => (c, ieslEval(c)) }.foreach { case (c, e) => println(f"$c, ${e.f1 * 100}%2.2f, ${e.precision * 100}%2.2f, ${e.recall * 100}%2.2f") }

}
