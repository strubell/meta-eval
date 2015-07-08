package edu.umass.cs.iesl.eval

class EvalOpts extends cc.factorie.util.DefaultCmdOptions {
  /* data */
  val grobidFile = new CmdOption("grobid-file", "", "STRING", "Filename(s) from which to read Grobid tagged data")
  val ieslFile = new CmdOption("iesl-file", "", "STRING", "Filename(s) from which to read IESL tagged data")
}
