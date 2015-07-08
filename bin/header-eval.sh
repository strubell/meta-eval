#!/bin/bash

memSize="2G"

grobidFile=$1
ieslFile=$2

$METAEVAL_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.eval.HeaderEval \
--grobid-file=$grobidFile \
--iesl-file=$ieslFile
