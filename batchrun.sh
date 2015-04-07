#!/bin/bash

root=$1
lexicons=$2
citeCRF="file://$root/citationCRF.factorie"
headerCRF="$root/headerCRF.factorie"
inputDir=$3
outputDir=$4

sbt -mem 4000 \
 -Dcc.factorie.app.nlp.lexicon.Lexicon=$lexicons \
 "runMain edu.umass.cs.iesl.rpp.BatchMain $citeCRF $headerCRF $inputDir $outputDir"



