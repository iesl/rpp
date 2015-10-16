#!/bin/bash

root=$1
lexicons=$2
citeCRF="file://$root/citationCRF.factorie"
headerCRF="file://$root/headerCRF.factorie"
inputDir=$3
outputDir=$4

CP="$root/target/classes:$root/target/rpp-0.1-SNAPSHOT-jar-with-dependencies.jar"

mem="4G"

java -Xmx${mem} -cp $CP -Dcc.factorie.app.nlp.lexicon.Lexicon=$lexicons edu.umass.cs.iesl.rpp.BatchMain \
--reference-model-uri=$citeCRF \
--header-tagger-model=$headerCRF \
--input-dir=$inputDir \
--output-dir=$outputDir
#--data-files-file=$inputDir

#sbt -mem 12000 \
# -Dcc.factorie.app.nlp.lexicon.Lexicon=$lexicons \
# "runMain edu.umass.cs.iesl.rpp.BatchMain $citeCRF $headerCRF $inputDir $outputDir"



