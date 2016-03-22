#!/bin/bash

root=$1
facjar=$2
citeCRF="file://$root/models/CitationTagger.factorie"
headerCRF="$root/models/HeaderTagger.factorie"
inputDir=$3
outputDir=$4

CP="$root/target/classes:$root/target/rpp-0.1-SNAPSHOT-jar-with-dependencies.jar:$facjar"

mem="4G"

java -Xmx${mem} -cp $CP edu.umass.cs.iesl.rpp.BatchMain \
--reference-model-uri=$citeCRF \
--header-tagger-model=$headerCRF \
--input-dir=$inputDir \
--output-dir=$outputDir



