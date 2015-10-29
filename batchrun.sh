#!/bin/bash

root=$1
facjar=$2
citeCRF="file://$root/citationCRF.factorie"
headerCRF="$root/headerCRF.factorie"
inputDir=$3
outputDir=$4

CP="$root/target/rpp-0.1-SNAPSHOT-jar-with-dependencies.jar:$facjar"

mem="4G"

java -Xmx${mem} -cp $CP edu.umass.cs.iesl.rpp.BatchMain \
--reference-model-uri=$citeCRF \
--header-tagger-model=$headerCRF \
--input-dir=$inputDir \
--output-dir=$outputDir




