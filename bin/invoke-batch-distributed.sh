#!/bin/bash

memory=2g

if [ ! -f "$RPP_ROOT/CP.hack" ]
then
    echo "generating CP.hack ..."
    cd $RPP_ROOT
     mvn compile -X \
     | grep 'classpathElements = ' \
     | sed 's#^.* classpathElements = \[\(.*\)\]$#\1#g' \
     | sed 's#, #:#g' \
     | head -1 \
     > $RPP_ROOT/CP.hack
     echo "...done"
fi

CP=`cat $RPP_ROOT/CP.hack`

root=$RPP_ROOT
lexicons=file:///iesl/canvas/strubell/rpp/lexicon
citeCRF="--reference-model-uri=file://$root/citationCRF.factorie"
headerCRF="--header-tagger-model=$root/headerCRF.factorie"
outputDir="--output-dir=$2"
dir="--dir=$1"
njobs="--num-jobs=$3"
memPerJob="--mem=$4"
ncores="--num-cores=$5"

echo "input dir: $dir"
echo "output dir: $outputDir"

java -Dfile.encoding=UTF8 -Dcc.factorie.app.nlp.lexicon.Lexicon=$lexicons -cp $CP -Xmx$memory \
edu.umass.cs.iesl.rpp.ParallelInvoker \
$dir $outputDir $njobs $memPerJob $citeCRF $headerCRF
