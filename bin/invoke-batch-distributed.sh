#!/bin/bash

memory=15g

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
lexicons=file:///home/kate/research/lexicon
citeCRF="--reference-model-uri=file://$root/citationCRF.factorie"
headerCRF="--header-tagger-model=$root/headerCRF.factorie"
input="--data-files-file=$1"
output="--output-dir=$2"

root="/home/kate/eval/rpp"
#lexicons="--lexicons-uri=file:///home/kate/research/lexicon"
#htModel="--header-tagger-model=$root/headerCRF.factorie"
#refModel="--reference-model-uri=file://$root/citationCRF.factorie"
#dataFilesFile="--data-files-file=$root/tmp-filelist-0"
outputDir="--output-dir=$root/output/"
dir="--dir=$root/input"
njobs="--num-jobs=4"
memPerJob="--mem=4"

java -Dfile.encoding=UTF8 -cp $CP -Xmx$memory "edu.umass.cs.iesl.rpp.ParallelInvoker" $dir $ouputDir $njobs $memPerJob
