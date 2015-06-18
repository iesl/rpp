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
outputDir="--output=$2"
dir="--dir=$1"
njobs="--num-jobs=$3"
memPerJob="--mem=$4"
ncores="--num-cores=$5"

echo "dir: $dir"
echo "outputDir: $outputDir"

java -Dfile.encoding=UTF8 -cp $CP -Xmx$memory edu.umass.cs.iesl.rpp.ParallelInvoker $dir $outputDir $njobs $memPerJob
