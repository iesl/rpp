#!/bin/bash

fname=$1
output=$2

egrep -B 1 \
"(IndexOutOfBoundsException)|(org.rexo.referencetagging.ReferencesNotFoundException: org.rexo.referencetagging.SegmentationException: java.lang.Exception: did not find reference section)" \
$fname \
| grep "^\* " \
| awk '{print $3}' \
> $output
