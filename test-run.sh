#!/bin/bash

root="/home/kate/AI2/rpp"
lexicons="$root/src/main/resources/lexicons"
input="$root/input"
output="$root/output"

$root/batchrun.sh $root $lexicons $input $output
