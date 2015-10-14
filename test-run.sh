#!/bin/bash

root="/path/to/RPP"
lexicons="$root/src/main/resources/lexicons"
input="$root/input"
output="$root/output"

$root/batchrun.sh $root $lexicons $input $output
