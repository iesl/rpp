#!/bin/bash

#
# Set the following two variables to their corresponding values:
#
root="/path/to/RPP"
factorieJar="/path/to/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"

input="$root/input"
output="$root/output"

$root/batchrun.sh $root $factorieJar $input $output
