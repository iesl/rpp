#!/bin/bash

root="/Users/kate/research/citez/rpp"
lexicons="file://$root/main/resources/lexicons"
citeCRF="file://$root/citationCRF.factorie"
headerCRF="$root/headerCRF.factorie"
input="/Users/kate/research/citez/data/1503.06760.pdf.xml"
output="$input.tagged"

./run.sh $lexicons $citeCRF $headerCRF $input $output
