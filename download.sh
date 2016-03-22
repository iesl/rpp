#!/bin/bash

root="$PWD"
cd $root/models
wget https://s3.amazonaws.com/iesl-citation-models/CitationTagger.tgz
tar xzvf CitationTagger.tgz
wget https://s3.amazonaws.com/iesl-paperheader-models/HeaderTagger.tgz
tar xzvf HeaderTagger.tgz
cd -

