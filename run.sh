#!/bin/bash

sbt -mem 3000 \
  -Dcc.factorie.app.nlp.lexicon.Lexicon="$1" \
  "runMain edu.umass.cs.iesl.rpp.Main $2 $3 $4 $5"

