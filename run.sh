#!/bin/bash
args=("$@")


# -Dcc.factorie.app.nlp.lexicon.Lexicon="cc.factorie.app.nlp.lexicon" \
sbt -mem 3000 \
  "runMain edu.umass.cs.iesl.rpp.Main --reference-model ../rpp/headerCRF.factorie --header-model ../rpp/headerCRF.factorie --input stdin"
