package org.rexo.crf

/**
 * Created by klimzaporojets on 11/7/14.
 */
//object model extends ChainModel[Label, Features, Token](
//  LabelDomain,
//  FeaturesDomain,
//  l => l.token.attr[Features],
//  l => l.token,
//  t => t.attr[Label])
//
//// The Document class implements documents as sequences of sentences and tokens.
//val document = new Document("The quick brown fox jumped over the lazy dog.")
//val tokenizer = new app.nlp.segment.DeterministicTokenizer
//tokenizer.process(document)
//val segmenter = new app.nlp.segment.DeterministicSentenceSegmenter
//segmenter.process(document)
//assertStringEquals(document.tokenCount, "10")
//assertStringEquals(document.sentenceCount, "1")
//
//// Let's assign all tokens the same label for the sake of simplicity
//document.tokens.foreach(t => t.attr += new Label(t, "A"))
//// Let's also have another possible Label value to make things interesting
//LabelDomain.index("B")
//// Let's also initialize features for all tokens
//document.tokens.foreach(t => {
//val features = t.attr += new Features(t)
//// One feature for the token's string value
//features += "W=" + t.string.toLowerCase
//// And one feature for its capitalization
//features += "IsCapitalized=" + t.string(0).isUpper.toString
//})