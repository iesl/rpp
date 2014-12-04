package rpp 

import scala.collection.immutable.IntMap
import org.jdom2.Element

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Token

import annotator.Annotator 

object TokenAnnotator {
  import Annotator._


  def addAnnotation(annotator: Annotator): Annotator = {

    def tokenize(str: String) = {
      val d = new Document(str)
      DeterministicTokenizer.process(d)
      d.tokens
    }

    def token2LabelMap(token: Token): IntMap[Label] = {
      if (token.stringStart + 1 == token.stringEnd) {
        IntMap(token.stringStart -> U('t'))
      } else {
        val first = token.stringStart
        val last = token.stringEnd - 1
        IntMap((token.stringStart + 1 until last).map(_ -> I): _*) + (first -> B('t')) + (last -> L)
      }
    }

    val tokenMapSeq = annotator.getElements().map(e => {
      val tokens = tokenize(e.getText())
      tokens.flatMap(token2LabelMap(_)).toMap
    }).toIndexedSeq

    annotator.annotate(List("token" -> 't'), Single(CharCon), (blockIndex, charIndex) => {
      tokenMapSeq(blockIndex).get(charIndex)
    })

  }

}
