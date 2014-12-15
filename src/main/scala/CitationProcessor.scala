package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.JavaConversions.iterableAsScalaIterable 
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

import scala.collection.immutable.IntMap
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.util.IteratorIterable

import annotator.Annotator 

import bibie._  
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Sentence

import cc.factorie.app.nlp.Token

object CitationProcessor extends Processor {
  import Annotator._

  override def process(annotator: Annotator): Annotator =  {

    import Annotator._

    val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("line")))

    val lastNameList = annotator.getTextByAnnotationType("ref-last").distinct

    val pairList = List("header", "body").flatMap(annoTypeStr => {
      val bIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon(annoTypeStr)))
      bIndexPairSet.toList.map {
        case (blockBIndex, charBIndex) =>
          val textMap = annotator.getTextMap(annoTypeStr)(blockBIndex, charBIndex)
          val indexPairMap = Annotator.mkIndexPairMap(textMap, lineBIndexPairSet)
          val text = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet, ' ')
          (text, indexPairMap)
      } 
    })


    val text = pairList.map(_._1).mkString(" ")
    val indexPairMap = {
      val indexPairMapList = pairList.map(_._2)
      indexPairMapList.tail.foldLeft(indexPairMapList.head) {
        case (indexPairMapAcc, indexPairMap) =>
          val offset = indexPairMapAcc.lastKey + 2
          indexPairMapAcc ++ indexPairMap.map(p => (p._1 + offset) -> p._2)
      }
    }

    val matches = findCitations(text, lastNameList)

    val table = matches.flatMap(m => {
      if (m.start + 1 == m.end) {
        List(indexPairMap(m.start) -> U('c'))
      } else {
        (indexPairMap(m.start) -> B('c')) +: ((m.start + 1) until (m.end - 1)).filter(i => indexPairMap.contains(i)).map(i => {
          (indexPairMap(i) -> I)
        }) :+ (indexPairMap(m.end - 1) -> L)
      }
    }).toMap

    annotator.annotate(List("citation" -> 'c'), Single(CharCon), (blockIndex, charIndex) => {
      table.get(blockIndex -> charIndex)
    })

  }


  def findCitations(text: String, lastNameList: List[String]) = {

    val numberStr = """[0-9]{1,3}"""
    val alphaNumericStr = """[a-zA-Z]{2,}[0-9]+"""
    val authorNameStr = lastNameList.mkString("|")
      
    val numBrackRegex = ("""(\[(""" + numberStr + """[ ,;]*)+\])""").r
    val numParenRegex = ("""(\((""" + numberStr + """[ ,;]*)+\))""").r
    val alphaNumBrackRegex = ("""(\[(""" + alphaNumericStr + """[ ,;]*)+\])""").r
    val alphaNumParenRegex = ("""(\((""" + alphaNumericStr + """[ ,;]*)+\))""").r
    val authorRegex = ("""([(]?(e[.]?g[.]?)?("""+authorNameStr+""") (((and|&) [a-zA-Z-]+)|(et al[.]?){1})?[ ]?[(]?([0-9]{4}[a-z]?)[)]?[)]?)+""").r


    val bestMatchIterator = {

      val matchList =  List(numBrackRegex, numParenRegex, alphaNumBrackRegex, alphaNumParenRegex, authorRegex).map(regex => {
        regex.findAllMatchIn(text).toList
      })

      def loop(list: List[List[Match]]): List[Match] = list match {
        case x::Nil => x
        case x1::(x2::xs) => 
          if (x1.length > x2.length) {
            loop(x1::xs) 
          } else {
            loop(x2::xs)
          }
      }

      loop(matchList)

    }

    bestMatchIterator.toList
 
  }


}

