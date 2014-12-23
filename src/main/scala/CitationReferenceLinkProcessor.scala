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

object CitationReferenceLinkProcessor extends Processor {
  import Annotator._

  override def process(annotator: Annotator): Annotator =  {

    val lineString = LineProcessor.lineString
    val refLastString = ReferencePartProcessor.refLastString
    val refMarkerString = ReferencePartProcessor.refMarkerString
    val citationString = CitationProcessor.citationString

    val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(lineString)))


    def findMatches(text: String): List[Match] = {
      val numberStr = """[0-9]+"""
      val alphaNumericStr = """[a-zA-Z]{2,}[0-9]+"""
      val lastNameStr = annotator.getTextByAnnotationType(refLastString).distinct.mkString("|")

      val bestMatchList = {

        val matchListList = List(numberStr, alphaNumericStr, lastNameStr).map(str => {
          str.r.findAllMatchIn(text).toList
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

        loop(matchListList)

      }

      bestMatchList


    }

    val List(citationList, referenceList) = List(citationString, refMarkerString).map(annoTypeStr => {
      val bIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(annoTypeStr)))
      bIndexPairSet.toList.map {
        case (blockBIndex, charBIndex) =>
          val textMap = annotator.getTextMap(annoTypeStr)(blockBIndex, charBIndex)
          val text = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet, ' ')
          val strSet = findMatches(text).map(_.toString()).toSet
          (text, strSet, (blockBIndex, charBIndex))
      } 
    })

    val refBIndexMap = referenceList.flatMap {
      case (refText, refStrSet, refBIndexPair) =>
        refStrSet.map(str => str -> refBIndexPair)
    } toMap

    val links = citationList.flatMap {
      case (citText, citStrSet, citBIndexPair) =>
        citStrSet.filter(s => refBIndexMap.contains(s)).map(s => {
          val refBIndexPair = refBIndexMap(s)
          HashMap(citationString -> citBIndexPair, refMarkerString -> refBIndexPair)
        })
    }

    annotator.annotateLink(links.toSet)

  }

}
