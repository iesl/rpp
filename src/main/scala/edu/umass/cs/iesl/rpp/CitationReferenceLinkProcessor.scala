package edu.umass.cs.iesl.rpp

import scala.collection.immutable.HashMap
import scala.util.matching.Regex.Match


import edu.umass.cs.iesl.xml_annotator.Annotator


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
          Annotator.AnnotationLink(
            "citation-reference-link", 
            HashMap(
              "cit" -> (citationString, citBIndexPair._1, citBIndexPair._2), 
              "ref" -> (refMarkerString, refBIndexPair._1, refBIndexPair._2)
            )
          )
        })
    }

    annotator.annotateLink(links.toSet)

  }

}
