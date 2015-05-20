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

    val lineBIndexSet = annotator.getBIndexSet(Single(SegmentCon(lineString)))


    def findMatches(text: String): List[Match] = {
      val numberStr = """[0-9]+"""
      val alphaNumericStr = """[a-zA-Z]{2,}[0-9]+"""
      val lastNameStr = annotator.getTextSet(refLastString).map(_._2).mkString("|")

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
      val bIndexSet = annotator.getBIndexSet(Single(SegmentCon(annoTypeStr)))
      bIndexSet.toList.flatMap { case index =>
        annotator.getTextOption(annoTypeStr)(index) map { case (startIndex, rawText) =>
          val text = Annotator.mkTextWithBreaks(rawText, lineBIndexSet.map(_ - startIndex), ' ')
          val strSet = findMatches(text).map(_.toString()).toSet
          (text, strSet, index)
        }
      } 
    })

    val refBIndexMap = referenceList.flatMap {
      case (refText, refStrSet, refBIndex) =>
        refStrSet.map(str => str -> refBIndex)
    } toMap

    val links = citationList.flatMap {
      case (citText, citStrSet, citBIndex) =>
        citStrSet.filter(s => refBIndexMap.contains(s)).map(s => {
          val refBIndex = refBIndexMap(s)
          Annotator.AnnotationLink(
            "citation-reference-link", 
            HashMap(
              "cit" -> (citationString, citBIndex), 
              "ref" -> (refMarkerString, refBIndex)
            )
          )
        })
    }

    annotator.annotateLink(links.toSet)

  }

}
