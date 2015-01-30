package edu.umass.cs.iesl.rpp

import scala.util.matching.Regex.Match


import edu.umass.cs.iesl.xml_annotator.Annotator


object CitationProcessor extends Processor {
  import Annotator._


  //annotation types
  val citationString = "citation"
  val citationChar = 'c' 

  override def process(annotator: Annotator): Annotator =  {

    val lineString = LineProcessor.lineString
    val refLastString = ReferencePartProcessor.refLastString 
    val refMarkerString = ReferencePartProcessor.refMarkerString
    val headerString = StructureProcessor.headerString
    val bodyString = StructureProcessor.bodyString


    val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(lineString)))

    val lastNameList = annotator.getTextByAnnotationType(refLastString).distinct

    val pairList = List(headerString, bodyString).flatMap(annoTypeStr => {
      val bIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(annoTypeStr)))
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


    val refMarkerCount = annotator.getBIndexPairSet(Single(SegmentCon(refMarkerString))).size

    val matches = findCitations(refMarkerCount, text, lastNameList)

    val table = matches.flatMap(m => {
      if (m.start + 1 == m.end) {
        List(indexPairMap(m.start) -> U(citationChar))
      } else {
        (indexPairMap(m.start) -> B(citationChar)) +: {
          ((m.start + 1) until (m.end - 1)).filter(i => indexPairMap.contains(i)).map(i => {
            (indexPairMap(i) -> I)
          }) :+ (indexPairMap(m.end - 1) -> L)
        }
      }
    }).toMap

    annotator.annotate(List(citationString -> citationChar), Single(CharCon), table)

  }


  def findCitations(refMarkerCount: Int, text: String, lastNameList: List[String]) = {

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
      }).filter(ms => {
        val ratio = if (refMarkerCount > 0) ms.size * 100 / refMarkerCount else (ms.size * 100)
        val diff = ms.size - refMarkerCount
        diff >= -10 && ratio < 300
      })


      def loop(list: List[List[Match]]): List[Match] = list match {
        case Nil => List[Match]()
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

