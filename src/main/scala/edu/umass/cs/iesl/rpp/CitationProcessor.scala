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


    val lineBIndexSet = annotator.getBIndexSet(Single(SegmentCon(lineString)))

    val lastNameList = annotator.getTextByAnnotationType(refLastString).distinct

    val textPairList = List(headerString, bodyString).flatMap(annoTypeStr => {
      val bIndexSet = annotator.getBIndexSet(Single(SegmentCon(annoTypeStr)))
      bIndexSet.toList.flatMap(index => {
        annotator.getText(annoTypeStr)(index)
      })
    })


    val text = textPairList.map { case (startIndex, str) => 
      mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex), ' ')
    } mkString(" ")

    val breakMap: Map[Int, Int] = {

      textPairList.foldLeft(Map[Int, Int]()) { case (mapAcc, (startIndex, str)) =>

        val innerBreakMap = mkBreakMap(str.size, lineBIndexSet.map(_ - startIndex))
        if (mapAcc.isEmpty) {
          innerBreakMap.map(p => (p._1, p._2 + startIndex))
        } else {
          val jump = mapAcc.keySet.max + 2
          mapAcc ++ innerBreakMap.map { case (breakIndex, i) =>
            (breakIndex + jump) -> (i + startIndex)
          }
        }
      }

    }


    val refMarkerCount = annotator.getBIndexSet(Single(SegmentCon(refMarkerString))).size

    val matches = findCitations(refMarkerCount, text, lastNameList)

    val table: Map[Int, Label] = matches.flatMap(m => {
      if (m.start + 1 == m.end) {
        List(breakMap(m.start) -> U(citationChar))
      } else {
        (breakMap(m.start) -> B(citationChar)) +: {
          ((m.start + 1) until (m.end - 1)).filter(i => breakMap.contains(i)).map(i => {
            (breakMap(i) -> I)
          }) :+ (breakMap(m.end - 1) -> L)
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

