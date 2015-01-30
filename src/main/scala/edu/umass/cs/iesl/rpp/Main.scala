package edu.umass.cs.iesl.rpp

import java.io.File

import org.jdom2.input.SAXBuilder

import edu.umass.cs.iesl.xml_annotator.Annotator

object Main {

  def main(args: Array[String]): Unit = {

    val referenceModelUri = args(0) 
    val inFilePath = args(1) 
    val outFilePath = args(2)

    val builder = new SAXBuilder()
    val dom = builder.build(new File(inFilePath)) 

    val l = List(
        LineProcessor, 
        StructureProcessor, 
        ReferencePartProcessor(referenceModelUri), 
        CitationProcessor, 
        CitationReferenceLinkProcessor, 
        HeaderPartProcessor
    )
    val annotator = l.foldLeft(Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    } 

    annotator.write(outFilePath)

    import Annotator._


    { //find citation and reference marker pairs 
      println
      println("citation and reference marker pairs")
      val citRefPair = annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
        val linkMap = annoLink.attrValueMap
        val (citationString, citBlockIndex, citCharIndex) = linkMap("cit")
        val (refMarkerString, refBlockIndex, refCharIndex) = linkMap("ref")

        val citString = annotator.getTextMap(citationString)(citBlockIndex, citCharIndex).values.map(_._2).mkString("")
        val refString = annotator.getTextMap(refMarkerString)(refBlockIndex, refCharIndex).values.map(_._2).mkString("")

        (citString, refString)

      })

      citRefPair.foreach(p => {
        println(p)
      })
    }

    { //find lines with citations and reference pairs 
      println
      println("citation and reference(biblio-marker) pairs")

      val bibMarkIndexPairSeq = annotator.getBIndexPairSet(Single(SegmentCon("biblio-marker"))).toIndexedSeq

      val citRefPair = annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
        val linkMap = annoLink.attrValueMap
        val (citationString, citBlockIndex, citCharIndex) = linkMap("cit")
        val (refMarkerString, refBlockIndex, refCharIndex) = linkMap("ref")

        val citString = annotator.getTextMap(citationString)(citBlockIndex, citCharIndex).values.map(_._2).mkString("")

        def pairLte(p1: (Int, Int), p2: (Int, Int)) = (p1._1 < p2._2) || (p1._1 == p2._1 && p1._2 <= p2._2)
        val bibMarkerIndex = bibMarkIndexPairSeq.lastIndexWhere(p => pairLte(p, (refBlockIndex, refCharIndex)))
        val bibMarkerIndexPair = bibMarkIndexPairSeq(bibMarkerIndex)

        val lineBIndexPairSet = annotator.getBIndexPairSet(Range("biblio-marker", SegmentCon("line")))
        val refString = mkTextWithBreaks(annotator.getTextMap("biblio-marker")(bibMarkerIndexPair._1, bibMarkerIndexPair._2), lineBIndexPairSet, '\n')
        
        (citString, refString)

      })

      citRefPair.foreach(p => {
        println("citation: " + p._1)
        println("reference: " + p._2)
        println()
      })
    }

    { //find author names 
      println
      println("header author names (1)")
      val authorBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header-author")))

      val authorList: List[List[String]] = authorBIndexPairSet.toList.map(bIndexPair => {
        val (blockIndex, charIndex) = bIndexPair
        val authorSegment = annotator.getSegment("header-author")(blockIndex, charIndex) 
        authorSegment.toList.flatMap { case (bi, labelMap) =>
          labelMap.map { case (ci, label) => 
            annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
          }
        }
      })

      authorList.foreach(a => println(a))
    }

    { //find author names in a different way 
      println
      println("header author names (2)")

      val authorBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header-author")))
      val authorTokenBIndexPairSet = annotator.getBIndexPairSet(Range("header-author", SegmentCon("header-token")))

      val authorList: List[List[String]] = authorTokenBIndexPairSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
        val (bi, ci) = tokenIndexPair 
        val authorToken: String = annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
        if (authorBIndexPairSet.contains(tokenIndexPair)) {
          List(authorToken) :: listAcc
        } else {
          (authorToken :: listAcc.head) :: listAcc.tail
        }
      }).reverse.map(_.reverse)

      authorList.foreach(a => println(a))
    }


    { //find all the lines
      println
      println("lines")
      val lines = annotator.getTextByAnnotationType("line")
      lines.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))

      //find all the references ("biblio-marker")
      println
      println("biblios")
      val biblios = annotator.getTextByAnnotationType("biblio-marker")
      biblios.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
    }


    { //if you don't want the lines within each biblio/refernece glued together, you can add line breaks 
      println
      println("biblios with line breaks")
      import Annotator._
      val biblioBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("biblio-marker")))
      val lineBIndexPairSet = annotator.getBIndexPairSet(Range("biblio-marker", SegmentCon("line")))
      val biblios2 = biblioBIndexPairSet.toList.map {
        case (blockBIndex, charBIndex) =>
            val textMap = annotator.getTextMap("biblio-marker")(blockBIndex, charBIndex)
            val text = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)
            text
      }
      biblios2.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
    }

    { //find all the lines that are references ("biblio-marker"),
      //which is allowed because biblio-markers are constrained by lines
      println
      println("biblio lines")
      val blines = annotator.getFilteredTextByAnnotationType("biblio-marker","line")
      blines.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
    }

    { //check the annotations for every type
      import Annotator._
      val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("line")))
      annotator.annotationInfoMap.keys.map(annoTypeString => {

        println
        println(annoTypeString)

        val bIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(annoTypeString)))
        val annotations = bIndexPairSet.toList.map {
          case (blockBIndex, charBIndex) =>
              val textMap = annotator.getTextMap(annoTypeString)(blockBIndex, charBIndex)
              Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet, ' ').trim()
        }
        annotations.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
      })
    }


    { //see all the annotations types that exists

      println("")
      println("annotation types: ")

      annotator.annotationInfoMap.map {
        case (annoTypeString, annotationInfo) =>
          println(annoTypeString)
      }

    }

    //read the Annotator source in xml-annotator
    //see example uses in LineProcessor and ReferencePartProcessor
    //send me questions at tlogan@cs.umass.edu

  }

}
