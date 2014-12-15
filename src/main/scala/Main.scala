package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import annotator.Annotator 

object Main {

  def main(args: Array[String]): Unit = {

    val filePath = args(0) 

    val builder = new SAXBuilder()
    val dom = builder.build(new File(filePath)) 

    val l = List(LineProcessor, StructureProcessor, ReferencePartProcessor, CitationProcessor, HeaderPartProcessor)
    val annotator = l.foldLeft(new Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    } 

    annotator.write("out.svg")

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
      val biblioBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("biblio-marker")))
      val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Range(SegmentCon("biblio-marker"), SegmentCon("line")))
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
      val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("line")))
      annotator.annotationInfoMap.keys.map(annoTypeString => {

        println
        println(annoTypeString)

        val bIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon(annoTypeString)))
        val annotations = bIndexPairSet.toList.map {
          case (blockBIndex, charBIndex) =>
              val textMap = annotator.getTextMap(annoTypeString)(blockBIndex, charBIndex)
              Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet).trim()
        }
        annotations.zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
      })
    }


    //read the Annotator source in xml-annotator
    //see example uses in LineProcessor and ReferencePartProcessor
    //send me questions at tlogan@cs.umass.edu

  }

}
