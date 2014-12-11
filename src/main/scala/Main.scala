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

    val filePath = "../iesl-pdf-to-text/svgdump/1301.4293.svg" 

    val builder = new SAXBuilder()
    val dom = builder.build(new File(filePath)) 

    val l = List(LineProcessor, StructureProcessor, ReferencePartProcessor, HeaderPartProcessor)
    val annotator = l.foldLeft(new Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    } 

    annotator.write("/home/thomas/out.svg")

    //find all the lines
    println
    println("lines")
    val lines = annotator.getTextByAnnotationType("line")
    lines.foreach(println(_))

    //find all the references ("biblio-marker")
    println
    println("biblios")
    val biblios = annotator.getTextByAnnotationType("biblio-marker")
    biblios.foreach(println(_))

    //find all the lines that are references ("biblio-marker"),
    //which is allowed because biblio-markers are constrained by lines
    println
    println("biblio lines")
    val blines = annotator.getFilteredTextByAnnotationType("biblio-marker","line")
    blines.foreach(println(_))


    //get other data by using more primitive functions.
    //functions (in order from low to high level abstractions)
    //on Annotator instance
    //annotator.getSegment
    //annotator.getRange
    //annotator.getElementsInRange
    //annotator.getTextMapInRange
    //annotator.getTextMap
    //annotator.getAnnotatableIndexPairSet

    //on Annotator object
    //Annotator.fontSize
    //Annotator.y
    //Annotator.xs
    //Annotator.endX
    //Annotator.commonAncestor
    //Annotator.getTransformedCoords
    //Annotator.mkPairIndexSeq

    //read the Annotator source in xml-annotator
    //see example uses in LineProcessor and ReferencePartProcessor
    //send me questions at tlogan@cs.umass.edu

  }

}
