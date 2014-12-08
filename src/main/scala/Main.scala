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
  import Annotator._

  def main(args: Array[String]): Unit = {
    val filePath = args(0)
    val builder = new SAXBuilder()
    val dom = builder.build(new File(filePath)) 
    val annotator = HeaderPartAnnotator.addAnnotation(ReferencePartAnnotator.addAnnotation(BodyParaRefAnnotator.addAnnotation(LineAnnotator.addAnnotation(TokenAnnotator.addAnnotation(new Annotator(dom))))))
    annotator.write("/home/thomas/out.svg")

  }

}
