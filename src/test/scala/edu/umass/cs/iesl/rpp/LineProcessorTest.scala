package edu.umass.cs.iesl.rpp

import org.scalatest._

import scala.collection.immutable.IntMap
import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.ListMap

import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import scala.io.Source

import java.io.File
import org.jdom2.input.SAXBuilder
import edu.umass.cs.iesl.xml_annotator.Annotator

import scala.sys.process._

class LineProcessorTest extends FlatSpec {

  val resourceInputPath = {
    getClass.getResource("/input").getPath() + "/"
  }

  val resourceOutputPath = {
    getClass.getResource("/line-proc-output").getPath() + "/"
  }

  def getListFromPath(file: File) = {
     val s = Source.fromFile(file)

     val items = (s.getLines().foldLeft(List[String]("")) {
       case (listAcc, line) =>
         if (line == "") {
           "" :: listAcc
         } else {
           (listAcc.head + line + "\n") :: listAcc.tail
         }
     }).filter(_ != "").map(_.dropRight(1)).reverse

     s.close

     items
  }



  Seq("find", resourceInputPath, "-type", "f").lines.foreach(inputPath => {

    val builder = new SAXBuilder()
    val dom = builder.build(new File(inputPath))

    val inputName = inputPath.stripPrefix(resourceInputPath)

    val lineListFile = new File(resourceOutputPath + inputName + ".expected/lines.txt")
    if (lineListFile.exists) {
      ("LineProccesor.process with input: " + inputName) should "determine which text belongs to which line" in {
        val expectedLines = getListFromPath(lineListFile).toIndexedSeq
        val actualLines = {
          val annotator = LineProcessor.process(Annotator(dom))
          annotator.getTextSeq("line").map(_._2).toIndexedSeq
        }

        expectedLines.zipWithIndex.map(p => {
          val (expLine, i) = p
          val actLine = actualLines(i)
          assertResult(expLine)(actLine)
        })
      }
    }

  })

}
