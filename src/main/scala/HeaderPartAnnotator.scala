package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import scala.io.Source
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import edu.umass.cs.iesl.paperheader.process
import edu.umass.cs.iesl.paperheader.crf

import edu.umass.cs.iesl.paperheader.crf.BioHeaderTag
import edu.umass.cs.iesl.paperheader.crf.LoadTSV

import annotator.Annotator 

object HeaderPartAnnotator {
  import Annotator._

  def addAnnotation(annotator: Annotator): Annotator =  {


    case class HeaderItem(pairIndex: (Int, Int), token: String, x: Int, y: Int, fontSize: Int)

    val rootElement = annotator.getDom().getRootElement()

    //create index from (section idx) -> (bIndex, cIndex)
    val headerBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("header")))

    val headerSet = headerBIndexPairSet.map {

      case (blockBIndex, charBIndex) =>
        val textMap = annotator.getTextMap("header")(blockBIndex, charBIndex)
        val elementMap = annotator.getElements("header")(blockBIndex, charBIndex)

        val headerItemSeq = textMap.toIndexedSeq.flatMap {
          case (_blockIndex, text) =>
            val _charIndex = if (_blockIndex == blockBIndex) charBIndex else 0
            (0 until text.size).filter(i => {
              val currNotWhite = "\\s".r.findFirstIn(text.charAt(i).toString) == None
              currNotWhite && (
                i == 0 || ("\\s".r.findFirstIn(text.charAt(i - 1).toString) != None)
              )
            }).map(i => {
              val e = elementMap(_blockIndex)
              val (xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
              HeaderItem(
                _blockIndex -> (_charIndex + i),
                text.substring(i).split("\\s")(0),
                xs(i).toInt,
                ys(i).toInt,
                Annotator.fontSize(e).toInt
              )
            })

        }

       headerItemSeq 

    }


    val str = headerSet.map(headerItemSeq => {
      "#\n" + (headerItemSeq.map {
        case HeaderItem(_, token, x, y, fontSize) =>
          token + "\t" + x + "\t" + y + "\t" + fontSize
      }).mkString("\n")
    }).mkString("\n\n") + "\n\n#"


    val docs = {
      val ds = (new LoadTSV(false)).fromSource(Source.fromString(str)).toIndexedSeq
      process.DocProcessor(ds)
      ds.toIndexedSeq
    } 

    val headerSeq = headerSet.toIndexedSeq
    val indexTypeTriple2LabelList = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq = headerSeq(docIdx)
        val pairIndexSeq = headerItemSeq.map(_.pairIndex)
        val typeLabelList = doc.sections.flatMap(_.tokens).map(_.attr[BioHeaderTag].categoryValue)

        typeLabelList.toList.zipWithIndex.map {
          case (typeLabel, lsIdx) =>
            val (bIndex, cIndex) = pairIndexSeq(lsIdx)
            val typeString = typeLabel.take(1)
            val labelString = typeLabel.drop(2)
            (bIndex, cIndex, labelString) -> (typeString match {
              case "B" => B(typeString.toCharArray()(0))
              case "I" => I
              case "O" => O
            })
        }
    } toList

    type IISTrip = (Int, Int, String)
    type StringLabelMap = Map[String, Label]

    def replaceBIWithUL(list: List[(IISTrip, Label)]): List[(IISTrip, Label)] = {

      def loop(
          nextLabelMap: StringLabelMap, 
          reverseList: List[(IISTrip, Label)]
      ): List[(IISTrip, Label)] = {
        reverseList match {
          case Nil => 
            List()
          case (triple, label)::xs =>
            val typeString = triple._3
            val _nextLabelMap = nextLabelMap + (typeString -> label) 
            val _label = (label, nextLabelMap.get(typeString)) match {
              case (I, Some(B(_))) => L
              case (B(c), Some(B(_))) => U(c)
              case _ => label
            }
            (triple -> _label)::loop(_nextLabelMap, xs)
        }

      }

      loop(HashMap[String, Label](), list.reverse).reverse
    }

    val tripLabelMap = replaceBIWithUL(indexTypeTriple2LabelList).toMap

    val typeStringList = List(
        "institution", 
        "address", 
        "title", 
        "author", 
        "tech", 
        "date", 
        "note", 
        "abstract", 
        "email"
    )

    typeStringList.foldLeft(annotator) {
      case (anno, typeString) =>
        val c = typeString.toCharArray()(0)
        anno.annotate(List(typeString -> c), Single(SegmentCon("token")), (blockIndex, charIndex) => {
          tripLabelMap.get((blockIndex, charIndex, typeString))
        })
    } 

  }

}
