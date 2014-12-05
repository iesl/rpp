package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import annotator.Annotator 

object ReferencePartAnnotator {
  import Annotator._

  def addAnnotation(annotator: Annotator): Annotator =  {

    import bibie._  
    import cc.factorie.app.nlp.Document
    import cc.factorie.app.nlp.segment.DeterministicTokenizer
    import cc.factorie.app.nlp.Sentence

    val trainer = TestCitationModel.loadModel(
      "file:///home/thomas/iesl/citationCRF.factorie",
      "file:///home/thomas/iesl/bibie/src/main/resources/lexicons"
    )

    val refBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("biblio-marker")))

    val docAndPairIndexSeqSet = {
      val pairs = refBIndexPairSet.toSeq.map {
        case (blockIndex, charIndex) =>
          val textMap = annotator.getTextMap("biblio-marker")(blockIndex, charIndex)
          val pairIndexSeq = textMap.toIndexedSeq.flatMap {
            case (_blockIndex, text) =>
              val _charIndex = if (_blockIndex == blockIndex) charIndex else 0
              (0 until text.size).map(i => _blockIndex -> (_charIndex + i))
          }

          val doc = {
            val text = textMap.values.mkString("")
            println("ref text: " + text)
            val d = new Document(text)
            DeterministicTokenizer.process(d)
            new Sentence(d.asSection, 0, d.tokens.size)
            d.tokens.foreach(t => {
              t.attr += new CitationLabel("", t)
            })
            d
          }

          (doc -> pairIndexSeq)
      }
      TestCitationModel.process(pairs.map(_._1).filter(_.tokens.size > 1), trainer, false)
      pairs
      
    }

    val pairIndex2typeLabelMapList= docAndPairIndexSeqSet.flatMap {
      case (doc, pairIndexSeq) =>
        doc.tokens.map(token => {
          val labelTypeStringList = token.attr[CitationLabel].categoryValue.split(":")
          val pairIndex = pairIndexSeq(token.stringStart)
          val typeLabelMap = labelTypeStringList.filter(!_.isEmpty).map(labelTypeString => {
            val labelString = labelTypeString.take(1)
            val typeString = labelTypeString.drop(2)
            val label: Label = (labelString match {
              case "B" => B(typeString.toCharArray()(0))
              case "I" => I
              case "O" => O
            })
            typeString -> label
          }).toMap

          pairIndex -> typeLabelMap
        })
    } 

    type IntPair = (Int, Int)
    type StringLabelMap = Map[String, Label]
    def replaceBIWithUL(
        nextLabelMap: StringLabelMap, 
        reverseList: List[(IntPair, StringLabelMap)]
    ): List[(IntPair, StringLabelMap)] = {
      reverseList match {
        case Nil => 
          List()
        case (pairIndex, typeLabelMap)::xs =>
          val _nextLabelMap = nextLabelMap ++ typeLabelMap
          val _typeLabelMap = typeLabelMap.map { case (string, label) => 
            (label, nextLabelMap.get(string)) match {
              case (I, Some(B(_))) => (string -> L)
              case (B(c), Some(B(_))) => (string -> U(c))
              case _ => (string -> label)
            }
          }
          (pairIndex -> _typeLabelMap)::replaceBIWithUL(_nextLabelMap, xs)
      }

    }

    val typeLabelMapMap = replaceBIWithUL(HashMap[String, Label](), pairIndex2typeLabelMapList.toList.reverse).toMap

    val typeStrings = List("authors", "person", "person-last", "person-first", "date", "year", "title", "venue", "journal")

    typeStrings.foldLeft(annotator) {
      case (anno, typeString) =>
        val c = typeString.toCharArray()(0)
        anno.annotate(List(typeString -> c), Single(SegmentCon("token")), (blockIndex, charIndex) => {
          typeLabelMapMap.get(blockIndex -> charIndex).flatMap(_.get(typeString))
        })
    } 

  }

}
