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

    val refTable = HashMap(
      (78,0) -> Some(I), 
      (29,0) -> Some(L), 
      (50,0) -> Some(I),
      (5,0) -> None,
      (55,0) -> Some(B('r')),
      (135,0) -> Some(B('r')),
      (34,0) -> Some(I),
      (27,0) -> Some(I),
      (9,0) -> None,
      (91,0) -> Some(L),
      (102,0) -> Some(L),
      (100,0) -> Some(I),
      (0,0) -> None,
      (26,0) -> Some(B('r')),
      (84,0) -> Some(L),
      (32,0) -> Some(I),
      (4,0) -> None,
      (47,0) -> Some(L),
      (99,0) -> Some(B('r')),
      (134,0) -> Some(L),
      (92,0) -> Some(B('r')),
      (13,0) -> Some(I),
      (124,0) -> Some(I),
      (57,0) -> Some(I),
      (40,0) -> Some(I),
      (76,0) -> Some(B('r')),
      (15,0) -> Some(I),
      (136,0) -> Some(I),
      (119,0) -> Some(I),
      (138,0) -> Some(I),
      (2,0) -> None,
      (56,0) -> Some(I),
      (3,0) -> None,
      (93,0) -> Some(I),
      (43,0) -> Some(B('r')),
      (8,0) -> None,
      (60,0) -> Some(B('r')),
      (131,0) -> Some(I),
      (45,0) -> Some(I),
      (25,0) -> Some(L),
      (68,0) -> Some(I),
      (128,0) -> Some(L),
      (77,0) -> Some(I),
      (30,0) -> Some(B('r')),
      (105,0) -> Some(I),
      (52,0) -> Some(I),
      (67,0) -> Some(B('r')),
      (33,0) -> Some(I),
      (104,0) -> Some(B('r')),
      (38,0) -> Some(I),
      (111,0) -> Some(B('r')),
      (116,0) -> Some(L),
      (61,0) -> Some(I),
      (85,0) -> Some(B('r')),
      (36,0) -> Some(L),
      (86,0) -> Some(I),
      (83,0) -> Some(I),
      (70,0) -> Some(L),
      (71,0) -> Some(B('r')),
      (54,0) -> Some(L),
      (28,0) -> Some(I),
      (49,0) -> Some(I),
      (117,0) -> Some(B('r')),
      (81,0) -> Some(B('r')),
      (122,0) -> Some(L),
      (59,0) -> Some(L),
      (123,0) -> Some(B('r')),
      (44,0) -> Some(I),
      (114,0) -> Some(I),
      (109,0) -> Some(I),
      (17,0) -> Some(L),
      (96,0) -> Some(I),
      (88,0) -> Some(I),
      (63,0) -> Some(I),
      (73,0) -> Some(I),
      (110,0) -> Some(L),
      (95,0) -> Some(I),
      (113,0) -> Some(I),
      (10,0) -> None,
      (18,0) -> Some(B('r')),
      (42,0) -> Some(L),
      (98,0) -> Some(L),
      (140,0) -> Some(L),
      (64,0) -> Some(I),
      (90,0) -> Some(I),
      (6,0) -> None,
      (75,0) -> Some(L),
      (129,0) -> Some(B('r')),
      (80,0) -> Some(L),
      (48,0) -> Some(B('r')),
      (11,0) -> None,
      (1,0) -> None,
      (126,0) -> Some(I),
      (121,0) -> Some(I),
      (12,0) -> Some(B('r')),
      (21,0) -> Some(I),
      (23,0) -> Some(I),
      (118,0) -> Some(I),
      (107,0) -> Some(I),
      (37,0) -> Some(B('r')),
      (66,0) -> Some(L),
      (7,0) -> None,
      (132,0) -> Some(I)
    )
    val _annotator = annotator.annotate(List("reference" -> 'r'), Single(SegmentCon("line")), (blockIndex, charIndex) => {
      refTable(blockIndex -> charIndex)
    })

    import bibie._  
    import cc.factorie.app.nlp.Document
    import cc.factorie.app.nlp.segment.DeterministicTokenizer
    import cc.factorie.app.nlp.Sentence

    val trainer = TestCitationModel.loadModel(
      "file:///home/thomas/iesl/citationCRF.factorie",
      "file:///home/thomas/iesl/bibie/src/main/resources/lexicons"
    )

    val refBIndexPairSet = _annotator.getAnnotatableIndexPairSet(Single(SegmentCon("reference")))

    val docAndPairIndexSeqSet = {
      val pairs = refBIndexPairSet.toSeq.map {
        case (blockIndex, charIndex) =>
          val textMap = _annotator.getTextMap("reference")(blockIndex, charIndex)
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

    typeStrings.foldLeft(_annotator) {
      case (anno, typeString) =>
        val c = typeString.toCharArray()(0)
        anno.annotate(List(typeString -> c), Single(SegmentCon("token")), (blockIndex, charIndex) => {
          typeLabelMapMap.get(blockIndex -> charIndex).flatMap(_.get(typeString))
        })
    } 

  }

}
