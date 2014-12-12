package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.util.IteratorIterable

import annotator.Annotator 

import bibie._  
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Sentence

import cc.factorie.app.nlp.Token


object ReferencePartProcessor extends Processor {
  import Annotator._

  override def process(annotator: Annotator): Annotator =  {

    val modelUri = "file://" + getClass.getResource("/citationCRF.factorie").getPath()
    val lexiconUrlPrefix = "file://" + getClass.getResource("/lexicons").getPath()

    val trainer = TestCitationModel.loadModel(modelUri, lexiconUrlPrefix)

    val refBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("biblio-marker")))

    val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Range(SegmentCon("biblio-marker"), SegmentCon("line")))

    case class DPT(doc: Document, indexPairMap: IntMap[(Int, Int)], tokenLabelMap: Map[(Int, Int), Label])


    val dptSeq = {

      def token2LabelMap(token: Token): IntMap[Label] = {
        if (token.stringStart + 1 == token.stringEnd) {
          IntMap(token.stringStart -> U('t'))
        } else {
          val first = token.stringStart
          val last = token.stringEnd - 1
          (IntMap((token.stringStart + 1 until last).map(_ -> I): _*) + (first -> B('t'))) + (last -> L)
        }
      }

      val dpts = refBIndexPairSet.toSeq.map {
        case (blockBIndex, charBIndex) =>
          val textMap = annotator.getTextMap("biblio-marker")(blockBIndex, charBIndex)
          val indexPairMap = Annotator.mkIndexPairMap(textMap, lineBIndexPairSet) 

          val doc = {

            val text = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)

            val d = new Document(text)
            DeterministicTokenizer.process(d)
            new Sentence(d.asSection, 0, d.tokens.size)
            d.tokens.foreach(t => {
              t.attr += new CitationLabel("", t)
            })
            d
          }

          val indexPair2TokenLabelMap = doc.tokens.flatMap(token2LabelMap(_)).toMap.map {
            case (tokId, label) => indexPairMap(tokId) -> label
          }

          DPT(doc, indexPairMap, indexPair2TokenLabelMap)
      }

      TestCitationModel.process(dpts.map(_.doc).filter(_.tokens.size > 1), trainer, false)
      dpts 
      
    }

    val indexPair2TokenLabelMap = dptSeq.flatMap(_.tokenLabelMap).toMap

    val annoWithTokens = annotator.annotate(List("reference-token" -> 't'), Single(CharCon), (blockIndex, charIndex) => {
      indexPair2TokenLabelMap.get(blockIndex -> charIndex)
    })

    val typePairMap = HashMap(
        "authors" -> ("authors", 'a'), 
        "person" -> ("person", 'p'), 
        "person-first" -> ("first", 'f'), 
        "person-middle" -> ("middle", 'm'), 
        "person-last" -> ("last", 'l'), 
        "date" -> ("date", 'd'), 
        "year" -> ("year", 'y'), 
        "month" -> ("month", 'm'), 
        "title" -> ("reference-title", 't'), 
        "venue" -> ("venue", 'v'), 
        "journal" -> ("journal", 'j'),
        "ref-marker" -> ("ref-marker", 'o'), 
        "volume" -> ("volume", 'z'), 
        "pages" -> ("pages", 'p'),
        "organization" -> ("org", 'o'),
        "booktitle" -> ("booktitle", 'b'),
        "reference_id" -> ("reference_id", 'x'),
        "address" -> ("address", 'a')
    )

    val indexPair2typeLabelMapList = dptSeq.toList.flatMap {
      case DPT(doc, indexPairMap, _) =>
        doc.tokens.map(token => {
          val labelTypeStringList = token.attr[CitationLabel].categoryValue.split(":")
          val indexPair = indexPairMap(token.stringStart)
          val typeLabelMap = labelTypeStringList.filter(!_.isEmpty).flatMap(labelTypeString => {
            val labelString = labelTypeString.take(1)
            val typeKey = labelTypeString.drop(2)

            typePairMap.get(typeKey).map(typePair => {
              val typeString =  typePair._1
              val typeChar = typePair._2

              val label: Label = (labelString match {
                case "B" => B(typeChar)
                case "I" => I
                case "O" => O
              })
              typeString -> label
            })

          }).toMap

          indexPair -> typeLabelMap
        })
    } 

    type IntPair = (Int, Int)
    type StringLabelMap = Map[String, Label]

    def replaceBIWithUL(list: List[(IntPair, StringLabelMap)]): List[(IntPair, StringLabelMap)] = {

      def loop(
          nextLabelMap: StringLabelMap, 
          reverseList: List[(IntPair, StringLabelMap)]
      ): List[(IntPair, StringLabelMap)] = {
        reverseList match {
          case Nil => 
            List()
          case (indexPair, typeLabelMap)::xs =>
            val _nextLabelMap = nextLabelMap ++ typeLabelMap
            val _typeLabelMap = typeLabelMap.map { case (typeString, label) => 
              (label, nextLabelMap(typeString)) match {
                case (I, B(_)) => (typeString -> L)
                case (B(c), B(_)) => (typeString -> U(c))
                case _ => (typeString -> label)
              }
            }
            (indexPair -> _typeLabelMap)::loop(_nextLabelMap, xs)
        }
      }

      val m =  typePairMap.values.map {
        case (typeString, typeChar) => typeString -> B(typeChar)
      } toMap

      loop(m, list.reverse).reverse

    }

    val typeLabelMapMap = replaceBIWithUL(indexPair2typeLabelMapList).toMap

    typePairMap.values.foldLeft(annoWithTokens) {
      case (anno, (annoTypeName, annoTypeAbbrev)) =>
        anno.annotate(List(annoTypeName -> annoTypeAbbrev), Single(SegmentCon("reference-token")), (blockIndex, charIndex) => {
          typeLabelMapMap.get(blockIndex -> charIndex).flatMap(_.get(annoTypeName))
        })
    } 

  }

}
