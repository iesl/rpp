package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import scala.io.Source
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.util.IteratorIterable

import edu.umass.cs.iesl.paperheader
import edu.umass.cs.iesl.paperheader.crf

import edu.umass.cs.iesl.paperheader.crf.BioHeaderTag
import edu.umass.cs.iesl.paperheader.crf.LoadTSV

import annotator.Annotator 

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Sentence

import cc.factorie.app.nlp.Token

object HeaderPartProcessor extends Processor {
  import Annotator._

  override def process(annotator: Annotator): Annotator =  {

    case class HeaderItem(pairIndex: (Int, Int), token: Token, x: Int, y: Int, fontSize: Int)

    def token2LabelMap(token: Token): IntMap[Label] = {
      if (token.stringStart + 1 == token.stringEnd) {
        IntMap(token.stringStart -> U('t'))
      } else {
        val first = token.stringStart
        val last = token.stringEnd - 1
        IntMap((token.stringStart + 1 until last).map(_ -> I): _*) + (first -> B('t')) + (last -> L)
      }
    }

    val rootElement = annotator.getDom().getRootElement()

    //create index from (section idx) -> (bIndex, cIndex)
    val headerBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("header")))
    val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Range(SegmentCon("header"), SegmentCon("line")))

    val headerSet = headerBIndexPairSet.map {

      case (blockBIndex, charBIndex) =>
        val textMap = annotator.getTextMap("header")(blockBIndex, charBIndex)
        val elementMap = annotator.getElements("header")(blockBIndex, charBIndex)

        val indexPairMap = Annotator.mkIndexPairMap(textMap, lineBIndexPairSet) 

        val headerItemSeq = {
          val text = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)
          val d = new Document(text)
          DeterministicTokenizer.process(d)
          d.tokens.map(token => {
            val pairIndex = indexPairMap(token.stringStart)
            val e = elementMap(pairIndex._1)
            val (xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
            HeaderItem(
              pairIndex,
              token,
              xs(pairIndex._2).toInt,
              ys(pairIndex._2).toInt,
              Annotator.fontSize(e).toInt
            )
          }).toIndexedSeq
        }

        val pairIndex2TokenLabelMap = headerItemSeq.flatMap(hi => token2LabelMap(hi.token)).map {
          case (i, label) => indexPairMap(i) -> label
        } toMap

        (pairIndex2TokenLabelMap, headerItemSeq)

    }


    val pairIndex2TokenLabelMap = headerSet.flatMap(_._1).toMap

    val annoWithTokens = annotator.annotate(List("header-token" -> 't'), Single(CharCon), (blockIndex, charIndex) => {
      pairIndex2TokenLabelMap.get(blockIndex -> charIndex)
    })

    val str = (headerSet.map { case (_, headerItemSeq) => {
      "#\n" + (headerItemSeq.map {
        case HeaderItem(_, token, x, y, fontSize) =>
          token + "\t" + x + "\t" + y + "\t" + fontSize
      }).mkString("\n")
    }}).mkString("\n\n") + "\n\n#"


    val docs = {
      val ds = (new LoadTSV(false)).fromSource(Source.fromString(str)).toIndexedSeq
      paperheader.process.DocProcessor(ds)
      ds.toIndexedSeq
    } 


    val typeStringMap = HashMap(
        "institution" -> ("institution", 'i'), 
        "address" -> ("address", 'a'), 
        "title" -> ("title", 't'), 
        "author" -> ("author", 'a'),
        "tech" -> ("tech", 't'), 
        "date" -> ("date", 'd'), 
        "note" -> ("note", 'n'), 
        "abstract" -> ("abstract", 'b'), 
        "email" -> ("email", 'e')
    )


    val headerSeq = headerSet.map(_._2).toIndexedSeq

    val indexTypeTriple2LabelList = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq = headerSeq(docIdx)
        val indexPairMap = headerItemSeq.map(_.pairIndex)
        val typeLabelList = doc.sections.flatMap(_.tokens).map(_.attr[BioHeaderTag].categoryValue)

        typeLabelList.toList.zipWithIndex.flatMap {
          case (typeLabel, lsIdx) =>
            val (bIndex, cIndex) = indexPairMap(lsIdx)
            val labelString = typeLabel.take(1)
            val typeString = typeLabel.drop(2)

            typeStringMap.get(typeString).map(_typePair => {

              val _typeString =  _typePair._1
              val _typeChar = _typePair._2

              (bIndex, cIndex, _typeString) -> (labelString match {
                case "B" => B(_typeChar)
                case "I" => I
                case "O" => O
              })

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


    typeStringMap.values.foldLeft(annoWithTokens) {
      case (anno, (annoTypeName, annoTypeAbbrev)) =>
        anno.annotate(List(annoTypeName -> annoTypeAbbrev), Single(SegmentCon("header-token")), (blockIndex, charIndex) => {
          tripLabelMap.get((blockIndex, charIndex, annoTypeName))
        })
    } 

  }

}
