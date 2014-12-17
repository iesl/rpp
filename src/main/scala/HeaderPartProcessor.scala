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

    case class HeaderItem(indexPair: (Int, Int), token: Token, x: Int, y: Int, fontSize: Int)

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
    val lineBIndexPairSet = annotator.getAnnotatableIndexPairSet(Range("header", SegmentCon("line")))

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
            val indexPair = indexPairMap(token.stringStart)
            val e = elementMap(indexPair._1)
            val (xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
            HeaderItem(
              indexPair,
              token,
              xs(indexPair._2).toInt,
              ys(indexPair._2).toInt,
              Annotator.fontSize(e).toInt
            )
          }).toIndexedSeq
        }

        val indexPair2TokenLabelMap = headerItemSeq.flatMap(hi => token2LabelMap(hi.token)).map {
          case (i, label) => indexPairMap(i) -> label
        } toMap

        (indexPair2TokenLabelMap, headerItemSeq)

    }


    val indexPair2TokenLabelMap = headerSet.flatMap(_._1).toMap

    val annoWithTokens = annotator.annotate(List("header-token" -> 't'), Single(CharCon), indexPair2TokenLabelMap)

    val str = (headerSet.map { case (_, headerItemSeq) => {
      "#\n" + (headerItemSeq.map {
        case HeaderItem(_, token, x, y, fontSize) =>
          token.string + "\t" + x + "\t" + y + "\t" + fontSize
      }).mkString("\n")
    }}).mkString("\n\n") + "\n\n#"

    val docs = {
      val ds = (new LoadTSV(false)).fromSource(Source.fromString(str)).toIndexedSeq
      paperheader.process.DocProcessor(ds)
      ds.toIndexedSeq
    } 


    val typePairMap = HashMap(
        "institution" -> ("header-institution", 'i'), 
        "address" -> ("header-address", 'a'), 
        "title" -> ("header-title", 't'), 
        "author" -> ("header-author", 'a'),
        "tech" -> ("header-tech", 't'), 
        "date" -> ("header-date", 'd'), 
        "note" -> ("header-note", 'n'), 
        "abstract" -> ("abstract", 'b'), 
        "email" -> ("header-email", 'e')
    )


    val headerSeq = headerSet.map(_._2).toIndexedSeq

    val indexTypeTriple2LabelList = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq = headerSeq(docIdx)
        val indexPairMap = headerItemSeq.map(_.indexPair)
        val typeLabelList = doc.sections.flatMap(_.tokens).map(_.attr[BioHeaderTag].categoryValue)

        typeLabelList.toList.zipWithIndex.flatMap {
          case (typeLabel, lsIdx) =>
            val (bIndex, cIndex) = indexPairMap(lsIdx)
            val labelString = typeLabel.take(1)
            val typeKey = typeLabel.drop(2)

            typePairMap.get(typeKey).map(typePair => {

              val typeString =  typePair._1
              val typeChar = typePair._2

              (bIndex, cIndex, typeString) -> (labelString match {
                case "B" => B(typeChar)
                case "I" => I
                case "O" => O
              })

            })


        }
    } toList

    type IISTrip = (Int, Int, String)
    type StringLabelMap = Map[String, Label]

    def replaceIWithB(list: List[(IISTrip, Label)]): List[(IISTrip, Label)] = {
      val name2Char = typePairMap.values.toMap
      def loop(
          prevTypeString: String, 
          lst: List[(IISTrip, Label)]
      ): List[(IISTrip, Label)] = lst match {

        case Nil => List()
        case x::xs => 
          val (xTypeString, xLabel) = x match { case ((_, _, typeString), label) => (typeString, label) }
          if (xLabel == I && xTypeString != prevTypeString) {
            val c = name2Char(xTypeString)
            (x._1 -> B(c))::loop(xTypeString, xs)
          } else {
            x::loop(xTypeString, xs)
          }
      
      }
      loop("", list)
    }


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
            val _label = (label, nextLabelMap(typeString)) match {
              case (I, B(_)) => L
              case (B(typeChar), B(_)) => U(typeChar)
              case _ => label
            }
            (triple -> _label)::loop(_nextLabelMap, xs)
        }

      }

      val m =  typePairMap.values.map {
        case (typeString, typeChar) => typeString -> B(typeChar)
      } toMap

      loop(m, list.reverse).reverse
    }

    val tripLabelMap = replaceBIWithUL(replaceIWithB(indexTypeTriple2LabelList)).toMap

    typePairMap.values.foldLeft(annoWithTokens) {
      case (anno, (annoTypeName, annoTypeAbbrev)) =>

        val  table = tripLabelMap.filter(p => {
          val key = p._1
          annoTypeName == key._3
        }).map {
          case ((blockIndex, charIndex, _), label) =>
            (blockIndex, charIndex) -> label
        }

        anno.annotate(List(annoTypeName -> annoTypeAbbrev), Single(SegmentCon("header-token")), table) 
    } 

  }

}
