package edu.umass.cs.iesl.rpp

import org.jdom2.Element

import scala.collection.immutable.HashMap

import scala.collection.immutable.{IntMap, SortedSet}
import scala.io.Source

import edu.umass.cs.iesl.paperheader

import edu.umass.cs.iesl.paperheader.crf._

import edu.umass.cs.iesl.xml_annotator.Annotator

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer

import cc.factorie.app.nlp.Token

import scala.compat.Platform


class HeaderPartProcessor(val headerTagger: HeaderTagger) extends Processor {
  import Annotator._

  val headerInstitution = "header-institution"
  val headerAddress = "header-address"
  val headerTitle = "header-title"
  val headerAuthor = "header-author"
  val headerTech = "header-tech"
  val headerDate = "header-date"
  val headerNote = "header-note"
  val headerAbstract = "abstract"
  val headerEmail = "header-email"

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

    // "root" of the XML file
    val rootElement = annotator.getDom().getRootElement

    //create index from (section idx) -> (bIndex, cIndex)
    val headerBIndexPairSet: SortedSet[(Int, Int)] = annotator.getBIndexPairSet(Single(SegmentCon("header")))
    val lineBIndexPairSet: SortedSet[(Int, Int)] = annotator.getBIndexPairSet(Range("header", SegmentCon("line")))

    //: (Map[(Int, Int), Label], IndexedSeq[HeaderItem])
    val headerSet: Set[(Map[(Int, Int), Label], IndexedSeq[HeaderItem])] = headerBIndexPairSet.map {

      case (blockBIndex, charBIndex) =>
        val textMap: IntMap[(Int, String)] = annotator.getTextMap("header")(blockBIndex, charBIndex)
        val elementMap: IntMap[Element] = annotator.getElements("header")(blockBIndex, charBIndex)

        val indexPairMap: IntMap[(Int, Int)] = Annotator.mkIndexPairMap(textMap, lineBIndexPairSet)

        val headerItemSeq: IndexedSeq[HeaderItem] = {
          val text: String = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)
          val d = new Document(text)
          DeterministicTokenizer.process(d)
          headerTagger.process(d)
          d.tokens.map(token => {
            val indexPair = indexPairMap(token.stringStart)
            val e = elementMap(indexPair._1)
            val PositionGroup(xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
            HeaderItem(
              indexPair,
              token,
              xs(indexPair._2).toInt,
              ys(indexPair._2).toInt,
              Annotator.fontSize(e).toInt
            )
          }).toIndexedSeq
        }

        val indexPair2TokenLabelMap: Map[(Int, Int), Label] = headerItemSeq.flatMap(hi =>
          token2LabelMap(hi.token)).map {
          case (i, label) => indexPairMap(i) -> label
        } toMap

        (indexPair2TokenLabelMap, headerItemSeq)

    }


    val indexPair2TokenLabelMap: Map[(Int, Int), Label] = headerSet.flatMap(_._1).toMap

    val annoWithTokens: Annotator = annotator.annotate(List("header-token" -> 't'), Single(CharCon), indexPair2TokenLabelMap)
    val separator = "{{-^--^-}}"

    val str: String = (headerSet.map { case (_, headerItemSeq) => {
      separator + "\n" + (headerItemSeq.map {
        case HeaderItem(_, token, x, y, fontSize) =>
          token.string //+ "\t" + x + "\t" + y + "\t" + fontSize
      }).mkString("\n")
    }}).mkString("\n\n") + "\n\n" + separator

    val docs = {
      //TODO: kzaporojets: undo it
      val ds = new LoadTSV(withLabels=false).fromSource(Source.fromString(str), separator).toIndexedSeq
      ds.foreach(headerTagger.process)
      ds.toIndexedSeq
    }

    val typePairMap: HashMap[String, (String, Char)] = HashMap(
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

    val headerSeq: IndexedSeq[IndexedSeq[HeaderItem]] = headerSet.map(_._2).toIndexedSeq

    val indexTypeTriple2LabelList: List[((Int, Int, String), Label)] = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq: IndexedSeq[HeaderItem] = headerSeq(docIdx)
        val indexPairMap: IndexedSeq[(Int, Int)] = headerItemSeq.map(_.indexPair)
        val typeLabelList = headerItemSeq.map(_.token).map(_.attr[BioHeaderTag].categoryValue)
        typeLabelList.zipWithIndex.flatMap {
          case (typeLabel, tokenIndex) =>
            val (bIndex, cIndex) = indexPairMap(tokenIndex)
            val labelString = typeLabel.take(1)
            val typeKey = typeLabel.drop(2)
            typePairMap.get(typeKey).map {
              case (typeString, typeChar) =>
                (bIndex, cIndex, typeString) -> (labelString match {
                  case "B" => B(typeChar)
                  case "I" => I
                  case "O" => O
                })
            }
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

object HeaderPartProcessor {
  def apply(headerTagger: HeaderTagger): HeaderPartProcessor = new HeaderPartProcessor(headerTagger)
}
