package edu.umass.cs.iesl.rpp

import org.jdom2.Element

import scala.collection.immutable.HashMap

import scala.collection.immutable.{IntMap, SortedSet}
import scala.io.Source

import edu.umass.cs.iesl.paperheader.tagger._
import edu.umass.cs.iesl.paperheader.load._

import edu.umass.cs.iesl.xml_annotator.Annotator

import cc.factorie.app.nlp.{Document, Sentence}
import cc.factorie.app.nlp.segment.DeterministicTokenizer

import cc.factorie.app.nlp.Token

import scala.compat.Platform


class HeaderPartProcessor(val headerTagger: HeaderTagger) extends Processor {
  import Annotator._
  import HeaderPartProcessor._

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
    val headerSet: Set[(Map[(Int, Int), Label], IndexedSeq[HeaderItem], Document)] = headerBIndexPairSet.map {

      case (blockBIndex, charBIndex) =>
        val textMap: IntMap[(Int, String)] = annotator.getTextMap("header")(blockBIndex, charBIndex)
        val elementMap: IntMap[Element] = annotator.getElements("header")(blockBIndex, charBIndex)

        val indexPairMap: IntMap[(Int, Int)] = Annotator.mkIndexPairMap(textMap, lineBIndexPairSet)


        val text: String = Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)

        val doc = {
          val d = new Document(text)
          DeterministicTokenizer.process(d)
          headerTagger.process(d)
          d
        }

        val headerItemSeq: IndexedSeq[HeaderItem] = doc.tokens.map(token => {
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

        val indexPair2TokenLabelMap: Map[(Int, Int), Label] = headerItemSeq.flatMap(hi => token2LabelMap(hi.token)).map {
          case (i, label) => indexPairMap(i) -> label
        } toMap

        (indexPair2TokenLabelMap, headerItemSeq, doc)

    }


    val indexPair2TokenLabelMap: Map[(Int, Int), Label] = headerSet.flatMap(_._1).toMap

    val annoWithTokens: Annotator = annotator.annotate(List("header-token" -> 't'), Single(CharCon), indexPair2TokenLabelMap)

    val docs = headerSet.map(_._3)

    val typePairMap: HashMap[String, (String, Char)] = HashMap(
      "institution" -> (headerInstitution, 'i'),
      "address" -> (headerAddress, 'a'),
      "title" -> (headerTitle, 't'),
      "author" -> (headerAuthor, 'a'),
      "tech" -> (headerTech, 't'),
      "date" -> (headerDate, 'd'),
      "note" -> (headerNote, 'n'),
      "abstract" -> (headerAbstract, 'b'),
      "email" -> (headerEmail, 'e')
    )

    val headerSeq: IndexedSeq[IndexedSeq[HeaderItem]] = headerSet.map(_._2).toIndexedSeq

    val indexTypeTriple2LabelList: List[((Int, Int, String), Label)] = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq: IndexedSeq[HeaderItem] = headerSeq(docIdx)
        val indexPairMap: IndexedSeq[(Int, Int)] = headerItemSeq.map(_.indexPair)
        val typeLabelList = headerItemSeq.map(_.token).map(t => {
          println("HeaderPartProcessor: " + t.attr[BilouHeaderTag].categoryValue + ": " + t.toString)
          t.attr[BilouHeaderTag].categoryValue
        })
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
                  case "L" => L
                  case "U" => U(typeChar)
                })
            }
        }
    } toList

    type IISTrip = (Int, Int, String)
    type StringLabelMap = Map[String, Label]

    val tripLabelMap = indexTypeTriple2LabelList.toMap

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

  val headerInstitution = "header-institution"
  val headerAddress = "header-address"
  val headerTitle = "header-title"
  val headerAuthor = "header-author"
  val headerTech = "header-tech"
  val headerDate = "header-date"
  val headerNote = "header-note"
  val headerAbstract = "abstract"
  val headerEmail = "header-email"
  val headerToken = "header-token"

  import Annotator._

  def getAuthorTokens(annotator: Annotator): Seq[Seq[String]] = {
    val authorBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(headerAuthor)))
    authorBIndexPairSet.toList.map(bIndexPair => {
      val (blockIndex, charIndex) = bIndexPair
      val authorSegment = annotator.getSegment(headerAuthor)(blockIndex, charIndex)
      authorSegment.toList.flatMap { case (bi, labelMap) =>
        labelMap.map { case (ci, label) =>
          annotator.getTextMap(headerToken)(bi, ci).values.map(_._2).mkString("")
        }
      }
    })
  }

  def getEmails(annotator: Annotator): Seq[String] = {
    val emailBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(headerEmail)))
    emailBIndexPairSet.toList.map(bIndexPair => {
      val (blockIndex, charIndex) = bIndexPair
      annotator.getTextMap(headerEmail)(blockIndex, charIndex).map(_._2).mkString("")
    })
  }

}

