package edu.umass.cs.iesl.rpp

import cc.factorie.app.nlp.{Document, Token}
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import edu.umass.cs.iesl.paperheader.tagger._
import edu.umass.cs.iesl.xml_annotator.Annotator

import scala.collection.immutable.{HashMap, IntMap, SortedSet}


class HeaderPartProcessor(val headerTagger: HeaderTagger) extends Processor {
  import edu.umass.cs.iesl.rpp.HeaderPartProcessor._
  import edu.umass.cs.iesl.xml_annotator.Annotator._

  override def process(annotator: Annotator): Annotator =  {

    case class HeaderItem(index: Int, token: Token, x: Int, y: Int, fontSize: Int)

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
    val headerBIndexSet: SortedSet[Int] = annotator.getBIndexSet(Single(SegmentCon("header")))
    val lineBIndexSet: SortedSet[Int] = annotator.getBIndexSet(Range("header", SegmentCon("line")))

    //: (Map[(Int, Int), Label], IndexedSeq[HeaderItem])
    val headerSet: Set[(Map[Int, Label], IndexedSeq[HeaderItem], Document)] = headerBIndexSet.flatMap { case (index) =>
      annotator.getTextOption("header")(index) map { case (startIndex, rawText) =>

        val elementMap: IntMap[Element] = annotator.getElements("header")(index)

        //val indexPairMap: IntMap[(Int, Int)] = Annotator.mkIndexPairMap(textMap, lineBIndexSet)

        val text: String = Annotator.mkTextWithBreaks(rawText, lineBIndexSet.map(_ - startIndex))
        val breakMap = Annotator.mkBreakMap(rawText.size, lineBIndexSet.map(_ - startIndex))

        val doc = {
          val d = new Document(text)
          DeterministicTokenizer.process(d)
          d.tokens.foreach { token => token.attr += new HeaderLabel("I-other", token) }
          headerTagger.process(d)
          d
        }

        val headerItemSeq: IndexedSeq[HeaderItem] = doc.tokens.map(token => {
          val index = breakMap(token.stringStart)
          val indexPair = annotator.mkIndexPair(index)
          val e = elementMap(indexPair._1)
          val PositionGroup(xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
          HeaderItem(
            index,
            token,
            xs(indexPair._2).toInt,
            ys(indexPair._2).toInt,
            Annotator.fontSize(e).toInt
          )
        }).toIndexedSeq

        val index2TokenLabelMap: Map[Int, Label] = headerItemSeq.flatMap(hi => token2LabelMap(hi.token)).map {
          case (i, label) => breakMap(i) -> label
        } toMap

        (index2TokenLabelMap, headerItemSeq, doc)
      }
    }


    val index2TokenLabelMap: Map[Int, Label] = headerSet.flatMap(_._1).toMap

    val annoWithTokens: Annotator = annotator.annotate(List("header-token" -> 't'), Range("header", CharCon), index2TokenLabelMap)

    val docs = headerSet.map(_._3)

//    'abstract',
//    'address',
//    'affiliation',
//    'author',
//    'copyright',
//    'date',
//    'dedication',
//    'degree',
//    'email'
//    'entitle',
//    'grant',
//    'intro',
//    'keyword',
//    'note',
//    'phone',
//    'pubnum',
//    'reference',
//    'submission',
//    'title',
//    'web',

    val typePairMap: HashMap[String, (String, Char)] = HashMap(
      "abstract" -> (headerAbstract, 'b'),
      "address" -> (headerAddress, 'a'),
      "affiliation" -> (headerAffiliation, 'a'),
      "author" -> (headerAuthor, 'a'),
      "date" -> (headerDate, 'd'),
      "email" -> (headerEmail, 'e'),
      "title" -> (headerTitle, 't')

//      "institution" -> (headerInstitution, 'i'),
//      "address" -> (headerAddress, 'a'),
//      "title" -> (headerTitle, 't'),
//      "author" -> (headerAuthor, 'a'),
//      "tech" -> (headerTech, 't'),
//      "date" -> (headerDate, 'd'),
//      "note" -> (headerNote, 'n'),
//      "abstract" -> (headerAbstract, 'b'),
//      "email" -> (headerEmail, 'e')
    )

    val headerSeq: IndexedSeq[IndexedSeq[HeaderItem]] = headerSet.map(_._2).toIndexedSeq

    val indexTypeDub2LabelList: List[((Int, String), Label)] = docs.zipWithIndex.flatMap {
      case (doc, docIdx) =>
        val headerItemSeq: IndexedSeq[HeaderItem] = headerSeq(docIdx)
        val indexMap: IndexedSeq[Int] = headerItemSeq.map(_.index)
        val typeLabelList = headerItemSeq.map(_.token).map(t => {
//          println("paper-header output: " + t.attr[HeaderLabel].categoryValue + ": " + t.toString)
          t.attr[HeaderLabel].categoryValue
        })
        typeLabelList.zipWithIndex.flatMap {
          case (typeLabel, tokenIndex) =>
            val totalIndex = indexMap(tokenIndex)
            val labelString = typeLabel.take(1)
            val typeKey = typeLabel.drop(2)

            typePairMap.get(typeKey).map {
              case (typeString, typeChar) =>
                (totalIndex, typeString) -> (labelString match {
                  case "B" => B(typeChar)
                  case "I" => I
                  case "O" => O
                  case "L" => L
                  case "U" => U(typeChar)
                })
            }
        }
    }.toIndexedSeq.sortBy((f) => f._1._1).toList// toList

    def replaceIWithB(list: List[((Int, String), Label)]): List[((Int, String), Label)] = {
      val name2Char = typePairMap.values.toMap
      def loop(
        prevTypeString: String,
        lst: List[((Int, String), Label)]
      ): List[((Int, String), Label)] = lst match {

        case Nil => List()
        case x::xs =>
          val (xTypeString, xLabel) = x match { case (( _, typeString), label) => (typeString, label) }
          if (xLabel == I && xTypeString != prevTypeString) {
            val c = name2Char(xTypeString)
            (x._1 -> B(c))::loop(xTypeString, xs)
          } else {
            x::loop(xTypeString, xs)
          }

      }
      loop("", list)
    }

    val dubLabelMap = replaceIWithB(indexTypeDub2LabelList).toMap

    typePairMap.values.foldLeft(annoWithTokens) {
      case (anno, (annoTypeName, annoTypeAbbrev)) =>

        val  table = dubLabelMap.filter(p => {
          val key = p._1
          annoTypeName == key._2
        }).map {
          case ((index, _), label) =>
            index -> label
        }

        anno.annotate(List(annoTypeName -> annoTypeAbbrev), Single(SegmentCon("header-token")), table)
    }

  }
}

object HeaderPartProcessor {
  def apply(headerTagger: HeaderTagger): HeaderPartProcessor = new HeaderPartProcessor(headerTagger)

  val headerAbstract = "abstract"
  val headerAddress = "header-address"
  val headerAffiliation = "header-affiliation"
  val headerAuthor = "header-author"
  val headerDate = "header-date"
  val headerEmail = "header-email"
  val headerTitle = "header-title"
  val headerToken = "header-token"

  //  val headerInstitution = "header-institution"
  //  val headerTech = "header-tech"
  //  val headerNote = "header-note"
}

