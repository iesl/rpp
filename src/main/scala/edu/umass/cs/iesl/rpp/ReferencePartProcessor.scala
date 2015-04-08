package edu.umass.cs.iesl.rpp

import scala.collection.immutable.HashMap

import scala.collection.immutable.IntMap

import edu.umass.cs.iesl.xml_annotator.Annotator

import edu.umass.cs.iesl.bibie._
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Sentence

import cc.factorie.app.nlp.Token
import scala.compat.Platform

object ReferencePartProcessor {

  //annotation types
  val referenceTokenString = "reference-token"
  val referenceTokenChar = 't' 

  val refAuthorsString = "ref-authors"
  val refAuthorsChar = 'a' 

  val refPersonString = "ref-person"
  val refPersonChar = 'p' 

  val refFirstString = "ref-first"
  val refFirstChar = 'f' 

  val refMiddleString = "ref-middle"
  val refMiddleChar = 'm' 

  val refLastString = "ref-last"
  val refLastChar = 'l'

  val refDateString = "ref-date"
  val refDateChar = 'd' 

  val refYearString = "ref-year"
  val refYearChar = 'y' 

  val refMonthString = "ref-month"
  val refMonthChar = 'm' 

  val refTitleString = "ref-title"
  val refTitleChar = 't'
  
  val refVenueString = "ref-venue"
  val refVenueChar = 'v'

  val refJournalString = "ref-journal"
  val refJournalChar = 'j'

  val refMarkerString = "ref-marker"
  val refMarkerChar = 'm'

  val refVolumeString = "ref-volume"
  val refVolumeChar = 'v' 

  val refPagesString = "ref-pages"
  val refPagesChar = 'p'

  val refOrganizationString = "ref-organization"
  val refOrganizationChar = 'o'

  val refBooktitleString = "ref-booktitle"
  val refBooktitleChar = 't' 

  val referenceIdString = "reference_id"
  val referenceIdChar = 'i'

  val refAddressString = "ref-address"
  val refAddressChar = 'r'

  def apply(trainer: CitationCRFTrainer): ReferencePartProcessor = {
    new ReferencePartProcessor(trainer)
  }


}


class ReferencePartProcessor(trainer: CitationCRFTrainer) extends Processor {
  import Annotator._
  import ReferencePartProcessor._

  override def process(annotator: Annotator): Annotator =  {


    val typePairMap = HashMap(
        "authors" -> (refAuthorsString, refAuthorsChar), 
        "person" -> (refPersonString, refPersonChar), 
        "person-first" -> (refFirstString, refFirstChar), 
        "person-middle" -> (refMiddleString, refMiddleChar), 
        "person-last" -> (refLastString, refLastChar), 
        "date" -> (refDateString, refDateChar), 
        "year" -> (refYearString, refYearChar), 
        "month" -> (refMonthString, refMonthChar), 
        "title" -> (refTitleString, refTitleChar), 
        "venue" -> (refVenueString, refVenueChar), 
        "journal" -> (refJournalString, refJournalChar),
        "ref-marker" -> (refMarkerString, refMarkerChar), 
        "volume" -> (refVolumeString, refVolumeChar), 
        "pages" -> (refPagesString, refPagesChar),
        "organization" -> (refOrganizationString, refOrganizationChar),
        "booktitle" -> (refBooktitleString, refBooktitleChar),
        "reference_id" -> (referenceIdString, referenceIdChar),
        "address" -> (refAddressString, refAddressChar)
    )

    val lineString = LineProcessor.lineString
    val biblioMarkerString = StructureProcessor.biblioMarkerString

    val refBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(biblioMarkerString)))

    val lineBIndexPairSet = annotator.getBIndexPairSet(Range(biblioMarkerString, SegmentCon(lineString)))

    case class DPT(doc: Document, indexPairMap: IntMap[(Int, Int)], tokenLabelMap: Map[(Int, Int), Label])


    val dptSeq = {

      def token2LabelMap(token: Token): IntMap[Label] = {
        if (token.stringStart + 1 == token.stringEnd) {
          IntMap(token.stringStart -> U(referenceTokenChar))
        } else {
          val first = token.stringStart
          val last = token.stringEnd - 1
          (IntMap((token.stringStart + 1 until last).map(_ -> I): _*) + (first -> B(referenceTokenChar))) + (last -> L)
        }
      }

      val dpts = refBIndexPairSet.toSeq.map {
        case (blockBIndex, charBIndex) =>
          val textMap = annotator.getTextMap(biblioMarkerString)(blockBIndex, charBIndex)
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

//      TestCitationModel.process(dpts.map(_.doc).filter(_.tokens.size > 1), trainer, false)
      dpts 
      
    }


    val indexPair2TokenLabelMap = dptSeq.flatMap(_.tokenLabelMap).toMap

    val annoWithTokens = annotator.annotate(List(referenceTokenString -> referenceTokenChar), Single(CharCon), indexPair2TokenLabelMap)

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

        val table = typeLabelMapMap.filter(p => {
          val typeLabelMap = p._2
          typeLabelMap.contains(annoTypeName)
        }).mapValues(typeLabelMap => {
          typeLabelMap(annoTypeName)
        })

        anno.annotate(List(annoTypeName -> annoTypeAbbrev), Single(SegmentCon(referenceTokenString)), table)
    } 


  }

}
