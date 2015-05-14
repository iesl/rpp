package edu.umass.cs.iesl.rpp

import java.io.File
import org.jdom2.input.SAXBuilder
import edu.umass.cs.iesl.xml_annotator.Annotator
import scala.compat.Platform
import Annotator._
import edu.umass.cs.iesl.bibie._
import edu.umass.cs.iesl.paperheader.tagger.HeaderTagger

object Main {

  def process(trainer: CitationCRFTrainer, headerTagger: HeaderTagger, inFilePath: String): Annotator = {
    val builder = new SAXBuilder()
    val dom = builder.build(new File(inFilePath))

    val l = List(
      LineProcessor,
      StructureProcessor,
      ReferencePartProcessor(trainer),
      CitationProcessor,
      CitationReferenceLinkProcessor,
      HeaderPartProcessor(headerTagger)
    )

    val annotator = l.foldLeft(Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    }

    annotator
  }


  def getCitationsAndRefMarkers(annotator: Annotator): Seq[(String, String)] = {
    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citBlockIndex, citCharIndex) = linkMap("cit")
      val (refMarkerString, refBlockIndex, refCharIndex) = linkMap("ref")

      val citString = annotator.getTextMap(citationString)(citBlockIndex, citCharIndex).values.map(_._2).mkString("")
      val refString = annotator.getTextMap(refMarkerString)(refBlockIndex, refCharIndex).values.map(_._2).mkString("")

      (citString, refString)

    }).toSeq
  }

  def getCitationsAndReferences(annotator: Annotator): Seq[(String, String)] = {

    val bibMarkIndexPairSeq = annotator.getBIndexPairSet(Single(SegmentCon("biblio-marker"))).toIndexedSeq
    val lineBIndexPairSet = annotator.getBIndexPairSet(Range("biblio-marker", SegmentCon("line")))

    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citBlockIndex, citCharIndex) = linkMap("cit")
      val (refMarkerString, refBlockIndex, refCharIndex) = linkMap("ref")

      val citString = annotator.getTextMap(citationString)(citBlockIndex, citCharIndex).values.map(_._2).mkString("")

      def pairLte(p1: (Int, Int), p2: (Int, Int)) = (p1._1 < p2._2) || (p1._1 == p2._1 && p1._2 <= p2._2)
      val bibMarkerIndex = bibMarkIndexPairSeq.lastIndexWhere(p => pairLte(p, (refBlockIndex, refCharIndex)))
      val bibMarkerIndexPair = bibMarkIndexPairSeq(bibMarkerIndex)

      val refString = mkTextWithBreaks(annotator.getTextMap("biblio-marker")(bibMarkerIndexPair._1, bibMarkerIndexPair._2), lineBIndexPairSet, '\n')

      (citString, refString)

    }).toSeq
  }

  def getReferences(annotator: Annotator): Seq[String] = {
    annotator.getTextByAnnotationType("biblio-marker")
  }

  def getReferencesWithBreaks(annotator: Annotator): Seq[String] = {
    val biblioBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("biblio-marker")))
    val lineBIndexPairSet = annotator.getBIndexPairSet(Range("biblio-marker", SegmentCon("line")))
    biblioBIndexPairSet.toList.map { case (blockBIndex, charBIndex) =>
      val textMap = annotator.getTextMap("biblio-marker")(blockBIndex, charBIndex)
      Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)
    }
  }

  def getLinesOfReferences(annotator: Annotator): Seq[String] = {
    //this is possible in such a way because biblio-marker is contrained by line
    annotator.getFilteredTextByAnnotationType("biblio-marker","line")
  }

  def getHeaderLines(annotator: Annotator): Seq[String] = {
    val biblioBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header")))
    val lineBIndexPairSet = annotator.getBIndexPairSet(Range("header", SegmentCon("line")))
    biblioBIndexPairSet.toList.map { case (blockBIndex, charBIndex) =>
      val textMap = annotator.getTextMap("header")(blockBIndex, charBIndex)
      Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet)
    }
  }

  def getCitationAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(tag)))
    val tagTokenBIndexPairSet = annotator.getBIndexPairSet(Range(tag, SegmentCon("reference-token")))
    tagTokenBIndexPairSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
      val (bi, ci) = tokenIndexPair
      val tagToken: String = annotator.getTextMap("reference-token")(bi, ci).values.map(_._2).mkString("")
      if (tagBIndexPairSet.contains(tokenIndexPair)) {
        List(tagToken) :: listAcc
      } else {
        (tagToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }



  def getAnnotatedReferences(annotator: Annotator): List[List[(String, String)]] = {
    val refTags = getAllAnnotationTypes(annotator).filter(t => t.startsWith("ref-"))
    val refTokenBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("reference-token")))
    val allAnnots = new scala.collection.mutable.ArrayBuffer[List[(String, String)]]()
    refTokenBIndexPairSet.toList.foreach {
      case (bi, ci) =>
        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
        val title = annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
        val titlePair: (String, String) = ("ref-title", title)
        annots += titlePair
        allAnnots += annots.toList
    }
//    refTokenBIndexPairSet.toList.foreach {
//      case (bi, ci) =>
//        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
//        val titleSeg = annotator.getSegment("ref-title")(bi, ci)
//
////        val title = titleSeg.toList.flatMap { case (bi, labelMap) =>
////            labelMap.map { case (ci, label) =>
////                annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
////            }
////        }
//        val titlePair: (String, String) = ("ref-title", title.mkString(" "))
//        annots += titlePair
////        val titleTm = annotator.getTextMap("ref-title")(bi, ci)
////        val title: (String, String) = ("ref-title", titleTm.values.map(_._2).mkString(""))
////        annots += title
//        allAnnots += annots.toList
//    }
    allAnnots.toList
  }


  def getHeaderTokens(annotator: Annotator): Seq[Seq[String]] = {
    val headerTokenBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header-token")))
    headerTokenBIndexPairSet.toList.map(pair => {
      val (blockIdx, charIdx) = pair
      val seg = annotator.getSegment("header-token")(blockIdx, charIdx)
      seg.toList.flatMap{ case (bi, labelMap) =>
        labelMap.map{ case (ci, label) =>
          annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
        }
      }
    })
  }

  def getAuthorNames(annotator: Annotator): Seq[Seq[String]] = {
    val authorBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header-author")))

    authorBIndexPairSet.toList.map(bIndexPair => {
      val (blockIndex, charIndex) = bIndexPair
      val authorSegment = annotator.getSegment("header-author")(blockIndex, charIndex)
      authorSegment.toList.flatMap { case (bi, labelMap) =>
        labelMap.map { case (ci, label) =>
          annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
        }
      }
    })

  }

  def getAuthorNames2(annotator: Annotator): Seq[Seq[String]] = {
    val authorBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("header-author")))
    val authorTokenBIndexPairSet = annotator.getBIndexPairSet(Range("header-author", SegmentCon("header-token")))

    authorTokenBIndexPairSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
      val (bi, ci) = tokenIndexPair
      val authorToken: String = annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
      if (authorBIndexPairSet.contains(tokenIndexPair)) {
        List(authorToken) :: listAcc
      } else {
        (authorToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }

  def getLines(annotator: Annotator): Seq[String] = {
    annotator.getTextByAnnotationType("line")
  }


  def getAllAnnotations(annotator: Annotator): Seq[(String, String)] = {
    val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("line")))
    annotator.annotationInfoMap.keys.flatMap(annoTypeString => {
      val bIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(annoTypeString)))
      bIndexPairSet.toList.map {
        case (blockBIndex, charBIndex) =>
          val textMap = annotator.getTextMap(annoTypeString)(blockBIndex, charBIndex)
          (annoTypeString, Annotator.mkTextWithBreaks(textMap, lineBIndexPairSet, ' ').trim())
      }
    }).toSeq
  }


  def getAllAnnotationTypes(annotator: Annotator): Seq[String] = {
    annotator.annotationInfoMap.map { case (annoTypeString, annotationInfo) =>
      annoTypeString
    } toSeq
  }

  def getHeaderAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon(tag)))
    val tagTokenBIndexPairSet = annotator.getBIndexPairSet(Range(tag, SegmentCon("header-token")))
    tagTokenBIndexPairSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
      val (bi, ci) = tokenIndexPair
      val tagToken: String = annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
      if (tagBIndexPairSet.contains(tokenIndexPair)) {
        List(tagToken) :: listAcc
      } else {
        (tagToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }



  def main(args: Array[String]): Unit = {

    val referenceModelUri = args(0)
    val headerTaggerModelFile = args(1)
    val inFilePath = args(2)
    val outFilePath = args(3)

    val lexiconUrlPrefix = "file://" + getClass.getResource("/lexicons").getPath()
    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)

    val headerTagger = new HeaderTagger
    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))

    val annotator = process(trainer, headerTagger, inFilePath).write(outFilePath)
    import HeaderPartProcessor._
    getAuthorTokens(annotator).map(l => println(l))
    getEmails(annotator).map(l => println(l))

  }

}
