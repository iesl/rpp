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
      HeaderPartProcessor(headerTagger)//,
      //ReferencePartProcessor(trainer),
      //CitationProcessor,
      //CitationReferenceLinkProcessor
    )

    val annotator = l.foldLeft(Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    }

    annotator
  }


  def getCitationsAndRefMarkers(annotator: Annotator): Seq[(String, String)] = {
    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citIndex) = linkMap("cit")
      val (refMarkerString, refIndex) = linkMap("ref")

      val citString = annotator.getTextOption(citationString)(citIndex).map(_._2).getOrElse("")
      val refString = annotator.getTextOption(refMarkerString)(refIndex).map(_._2).getOrElse("")
      (citString, refString)

    }).toSeq
  }

  def getCitationsAndReferences(annotator: Annotator): Seq[(String, String)] = {

    val bibMarkIndexSeq = annotator.getBIndexSet(Single(SegmentCon("biblio-marker"))).toIndexedSeq
    val lineBIndexSet = annotator.getBIndexSet(Range("biblio-marker", SegmentCon("line")))

    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").flatMap(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citIndex) = linkMap("cit")
      val (refMarkerString, refIndex) = linkMap("ref")

      val citString = annotator.getTextOption(citationString)(citIndex).map(_._2).getOrElse("")

      val bibMarkerIndex = bibMarkIndexSeq.lastIndexWhere(_ <= refIndex)
      val bibMarkerTotalIndex = bibMarkIndexSeq(bibMarkerIndex)

      annotator.getTextOption("biblio-marker")(bibMarkerTotalIndex).map { case (startIndex, str) =>
        val refString = mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex), '\n')
        (citString, refString)
      }

    }).toSeq
  }

  def getReferences(annotator: Annotator): Seq[String] = {
    annotator.getTextSet("biblio-marker").map(_._2).toSeq
  }

  def getReferencesWithBreaks(annotator: Annotator): Seq[String] = {
    val biblioBIndexSet = annotator.getBIndexSet(Single(SegmentCon("biblio-marker")))
    val lineBIndexSet = annotator.getBIndexSet(Range("biblio-marker", SegmentCon("line")))
    biblioBIndexSet.toList.flatMap { case (index) =>
      annotator.getTextOption("biblio-marker")(index) map { case (startIndex, str) =>
        Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex))
      }
    }
  }


  def getLinesOfReferences(annotator: Annotator): Seq[String] = {
    //this is possible in such a way because biblio-marker is constrained by line
    annotator.getFilteredTextSet("biblio-marker","line").map(_._2).toSeq
  }

  def getHeaderLines(annotator: Annotator): Seq[String] = {
    val biblioBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header")))
    val lineBIndexSet = annotator.getBIndexSet(Range("header", SegmentCon("line")))

    biblioBIndexSet.toList.flatMap { case (index) =>
      annotator.getTextOption("header")(index) map { case (startIndex, str) =>
        Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex))
      }
    }
  }

  def getCitationAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexSet = annotator.getBIndexSet(Single(SegmentCon(tag)))
    val tagTokenBIndexSet = annotator.getBIndexSet(Range(tag, SegmentCon("reference-token")))
    tagTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndex) => {
      val tagToken: String = annotator.getTextOption("reference-token")(tokenIndex).map(_._2).getOrElse("")
      if (tagBIndexSet.contains(tokenIndex)) {
        List(tagToken) :: listAcc
      } else {
        (tagToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }



//  def getAnnotatedReferences(annotator: Annotator): List[List[(String, String)]] = {
//    val refTags = getAllAnnotationTypes(annotator).filter(t => t.startsWith("ref-"))
//    val refTokenBIndexSet = annotator.getBIndexSet(Single(SegmentCon("reference-token")))
//    val allAnnots = new scala.collection.mutable.ArrayBuffer[List[(String, String)]]()
//    refTokenBIndexSet.toList.foreach {
//      case (bi, ci) =>
//        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
//        val title = annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
//        val titlePair: (String, String) = ("ref-title", title)
//        annots += titlePair
//        allAnnots += annots.toList
//    }
////    refTokenBIndexSet.toList.foreach {
////      case (bi, ci) =>
////        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
////        val titleSeg = annotator.getSegment("ref-title")(bi, ci)
////
//////        val title = titleSeg.toList.flatMap { case (bi, labelMap) =>
//////            labelMap.map { case (ci, label) =>
//////                annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
//////            }
//////        }
////        val titlePair: (String, String) = ("ref-title", title.mkString(" "))
////        annots += titlePair
//////        val titleTm = annotator.getTextMap("ref-title")(bi, ci)
//////        val title: (String, String) = ("ref-title", titleTm.values.map(_._2).mkString(""))
//////        annots += title
////        allAnnots += annots.toList
////    }
//    allAnnots.toList
//  }


  //def getHeaderTokens(annotator: Annotator): Seq[Seq[String]] = {
  //  val headerTokenBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header-token")))
  //  headerTokenBIndexSet.toList.map(pair => {
  //    val (blockIdx, charIdx) = pair
  //    val seg = annotator.getSegment("header-token")(blockIdx, charIdx)
  //    seg.toList.flatMap{ case (bi, labelMap) =>
  //      labelMap.map{ case (ci, label) =>
  //        annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
  //      }
  //    }
  //  })
  //}


  def getAuthorNames2(annotator: Annotator): Seq[Seq[String]] = {
    val authorBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header-author")))
    val authorTokenBIndexSet = annotator.getBIndexSet(Range("header-author", SegmentCon("header-token")))

    authorTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
      val authorToken: String = annotator.getTextOption("header-token")(tokenIndexPair).map(_._2).getOrElse("")
      if (authorBIndexSet.contains(tokenIndexPair)) {
        List(authorToken) :: listAcc
      } else {
        (authorToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }

  def getLines(annotator: Annotator): Seq[String] = {
    annotator.getTextSet("line").map(_._2).toSeq
  }


  def getAllAnnotations(annotator: Annotator): Seq[(String, String)] = {
    val lineBIndexSet = annotator.getBIndexSet(Single(SegmentCon("line")))
    annotator.annotationInfoMap.keys.flatMap(annoTypeString => {
      val bIndexPairSet = annotator.getBIndexSet(Single(SegmentCon(annoTypeString)))
      bIndexPairSet.toList.flatMap { case index =>
        annotator.getTextOption(annoTypeString)(index) map { case (startIndex, str) =>
          (annoTypeString, Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex), ' ').trim())
        }
      }
    }).toSeq
  }


  def getAllAnnotationTypes(annotator: Annotator): Seq[String] = {
    annotator.annotationInfoMap.map { case (annoTypeString, annotationInfo) =>
      annoTypeString
    } toSeq
  }

  def getHeaderAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexSet = annotator.getBIndexSet(Single(SegmentCon(tag)))
    val tagTokenBIndexSet = annotator.getBIndexSet(Range(tag, SegmentCon("header-token")))
    tagTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenBIndex) => {
      val tagToken: String = annotator.getTextOption("header-token")(tokenBIndex).map(_._2).getOrElse("")
      if (tagBIndexSet.contains(tokenBIndex)) {
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
    println { annotator.getAnnotationByTypeString(headerAuthor) }
    annotator.getTextSet(headerEmail).map(l => println(l))



  }

}
