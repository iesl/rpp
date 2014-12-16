package rpp 

import org.rexo.util.EnglishDictionary
import java.io._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
import org.rexo.pipeline.components.{ReferenceExtractionFilter, RxPipeline, RxDocument}

import org.rexo.pipeline.components.svg.{RxDocumentSvg, RxPipelineSvg}
import edu.umass.cs.rexo.ghuang.segmentation.svg.SegmentationFilterSvg
import org.rexo.pipeline.svg.{BodyExtractionFilterSvg, ReferenceExtractionFilterSvg}
import scala.Some
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils

import org.jdom2.Document
import org.jdom2.input.SAXBuilder
import org.rexo.extra.extract.Span
import scala.collection.mutable
import org.rexo.span.CompositeSpan
import scala.collection.mutable.ArrayBuffer
import org.rexo.pipeline.BodyExtractionFilter
import org.rexo.extra.types.Sequence
import org.rexo.store.MetaDataSvgAnnotator

import annotator.Annotator

import Annotator._

//based onScalaTaggerSvg in rexa-scalatagger
object StructureProcessor extends Processor {

  val logger = Logger(LoggerFactory.getLogger("ScalaTagger"))

  override def process(annotator: Annotator): Annotator = {

    val rdoc = TaggerUtil.mkRDoc(annotator)
    val pipeline: RxPipelineSvg = TaggerUtil.buildPipeline(Map[Any, Any]())

    try {
      pipeline.execute(rdoc)
      logger.info("writing output file")
      val a = TaggerUtil.annotateRx(rdoc)
      if (a == null) annotator else a
    } catch {
      case e: Exception => {
        e.printStackTrace()
        annotator
      }
    }

  }

}

object TaggerUtil {

  def mkRDoc(annotator: Annotator) = {
    val dataDir:String = null
    val DICT_FILE:String = getClass.getResource("/words.txt").getPath

    val dictionary: EnglishDictionary = EnglishDictionary.createDefault(new File(dataDir, DICT_FILE))

    val tokenization: NewHtmlTokenizationSvg = NewHtmlTokenizationSvg.createNewHtmlTokenization(annotator, dictionary)
    val rdoc: RxDocumentSvg = new RxDocumentSvg
    rdoc.setTokenization(tokenization)
    rdoc
  }

  def buildPipeline(argumentMap: Map[Any, Any]): RxPipelineSvg = {
    val pipeline: RxPipelineSvg = new RxPipelineSvg
    val logp: Boolean = argumentMap.get("enable.log") != null
    pipeline.getScope("session").put("log.boolean", logp)
    pipeline.getScope("session").put("log.directory", new File("./log"))
    pipeline.getScope("session").put("sessionID.integer", new Integer(-1))
    pipeline.addStandardFilters

    pipeline.add(new SegmentationFilterSvg())
    pipeline.add(new BodyExtractionFilterSvg())

    pipeline
  }

  def annotateRx(rdoc: RxDocumentSvg): Annotator = {
    val tokenization: NewHtmlTokenizationSvg = rdoc.getTokenization
    val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").getOrElse(null).asInstanceOf[collection.mutable.Map[Any, Any]]
    if (tokenization == null) {
      null
    } else {

      val referencesTokenization:NewHtmlTokenizationSvg = segmentations.get("referencesTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]
      val headerTokenization:NewHtmlTokenizationSvg = segmentations.get("headerTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]
      val bodyTokenization:NewHtmlTokenizationSvg = segmentations.get("bodyTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]

      val bodyLabels:Sequence = segmentations.get("bodyLabels").get.asInstanceOf[Sequence]
      val referenceLabels:Sequence = segmentations.get("referenceLabels").get.asInstanceOf[Sequence]

      val annoReference:Annotator = annotateV2("reference",'r', referencesTokenization, rdoc.getTokenization._annotator)
      val annoRefAndHeader:Annotator = annotateV2("header",'h', headerTokenization, annoReference)
      val annoRefHeadBody:Annotator = annotateV2("body",'b', bodyTokenization, annoRefAndHeader)
      val annotatedBody = MetaDataSvgAnnotator.annotateBody(bodyTokenization, bodyLabels, annoRefHeadBody)
      val annotatedReference = MetaDataSvgAnnotator.annotateReferences(referencesTokenization, referenceLabels, annotatedBody)
      annotatedReference

    }

  }

  private def getBlockId(elem:Span):String = {
    val realElem:Span = {
      if (elem.isInstanceOf[CompositeSpan]) {
        elem.asInstanceOf[CompositeSpan].getSpans(0).asInstanceOf[Span]
      } else {
        elem
      }
    }

    val blockId = LayoutUtils
      .getProperty(realElem.asInstanceOf[Span], "divElement")
      .asInstanceOf[scala.Tuple2[Any,Any]]._1.toString.toInt

    val pageNum =  LayoutUtils.getProperty(realElem.asInstanceOf[Span], "pageNum")

    blockId + "_" +  pageNum.asInstanceOf[Double].intValue
  }

  private def isPartOfBody(elem:Span):Boolean = {
    !LayoutUtils.isPropertySet(elem, "isHeaderFooterLine") //isActiveFeature(elem,"isHeaderFooterLine");
  }

  private def trimLineSpans(lineSpans: mutable.ArrayBuffer[Span]): ArrayBuffer[Span] = {

    def trimStart(lineSpans: mutable.ArrayBuffer[Span]): mutable.ArrayBuffer[Span] = {
      if (!isPartOfBody(lineSpans.head)) {
        trimStart(lineSpans.tail)
      } else {
        lineSpans
      }
    }

    def trimEnd(lineSpans: mutable.ArrayBuffer[Span]): mutable.ArrayBuffer[Span] = {
      val reversedList = lineSpans.reverse

      if (!isPartOfBody(reversedList.head)) {
        trimEnd(reversedList.tail.reverse)
      } else {
        lineSpans
      }

    }

    trimEnd(trimStart(lineSpans))
  }

  def annotateV2(
      annotation: String, 
      annoLetter: Char, 
      segmentation: NewHtmlTokenizationSvg,
      annotator: Annotator
  ): Annotator = {
    val origLineSpans = segmentation.getLineSpans
    val lineSpans = trimLineSpans(origLineSpans)

    val firstLineSpan:Span = lineSpans.head
    val lastLineSpan:Span = lineSpans.last
    //:scala.collection.immutable [Tuple2[String, Option[Label]]]
    val resSliding: scala.collection.immutable.::[Tuple2[String, Option[Label]]] =
      lineSpans.iterator.toList.map { x =>
      if(x.getStartIdx == firstLineSpan.getStartIdx) {
        (getBlockId(x), Some(B(annoLetter)))
      } else if (x.getStartIdx == lastLineSpan.getStartIdx) {
        (getBlockId(x), Some(L))
      } else if (isPartOfBody(x)) {
        (getBlockId(x), Some(I))
      } else if (!isPartOfBody(x)) {
        (getBlockId(x), Some(O))
      }
    }.asInstanceOf[scala.collection.immutable.::[Tuple2[String, Option[Label]]]]

    val resSlidingMap = resSliding.toMap

    val resAnnot = { 
      val table = resSlidingMap.flatMap {
        case (blockIndexStr, labelOp) =>
          val blockIndex = blockIndexStr.dropRight(2).toInt
          labelOp.map(l => (blockIndex, 0) -> l)
      }
      annotator.annotate(List(annotation -> annoLetter), Single(SegmentCon("line")), table)
    }

    resAnnot
  }

}
