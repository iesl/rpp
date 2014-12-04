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
object BodyParaRefAnnotator {

  val logger = Logger(LoggerFactory.getLogger("ScalaTagger"))

  def addAnnotation(annotator: Annotator): Annotator = {


    def buildPipeline(argumentMap: Map[Any, Any]): RxPipelineSvg = {
      val pipeline: RxPipelineSvg = new RxPipelineSvg
      val logp: Boolean = argumentMap.get("enable.log") != null
      pipeline.getScope("session").put("log.boolean", logp)
      pipeline.getScope("session").put("log.directory", new File("./log"))
      pipeline.getScope("session").put("sessionID.integer", new Integer(-1))
      pipeline.addStandardFilters

      pipeline.add(new SegmentationFilterSvg())
      pipeline.add(new BodyExtractionFilterSvg())

      return pipeline
    }

    def readInputDocument(baseFilePath:String): List[Document] =
    {
      val builder = new SAXBuilder()

      def getFileList(baseFilePath:String, pageNumber:Int):List[Document] =
      {
        val newFile:java.io.File = new File(baseFilePath+pageNumber+".svg")
        if(newFile.exists)
        {
          return builder.build(newFile) +: getFileList(baseFilePath, pageNumber+1)

        }
        return List()
      }

      return getFileList(baseFilePath,1)
    }


    def annotateRx(rdoc: RxDocumentSvg): Annotator = {
      val outputPath: String = "~/out.svg"
      val tokenization: NewHtmlTokenizationSvg = rdoc.getTokenization
      val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").getOrElse(null).asInstanceOf[collection.mutable.Map[Any, Any]]
      if (tokenization == null) {
        return null
      } else {
        val referencesTokenization:NewHtmlTokenizationSvg =
              segmentations.get("referencesTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]

        val headerTokenization:NewHtmlTokenizationSvg =
              segmentations.get("headerTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]

        val bodyTokenization:NewHtmlTokenizationSvg =
              segmentations.get("bodyTokenization").get.asInstanceOf[NewHtmlTokenizationSvg]


        val bodyLabels:Sequence =
          segmentations.get("bodyLabels").get.asInstanceOf[Sequence]


        val referenceLabels:Sequence =
          segmentations.get("referenceLabels").get.asInstanceOf[Sequence]

        val annoReference:Annotator =
            annotateV2("reference",'r', referencesTokenization,
              rdoc.getTokenization._annotator)
        val annoRefAndHeader:Annotator =
          annotateV2("header",'h', headerTokenization,
            annoReference)
        val annoRefHeadBody:Annotator =
          annotateV2("body",'b', bodyTokenization,
            annoRefAndHeader)

        val annotatedBody = MetaDataSvgAnnotator.annotateBody(bodyTokenization, bodyLabels, annoRefHeadBody)

        val annotatedReference = MetaDataSvgAnnotator.annotateReferences(referencesTokenization, referenceLabels, annotatedBody)

        return annotatedReference
      }

    }

    def getBlockId(elem:Span):String =
    {
      val realElem:Span = {if(elem.isInstanceOf[CompositeSpan])
          {
            elem.asInstanceOf[CompositeSpan].getSpans(0).asInstanceOf[Span]
          }
          else
          {
            elem
          }
      }
      val blockId = LayoutUtils.getProperty(
          realElem.asInstanceOf[Span], "divElement"
        )
        .asInstanceOf[scala.Tuple2[Any,Any]]._1.toString.toInt

      val pageNum =  LayoutUtils.getProperty(realElem.asInstanceOf[Span], "pageNum")

      return blockId + "_" +  pageNum.asInstanceOf[Double].intValue
    }

    def isPartOfBody(elem:Span):Boolean =
    {
      return !LayoutUtils.isPropertySet(elem, "isHeaderFooterLine") //isActiveFeature(elem,"isHeaderFooterLine");
    }


    def getFirstLabel(lineSpans:mutable.ArrayBuffer[Span], annoLetter:Char):Tuple2[String, Option[Label]] =
    {
      if(lineSpans.size == 1 && isPartOfBody(lineSpans(0)))
      {
        return (getBlockId(lineSpans(0)), Some(U(annoLetter)))
      }
      else if(lineSpans.size > 1 && isPartOfBody(lineSpans(0)))
      {
        return (getBlockId(lineSpans(0)), Some(B(annoLetter)))
      }
      else if(!isPartOfBody(lineSpans(0))) {
        return (getBlockId(lineSpans(0)), Some(O))
      }
      return null
    }

    def getLastLabel(lineSpans:mutable.ArrayBuffer[Span], annoLetter:Char):Tuple2[String, Option[Label]] =
    {
      val last = lineSpans.size-1
      if((lineSpans.size == 1 && isPartOfBody(lineSpans(last))))
      {
        return (getBlockId(lineSpans(last)), Some(U(annoLetter)))
      }
      else if(lineSpans.size > 1 && isPartOfBody(lineSpans(last)))
      {
        return (getBlockId(lineSpans(last)), Some(L))
      }
      else if(!isPartOfBody(lineSpans(last))) {
        return (getBlockId(lineSpans(last)), Some(O))
      }
      return null
    }


    def trimLineSpans(lineSpans: mutable.ArrayBuffer[Span]):ArrayBuffer[Span] =
    {
      def trimStart(lineSpans: mutable.ArrayBuffer[Span]):mutable.ArrayBuffer[Span] =
      {
        if(!isPartOfBody(lineSpans.head))
        {
          return trimStart(lineSpans.tail)
        }
        return lineSpans
      }

      def trimEnd(lineSpans: mutable.ArrayBuffer[Span]):mutable.ArrayBuffer[Span] =
      {
        val reversedList = lineSpans.reverse
        if(!isPartOfBody(reversedList.head))
        {
          return trimEnd(reversedList.tail.reverse)
        }
        return lineSpans
      }
      return trimEnd(trimStart(lineSpans))
    }
    def annotateV2(annotation:String, annoLetter:Char, segmentation:NewHtmlTokenizationSvg,
                 annotator:Annotator):Annotator =
    {
      val origLineSpans = segmentation.getLineSpans
      val lineSpans = trimLineSpans(origLineSpans)

      val firstLineSpan:Span = lineSpans.head
      val lastLineSpan:Span = lineSpans.last
      //:scala.collection.immutable [Tuple2[String, Option[Label]]]
      val resSliding:scala.collection.immutable.::[Tuple2[String, Option[Label]]] =
        lineSpans.iterator.toList.map { x =>
        if(x.getStartIdx == firstLineSpan.getStartIdx)
        {
          (getBlockId(x), Some(B(annoLetter)))
        }
        else if (x.getStartIdx == lastLineSpan.getStartIdx)
        {
          (getBlockId(x), Some(L))
        }
        else if (isPartOfBody(x))
        {
          (getBlockId(x), Some(I))
        }
        else if (!isPartOfBody(x))
        {
          (getBlockId(x), Some(O))
        }
      }.asInstanceOf[scala.collection.immutable.::[Tuple2[String, Option[Label]]]]

      val resSlidingMap = resSliding.toMap

      val resAnnot = { 
        val x = annotator
        x.annotate(List(annotation -> annoLetter), Single(SegmentCon("line")), (blockIndex, charIndex) => {
          if(resSlidingMap.contains(blockIndex + "_" + (1))){
            resSlidingMap(blockIndex + "_" + (1))
          }
          else
          {
            None
          }
        })
      }

      return resAnnot
    }

    def annotate(annotation:String, annoLetter:Char, segmentation:NewHtmlTokenizationSvg,
                  annotator:Annotator):Annotator =
    {
      val lineSpans = segmentation.getLineSpans


      //------------start new sliding code------------
      val slidingLine = lineSpans.iterator.sliding(3)./*withPadding(lineSpans.last).*/toList

      // :Map[Int, Option[Label]]
      val resSliding:scala.collection.immutable.::[Tuple2[String, Option[Label]]]
      =
      {if(lineSpans.size > 2) {slidingLine/*.withPadding(lineSpans.last)*/.map { x =>
//        println(x(0) + " " + isPartOfBody(x(0)).toString.toUpperCase())
//        println(x(1) + " " + isPartOfBody(x(1)).toString.toUpperCase())
//        println(x(2) + " " + isPartOfBody(x(2)).toString.toUpperCase())
        if(!isPartOfBody(x(0)) && isPartOfBody(x(1)) && isPartOfBody(x(2)))
        {
          //begin in x1
          (getBlockId(x(1)), Some(B(annoLetter)))
        }
        else if(isPartOfBody(x(0)) && isPartOfBody(x(1)) && isPartOfBody(x(2)))
        {
          //inside in x1
          (getBlockId(x(1)), Some(I))
        }
        else if(!isPartOfBody(x(0)) && isPartOfBody(x(1)) && !isPartOfBody(x(2)))
        {
          //unique in x1
          (getBlockId(x(1)), Some(U(annoLetter)))
        }

        else if(isPartOfBody(x(1)) && !isPartOfBody(x(2)))
        {
          //last in x1
          (getBlockId(x(1)), Some(L))
        }

        else if(!isPartOfBody(x(1)))
        {
          //out in x1
          (getBlockId(x(1)), Some(O))
        }
      }.asInstanceOf[scala.collection.immutable.::[Tuple2[String, Option[Label]]]]  }
      else {
        /*Map()*/ null
      }}

      val resSlidingMap:Map[String, Option[Label]] = (getFirstLabel(lineSpans,annoLetter) +:
                            resSliding :+
                              getLastLabel(lineSpans,annoLetter)).toMap


      val resAnnot = { 
          val x = annotator
          x.annotate(List(annotation -> annoLetter), Single(SegmentCon("line")), (blockIndex, charIndex) => {
            resSlidingMap(blockIndex + "_" + (1))
//            println((x._2+1) + " on block " + blockIndex)
//            if(x._2+1 == firstPageNum && blockIndex == firstBlock)
//            {
//              println(annotation + " begin " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(B(annoLetter))
//            }
//            else if(x._2+1 == lastPageNum && blockIndex == lastBlock)
//            {
//              println(annotation + " last " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(L)
//            }
//            else if(x._2+1 == lastPageNum && blockIndex < lastBlock ||
//              (x._2+1 == firstPageNum && blockIndex > firstBlock &&
//                  !(x._2+1 == lastPageNum && blockIndex > lastBlock)) ||
//              (x._2+1 > firstPageNum && x._2+1 < lastPageNum ))
//            {
//              println(annotation + " inside " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(I)
//            }
//            else
//            {
//              println(annotation + " none " + x._1.getTextMap("line")(blockIndex, charIndex))
//              None
//            }
          }//this not comment
          )
      }


      return resAnnot
//      val lastPageNum =
//        LayoutUtils.getProperty(segmentation.getLineSpans(segmentation.getLineSpans.size - 1), "pageNum")
//        .toString.toDouble.toInt
//
//      val lastBlock =
//        LayoutUtils.getProperty(segmentation.getLineSpans(segmentation.getLineSpans.size - 1), "divElement")
//        .asInstanceOf[scala.Tuple2[Any,Any]]._1.toString.toInt
//
//      val firstPageNum =
//        LayoutUtils.getProperty(segmentation.getLineSpans(0), "pageNum")
//        .toString.toDouble.toInt
//
//      val firstBlock =
//        LayoutUtils.getProperty(segmentation.getLineSpans(0), "divElement")
//          .asInstanceOf[scala.Tuple2[Any,Any]]._1.toString.toInt
//
//
//      val resAnnot = annotators.zipWithIndex.map { x =>
//        if (x._2+1 >= firstPageNum && x._2+1 <= lastPageNum) {
//          x._1.annotate(List(annotation -> annoLetter), Single(SegmentCon("line")), (blockIndex, charIndex) => {
//            println((x._2+1) + " on block " + blockIndex)
//            if(x._2+1 == firstPageNum && blockIndex == firstBlock)
//            {
//              println(annotation + " begin " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(B(annoLetter))
//            }
//            else if(x._2+1 == lastPageNum && blockIndex == lastBlock)
//            {
//              println(annotation + " last " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(L)
//            }
//            else if(x._2+1 == lastPageNum && blockIndex < lastBlock ||
//              (x._2+1 == firstPageNum && blockIndex > firstBlock &&
//                  !(x._2+1 == lastPageNum && blockIndex > lastBlock)) ||
//              (x._2+1 > firstPageNum && x._2+1 < lastPageNum ))
//            {
//              println(annotation + " inside " + x._1.getTextMap("line")(blockIndex, charIndex))
//              Some(I)
//            }
//            else
//            {
//              println(annotation + " none " + x._1.getTextMap("line")(blockIndex, charIndex))
//              None
//            }
//          }
//          )
//        }
//        else
//        {
//          //if not just return without annotating
//          x._1
//        }
//      }
//      resAnnot
    }


    val dataDir:String = null
    val DICT_FILE:String = getClass.getResource("/words.txt").getPath

    val dictionary: EnglishDictionary = EnglishDictionary.createDefault(new File(dataDir, DICT_FILE))

    val tokenization: NewHtmlTokenizationSvg = NewHtmlTokenizationSvg.createNewHtmlTokenization(annotator, dictionary)
    val rdoc: RxDocumentSvg = new RxDocumentSvg
    rdoc.setTokenization(tokenization)

    val pipeline: RxPipelineSvg = buildPipeline(Map[Any, Any]())

    try {
      pipeline.execute(rdoc)
      logger.info("writing output file")
      val a = annotateRx(rdoc)
      if (a == null) annotator else a
    } catch {
      case e: Exception => {
        e.printStackTrace()
        annotator
      }
    }

  }


}
