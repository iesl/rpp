package org.rexo.store

import annotator.Annotator
import annotator.Annotator._
import annotator.Annotator.SegmentCon
import annotator.Annotator.Single
import org.rexo.extraction.NewHtmlTokenizationSvg
import org.rexo.extra.types.{Token, Sequence}
import scala.collection.mutable.ArrayBuffer
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils
import org.rexo.extra.extract.Span
import scala.Tuple3
import scala.Some
import scala.Tuple2

/**
 * Created by klimzaporojets on 11/24/14.
 */
object MetaDataSvgAnnotator {
  val bodyAnnotations:Map[String, Tuple2[String,Char]] =
    Map("paragraph"->("paragraph", 'p'), "section-marker"->("section-marker", 's'),
              "table-marker"->("table-marker", 't'), "figure-marker"->("figure-marker", 'f'));

  val referenceAnnotations:Map[String, Tuple2[String,Char]] =
    Map("biblio-"-> ("biblio-marker", 'b'));


  val referenceLabels:Map[String,String] = Map("inside" -> "-I", "begin" -> "-B")
  val bodyLabels:Map[String,String] = Map("inside" -> "-inside", "begin" -> "-begin")

  private def getAnnotations(annoData:Tuple3[String, Tuple2[String,Char], Map[String, String]], annotator:Annotator,
                              labelNames:Map[String,String]):Annotator =
  {
    val resAnnot = { 
      val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("line")))
      val table = lineBIndexPairSet.flatMap(indexPair => {
        val blockIndex = indexPair._1
        val charIndex = indexPair._2
        val elements = annotator.getElements("line")(blockIndex, charIndex)

        val annotatorPage =  NewHtmlTokenizationSvg.getPageNumber(elements.get(blockIndex).get, annotator.getDom()) + 1

        if(annoData._3.contains(blockIndex + "_" + annotatorPage)){
          val valLabel = annoData._3.get(blockIndex + "_" + annotatorPage).get
//          if(valLabel.contains("-inside"))
          if(valLabel.contains(labelNames.get("inside").get))
          {
            Some(indexPair -> I)
          }
//          else if (valLabel.contains("-begin"))
          else if (valLabel.contains(labelNames.get("begin").get))
          {
            Some(indexPair -> B(annoData._2._2))
          }
          else if (valLabel.contains("last"))
          {
            Some(indexPair -> L)
          }
          else if (valLabel.contains("unique"))
          {
            Some(indexPair -> U(annoData._2._2))
          }
          else if (valLabel.contains("other"))
          {
            Some(indexPair -> O)
          }
          else {
            throw new Exception("Error in svg annotator, no associated label found")
          }
        }
        else
        {
          None
        }
      }).toMap
      annotator.annotate(List(annoData._2._1 -> annoData._2._2), Single(SegmentCon("line")), table)
    }
    return resAnnot
  }

  private def getParagraphIds(lines:List[Tuple2[String,String]], paragraphId:Int, beginLabel:String):List[Tuple3[Int,String,String]] =
  {
    val current =
    {if(lines.head._2.contains(beginLabel))
    //if(lines.head._2.contains("-begin"))
    {
      paragraphId+1
    }
    else
    {
      paragraphId
    }
    }

    if(lines.size == 1)
    {
      return List((current, lines.head._1, lines.head._2))
    }
    else
    {
      return (current, lines.head._1, lines.head._2) +: getParagraphIds(lines.tail, current,beginLabel)
    }
  }

  private def trimOthersInList(listTags:List[Tuple3[Int,String,String]]):List[Tuple3[Int,String,String]] =
  {
    if(listTags.size==0){
      return listTags}

    val rest = {if(listTags.size>1)
    {
      listTags.tail
    }else{List()}}

    val head = listTags.head
    if(head._3.equals("other"))
    {
      return trimOthersInList(rest)
    }
    else
    {
      return listTags
    }
  }

  private def trimOthers(parIdsGrouped:Map[Int,List[Tuple3[Int,String,String]]]):Map[Int,List[Tuple3[Int,String,String]]] =
  {
    val trimmed = parIdsGrouped.map{entry=>
      (entry._1 -> trimOthersInList(entry._2.reverse).reverse)
    }
    return trimmed
  }

  private def hasSameLine(token1:Token, token2:Token):Boolean =
  {
    return token1.getNumericProperty("lineNum").asInstanceOf[Int] ==
              token2.getNumericProperty("lineNum").asInstanceOf[Int]
  }

  //ORIGINAL RECURSIVE getSegmentRawType
  //private def getSegmentRawType(tokens:ArrayBuffer[Token], labelId:Int, labels:Sequence, perLine:Boolean):Map[String,String]= {
  //  val currentToken:Token = tokens.head
  //  val tokenBlockId:Int = currentToken.getProperty("divElement").asInstanceOf[scala.Tuple2[Any, Any]]._1.toString.toInt
  //  val pageNumber:Int = currentToken.getProperty("pageNum").asInstanceOf[Double].toInt
  //  if (tokens.size>1) {
  //    val tokTail = tokens.tail
  //    if(perLine && hasSameLine(currentToken, tokTail.head)){
  //      return Map((tokenBlockId + "_" + pageNumber).toString -> labels.get(labelId).toString) ++ (getSegmentRawType(tokTail, labelId, labels, perLine))
  //    } else {
  //      return Map((tokenBlockId + "_" + pageNumber).toString -> labels.get(labelId).toString) ++ (getSegmentRawType(tokTail, labelId+1, labels, perLine))
  //    }
  //  } else {
  //    return Map((tokenBlockId + "_" + pageNumber).toString -> labels.get(labelId).toString)
  //  }
  //}

  //TAIL RECURSIVE getSegmentRawType
  private def getSegmentRawType(tokens:ArrayBuffer[Token], labelId:Int, labels:Sequence, perLine:Boolean): Map[String,String] = {
    require(!tokens.isEmpty)

    def loop(mapAcc: Map[String, String])(toks: ArrayBuffer[Token], lId: Int): Map[String, String] = {

      val currentToken:Token = toks.head
      val tokenBlockId:Int = currentToken.getProperty("divElement").asInstanceOf[scala.Tuple2[Any, Any]]._1.toString.toInt
      val pageNumber:Int = currentToken.getProperty("pageNum").asInstanceOf[Double].toInt
      val tokTail = toks.tail
      val pair = (tokenBlockId + "_" + pageNumber).toString -> labels.get(lId).toString

      if (toks.size > 1) {
        if(perLine && hasSameLine(currentToken, tokTail.head)){
          loop(mapAcc + pair)(tokTail, lId)
        } else {
          loop(mapAcc + pair)(tokTail, lId + 1)
        }
      } else {
        mapAcc + pair
      }

    }

    loop(Map[String, String]())(tokens, labelId)

  }

  private def insertLast(listTags:List[Tuple3[Int,String,String]]):List[Tuple3[Int,String,String]] =
  {
    if(listTags.size==0)
    {
      return listTags
    }
    if(listTags.size==1)
    {
      return List((listTags(0)._1,listTags(0)._2, "unique"))
    }
    val last = listTags.reverse.head
    val prev_last = listTags.reverse.tail.reverse
    return (prev_last :+ (last._1, last._2, "last"))
  }

  def annotateRec(annotations:List[Tuple3[String, Tuple2[String,Char], Map[String, String]]], annotator:Annotator, labelNames:Map[String,String]):Annotator =
  {
    if(annotations.size==1)
    {
      val annoData:Tuple3[String, Tuple2[String,Char], Map[String, String]] = annotations(0)
      val resAnnot = getAnnotations(annoData,annotator,labelNames)
      return resAnnot
    }
    else
    {
      val headAnno = annotations.head
      val resAnnot = getAnnotations(headAnno,annotateRec(annotations.tail, annotator,labelNames),labelNames)
      return resAnnot
    }
  }

  def annotateReferences(segmentation: NewHtmlTokenizationSvg, labels:Sequence, annotator:Annotator): Annotator =
  {
    val bIndexRaw = getSegmentRawType(segmentation.tokens, 0, labels, true);

    val annotations = referenceAnnotations.map{anno=>
      val annotation = anno._1 //print(anno._1)

      val blockIdxToRawType:List[Tuple2[String, String]] = {

        val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("line")))
        lineBIndexPairSet.toList.map {
          case (blockIndex, charIndex) =>
            val elements = annotator.getElements("line")(blockIndex, charIndex)

            val annotatorPage =  NewHtmlTokenizationSvg.getPageNumber(elements.get(blockIndex).get, annotator.getDom()) + 1
            if(bIndexRaw.contains(blockIndex+"_"+annotatorPage) &&
              bIndexRaw.get(blockIndex+"_"+annotatorPage).get.contains(annotation) )
            {
              (blockIndex+"_"+annotatorPage, bIndexRaw.get(blockIndex+"_"+annotatorPage).get)
            }
            else
            {
              (blockIndex+"_"+annotatorPage, "other")
            }
        }.toList
      }.asInstanceOf[List[Tuple2[String, String]]]

      val parIds:List[Tuple3[Int,String,String]] = getParagraphIds(blockIdxToRawType, 0, "-B")

      val parIdsGrouped:Map[Int,List[Tuple3[Int,String,String]]] = parIds.groupBy(_._1)

      var parIdsTrimmed = trimOthers(parIdsGrouped)

      var lastInserted = parIdsTrimmed.map{entry=>
        (entry._1 -> insertLast(entry._2))
      }

      var hashAnnotations:Map[String,String] = lastInserted.values.map{value=>
        value.map{listTags=>
          (listTags._2 -> listTags._3)
        }
      }.flatten.toMap

      (anno._1, anno._2, hashAnnotations)
    }

    return annotateRec(annotations.toList, annotator, referenceLabels)
  }

  def annotateBody(segmentation: NewHtmlTokenizationSvg, labels:Sequence, annotator: Annotator): Annotator =
  {
    val bIndexRaw = getSegmentRawType(segmentation.tokens, 0, labels, false);

    val annotations = bodyAnnotations.map{anno=>
      val annotation = anno._1 //print(anno._1)

      val blockIdxToRawType:List[Tuple2[String, String]] = {

        val lineBIndexPairSet = annotator.getBIndexPairSet(Single(SegmentCon("line")))
        lineBIndexPairSet.toList.map {
          case (blockIndex, charIndex) =>
            val elements = annotator.getElements("line")(blockIndex, charIndex)

            val annotatorPage =  NewHtmlTokenizationSvg.getPageNumber(elements.get(blockIndex).get, annotator.getDom()) + 1

            if(bIndexRaw.contains(blockIndex+"_"+annotatorPage) &&
                  bIndexRaw.get(blockIndex+"_"+annotatorPage).get.contains(annotation) )
            {
              (blockIndex+"_"+annotatorPage, bIndexRaw.get(blockIndex+"_"+annotatorPage).get)
            }
            else
            {
              (blockIndex+"_"+annotatorPage, "other")
            }
        }.toList
      }.asInstanceOf[List[Tuple2[String, String]]]

      val parIds:List[Tuple3[Int,String,String]] = getParagraphIds(blockIdxToRawType, 0, "-begin")

      val parIdsGrouped:Map[Int,List[Tuple3[Int,String,String]]] = parIds.groupBy(_._1)

      var parIdsTrimmed = trimOthers(parIdsGrouped)

      var lastInserted = parIdsTrimmed.map{entry=>
        (entry._1 -> insertLast(entry._2))
      }

      var hashAnnotations:Map[String,String] = lastInserted.values.map{value=>
        value.map{listTags=>
          (listTags._2 -> listTags._3)
        }
      }.flatten.toMap

      (anno._1, anno._2, hashAnnotations)
    }

    return annotateRec(annotations.toList, annotator, bodyLabels)
  }
}
