package edu.umass.cs.rexo.ghuang.segmentation.svg

import java.util.logging.{Logger, Level}

import org.rexo.extraction.NewHtmlTokenizationSvg
import org.rexo.extra.extract.Span
import org.rexo.extra.types.PropertyHolder
import edu.umass.cs.rexo.ghuang.segmentation.JournalSegmenter

//import old.base.types.PropertyHolder
import org.rexo.referencetagging.{ReferencesNotFoundException, HeaderNotFoundException}

//import old.base.extract.Span
import scala.collection.mutable


/**
 * Author: saunders Created Nov 9, 2005 Copyright (C) Univ. of Massachusetts Amherst, Computer Science Dept.
 */

object LayoutSegmentFinderSvg {
  val NULL_PATTERN: scala.util.matching.Regex = null
  val INTRODUCTION_PATTERN = new scala.util.matching.Regex("""^[#iIvVxX\s\.\d]*(I(?i:ntroduction)|I(?i:ntroduction and Motivation))\s*""")
  val ABSTRACT_PATTERN = new scala.util.matching.Regex("""^[\s]*((A(?i:bstract))|((abstract)))[\s]*$""")
  val BIBLIOGRAPHY_PATTERN = new scala.util.matching.Regex("""^[#iIvVxX\d\.\s]{0,5}(R(?i:eferences)|B(?i:ibliography)|R(?i:eferences and Notes)|L(?i:iterature Cited)|(.*REFERENCES.*))\s*$""")

}

class LayoutSegmentFinderSvg {

  val logger = Logger.getLogger("LayoutSegmentFinder")

  private[segmentation] var m_rulesBibSegmentor: RulesBibliographySegmenterSvg = new RulesBibliographySegmenterSvg;

  def markup(tokenization: NewHtmlTokenizationSvg): collection.mutable.Map[String, Any] = {
    return getSubsections(tokenization)
  }

  protected def getSubsections(tokenization: NewHtmlTokenizationSvg): collection.mutable.Map[String, Any ] = {
    val subsections: collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()

    var lineSpans = collection.mutable.MutableList[Span]()
    //this is added insted of assigning tokenization.getLineSpans in previous line
    lineSpans.++=(tokenization.getLineSpans.toList)


    val headerLineList: collection.mutable.MutableList[Span] = collection.mutable.MutableList[Span]()

    var subList: mutable.MutableList[Span] = findMatchingLines(lineSpans, LayoutSegmentFinderSvg.NULL_PATTERN,
                          LayoutSegmentFinderSvg.ABSTRACT_PATTERN, Integer.MAX_VALUE, Integer.MAX_VALUE)
    if (!subList.isEmpty) {
      headerLineList.++=(subList)
      lineSpans = lineSpans.takeRight(lineSpans.size - subList.size)
      subList.clear
      subList = findMatchingLines(lineSpans, LayoutSegmentFinderSvg.ABSTRACT_PATTERN,
                        LayoutSegmentFinderSvg.INTRODUCTION_PATTERN, Integer.MAX_VALUE, 1)

      headerLineList.++=(subList)
      lineSpans = lineSpans.takeRight(lineSpans.size - subList.size)
      subList.clear
    }
    else {
      subList = findMatchingLines(lineSpans, LayoutSegmentFinderSvg.NULL_PATTERN,
                              LayoutSegmentFinderSvg.INTRODUCTION_PATTERN, Integer.MAX_VALUE, Integer.MAX_VALUE)

      if (!subList.isEmpty) {
        headerLineList.++=(subList)
        lineSpans = lineSpans.takeRight(lineSpans.size - subList.size)
        subList.clear
      }
      else {
        val js: JournalSegmenter = JournalSegmenter.getSegmenter(lineSpans.toList)
        if (js == null) {
//          logger.log(Level.WARNING, "Did not find abstract or introduction")
          throw HeaderNotFoundException("Did not find 'abstract' or 'introduction'")
        }
        subList = js.getAbstract(lineSpans)
        if (subList.isEmpty) {
//          logger.log(Level.WARNING, "Did not find abstract or introduction")
          throw HeaderNotFoundException("Did not find 'abstract' or 'introduction'")
        }
        else {
          headerLineList.++=(subList)
          lineSpans = lineSpans.takeRight(lineSpans.size - subList.size)
          subList.clear
        }
      }
    }
    val headerTokenBoundaries: Array[Long] = lineListBoundaries(headerLineList)
    val header: NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(headerTokenBoundaries(0).asInstanceOf[Int], headerTokenBoundaries(1).asInstanceOf[Int])
    subsections.put("headerTokenization", header)
    val bodyLines: mutable.MutableList[Span] = mutable.MutableList[Span]()
    subList = findMatchingLines(lineSpans, LayoutSegmentFinderSvg.NULL_PATTERN,
                                  LayoutSegmentFinderSvg.BIBLIOGRAPHY_PATTERN, Integer.MAX_VALUE, Integer.MAX_VALUE)
    while (subList.nonEmpty) {
      bodyLines.++=(subList)
      lineSpans = lineSpans.takeRight(lineSpans.size - subList.size)
      subList.clear
      subList = findMatchingLines(lineSpans, LayoutSegmentFinderSvg.BIBLIOGRAPHY_PATTERN,
                  LayoutSegmentFinderSvg.BIBLIOGRAPHY_PATTERN, Integer.MAX_VALUE, Integer.MAX_VALUE)
    }

    if (bodyLines.nonEmpty) {
      val bodyTokenBoundaries: Array[Long] = lineListBoundaries(bodyLines)
      val body: NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(bodyTokenBoundaries(0).asInstanceOf[Int], bodyTokenBoundaries(1).asInstanceOf[Int])
      subsections.put("bodyTokenization", body)
    }
    else {
//      logger.log(Level.WARNING, "Did not find body section")
      throw new ReferencesNotFoundException("Did not find body section")
    }

    val referenceLines:mutable.MutableList[Span] = mutable.MutableList[Span]()
    referenceLines ++= lineSpans
    if(referenceLines.nonEmpty)
    {
      val referenceTokenBoundaries:Array[Long] = lineListBoundaries(referenceLines)
      val references:NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(referenceTokenBoundaries(0).asInstanceOf[Int], referenceTokenBoundaries(1).asInstanceOf[Int])
      subsections.put("referencesTokenization", references)
    }
    else
    {
      logger.log(Level.WARNING, "Did not find references section")
//      throw new ReferencesNotFoundException("did not find reference for adding into lines")
    }

    val biblioBoundaries: Array[Long] = lineListBoundaries(lineSpans)
    val biblio: NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(biblioBoundaries(0).asInstanceOf[Int], biblioBoundaries(1).asInstanceOf[Int])
    val referenceData: RulesBibliographySegmenterSvg.ReferenceData = m_rulesBibSegmentor.segmentReferences(biblio)
    subsections.put("featuresData", biblio.getProperty("referenceFeatures"))

    if (referenceData.prologueList.nonEmpty) {
      val prologueTokenBoundaries: Array[Long] = lineListBoundaries(referenceData.prologueList)
      val prologue: NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(prologueTokenBoundaries(0).asInstanceOf[Int], prologueTokenBoundaries(1).asInstanceOf[Int])
      subsections.put("prologueTokenization", prologue)
    }

    var referencesList: collection.mutable.MutableList[Any] = referenceData.referenceLineList
    val refTokenizationList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]()
    while (referencesList.nonEmpty) {
      val referenceLine: mutable.MutableList[Any] = referencesList.head.asInstanceOf[mutable.MutableList[Any]]
      if(referencesList.size == 1 )
      {
        referencesList = collection.mutable.MutableList[Any]()
      }
      else
      {
        referencesList = referencesList.tail
      }
      val referenceTokenBoundaries: Array[Long] = lineListBoundaries(referenceLine.asInstanceOf[mutable.MutableList[Span]])
      val reference: NewHtmlTokenizationSvg = tokenization.getSubspanTokenization(referenceTokenBoundaries(0).asInstanceOf[Int], referenceTokenBoundaries(1).asInstanceOf[Int])
      refTokenizationList.+=(reference)
    }
    subsections.put("referenceList", refTokenizationList)
    subsections.put("referenceLabels", referenceData.predictedLabels)

    subsections
  }

  private def findMatchingLines(lineSpans: mutable.MutableList[Span], beginPattern: scala.util.matching.Regex,
                                endPattern: scala.util.matching.Regex,
                                        lineCountMax: Int, pageCountMax: Int): mutable.MutableList[Span] = {
    var lineCount: Int = 0
    var pageCount: Int = 1
    var foundBegin: Boolean = beginPattern == null
    var foundEnd: Boolean = endPattern == null
    val docLineCount: Int = lineSpans.size
    var subListStart: Int = if (foundBegin) { 0 }else {docLineCount};
    var subListEnd: Int = if (foundEnd) {docLineCount} else {0};

      var i: Int = 0
      while (i < lineSpans.size) {
        {
          val lineSpan: Span = lineSpans(i)
          val text: String = lineSpan.getText
          lineCount += 1
          if (i > 0 && isNewPage(lineSpan.asInstanceOf[PropertyHolder])) {
            pageCount += 1
          }
          if (!((lineSpan.asInstanceOf[PropertyHolder]).getNumericProperty("isHeaderFooterLine") > 0)) {
            if (!foundBegin) {
              val found = beginPattern.findAllIn(text)
              if(!found.isEmpty)
                foundBegin = true
                subListStart = i
                lineCount = 0
                pageCount = 0
              }
            else if (!foundEnd) {

              val found:Boolean  = endPattern.pattern.matcher(text).matches()
              if (found || lineCount > lineCountMax || pageCount > pageCountMax) {
                foundEnd = true
                subListEnd = i
              }
            }
            else {
              return lineSpans.slice(subListStart, subListEnd)

            }
          }

          }
        i += 1;
      }
    lineSpans.slice(subListStart, subListEnd)
  }


  private def isNewPage(span: PropertyHolder): Boolean = {
    span.getNumericProperty("newpage") > 0.0
  }

  private def lineListBoundaries(lineList: mutable.MutableList[Span]): Array[Long] = {
    val start: Span = lineList(0)
    val end: Span = lineList(lineList.size - 1)
    val startToken: Long = start.getStartIdx
    val endToken: Long = end.getEndIdx
    return Array[Long](startToken, endToken)
  }
}
