package org.rexo.pipeline.extractors

import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
import org.rexo.extra.types.{TokenSequence, Sequence}
import scala.collection.mutable
import org.rexo.extra.extract.Span
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils

/**
 * Created by klimzaporojets on 10/2/14.
 */
/**
 * Created by klimzaporojets on 8/4/14.
 */
class BodyRulesTransducerSvg {
  def transduce(data: NewHtmlTokenizationSvg): Sequence = {
    val transducedData: Sequence = new TokenSequence
    val currentlyInColumn: Boolean = false
    var tokenId: Int = 0
    var previousSectionMarker: Boolean = false
    var figureOrTableMarker: String = ""
    var footerMarker: String = ""
    val featuresTableContent: mutable.MutableList[String] = mutable.MutableList[String]() //new ArrayList[String]

    featuresTableContent.+=("pixelsPerCharacter2pxGreater")
    featuresTableContent.+=("pixelsPerCharacterUndefined")
    featuresTableContent.+=("noWordsFromDictionary")
    featuresTableContent.+=("3wordFromDictLess")

//    featuresTableContent.add("pixelsPerCharacter2pxGreater")
//    featuresTableContent.add("pixelsPerCharacterUndefined")
//    featuresTableContent.add("noWordsFromDictionary")
//    featuresTableContent.add("3wordFromDictLess")
    val relaxedFeaturesTableContent: mutable.MutableList[String] = mutable.MutableList[String]() //new ArrayList[String]
    relaxedFeaturesTableContent.+=("pixelsPerCharacter2pxGreater")
    relaxedFeaturesTableContent.+=("noWordsFromDictionary")
    relaxedFeaturesTableContent.+=("oneWordFromDictionary")

//    relaxedFeaturesTableContent.add("pixelsPerCharacter2pxGreater")
//    relaxedFeaturesTableContent.add("noWordsFromDictionary")
//    relaxedFeaturesTableContent.add("oneWordFromDictionary")
    var previousFigure: Boolean = false
    var debugMe: Boolean = false
    var paragraphId: Int = 0
    val lastLabelIndexes: collection.mutable.Map[String, Int] = collection.mutable.Map[String, Int]() //new HashMap[String, Integer]
//    {
      var i: Int = 0
      while (i < data.getLineSpans.size) {
        {
          previousFigure = false
          var label: String = ""
          val previousSpan: Span = if (i > 0) data.getLineSpans/*.get*/(i - 1)/*.get*/.asInstanceOf[Span] else null
          val currentSpan: Span = data.getLineSpans/*.get*/(i)/*.get*/.asInstanceOf[Span]
//          if (currentSpan == null) {
//            continue //todo: continue is not supported
//          }
          if(currentSpan != null)
          {
            val nextSpan: Span = if (i < data.getLineSpans.size - 1) data.getLineSpans(i + 1).asInstanceOf[Span] else null
  //          if (!debugMe) {
  //            debugMe = currentSpan.isInstanceOf[CompositeSpan] && ((currentSpan.asInstanceOf[CompositeSpan]).getProperty("pageNum").asInstanceOf[Double]) eq 2.0
  //          }
            if ((((LayoutUtils.isActiveFeature(currentSpan, "firstLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "secondLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "thirdLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "allCaps") || (LayoutUtils.isActiveFeature(currentSpan, "centeredLine")) || ((LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft") && LayoutUtils.isActiveFeature(currentSpan, "startsCap") && !LayoutUtils.isActiveFeature(currentSpan, "tabbedLeftMargin") && !LayoutUtils.isActiveFeature(currentSpan, "endsInDotAndNumber")) || (previousSectionMarker && LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft")))) && (!LayoutUtils.isActiveFeature(currentSpan, "noColumnAssociated") || columnInFutureWithTitles(i, data, 3)) && (((LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater")) || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance4pxGreater")) || (LayoutUtils.isActiveFeature(currentSpan, "lineHeight2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater")) || (LayoutUtils.isActiveFeature(currentSpan, "lineHeight2pxGreater") && (LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft")))) && (previousSectionMarker || LayoutUtils.isActiveFeature(currentSpan, "newColumn") || LayoutUtils.isActiveFeature(currentSpan, "noColumnAssociated") || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry2pxGreater") && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater"))) && (!LayoutUtils.isActiveFeature(currentSpan, "lineWidth10pxGreater"))) && (!LayoutUtils.isActiveFeature(currentSpan, "endsInDot"))) || (previousSectionMarker && LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater"))) && (!LayoutUtils.isActiveFeature(currentSpan, "noAlphabetic") && LayoutUtils.isActiveFeature(currentSpan, "1wordFormOrGreater") && !LayoutUtils.isActiveFeature(currentSpan, "endsInDot"))) {
              if ((LayoutUtils.isActiveFeature(currentSpan, "firstLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "secondLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "thirdLevelSectionPtrn")) || (previousSectionMarker && LayoutUtils.isActiveFeature(previousSpan, "rightMarginToTheLeft") && LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry2pxGreater") && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater"))) {
                label = "section-marker-begin"
              }
              else {
                label = "section-marker-inside"
              }
            }
            if ((label == "") && LayoutUtils.isActiveFeature(currentSpan, "noColumnAssociated") && (LayoutUtils.isActiveFeature(currentSpan, "newPage") || LayoutUtils.isActiveFeature(currentSpan, "newColumn") || LayoutUtils.isActiveFeature(currentSpan, "upAndToTheLeft") || (previousSpan == null) || LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater") || LayoutUtils.isActiveFeature(previousSpan, "noColumnAssociated") || LayoutUtils.isActiveFeature(currentSpan, "columnLayoutChange") || LayoutUtils.isActiveFeature(currentSpan, "lineHeight30pxGreater") || LayoutUtils.isActiveFeature(currentSpan, "up") || LayoutUtils.isActiveFeature(currentSpan, "indexLinePattern"))) {
              label = "notext"
            }
            else if (label == "") {
              label = "text-inside"
            }
            if (!label.contains("section-marker") && LayoutUtils.isActiveFeature(currentSpan, "columnLayoutChange")) {
              label = "text-begin"
            }
            if ((figureOrTableMarker.contains("table-marker") && ((LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry2pxGreater") && !LayoutUtils.isActiveFeature(currentSpan, "pixelsPerCharacter2pxGreater") && !LayoutUtils.isActiveFeature(currentSpan, "pixelsPerCharacterUndefined") && !LayoutUtils.isActiveFeature(currentSpan, "noWordsFromDictionary") && !LayoutUtils.isActiveFeature(currentSpan, "3wordFromDictLess") && !LayoutUtils.isActiveFeature(currentSpan, "lineHeight1pxLess") && !(LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft") && !LayoutUtils.isActiveFeature(currentSpan, "endsInDot"))) || LayoutUtils.isActiveFeature(currentSpan, "up20PxGreater") || LayoutUtils.isActiveFeature(currentSpan, "newPage")))) {
              figureOrTableMarker = ""
            }
            if ((figureOrTableMarker.contains("figure-marker") && ((LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry2pxGreater") && !(LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft") && !LayoutUtils.isActiveFeature(currentSpan, "endsInDot"))) || LayoutUtils.isActiveFeature(currentSpan, "up20PxGreater") || LayoutUtils.isActiveFeature(currentSpan, "lineHeight10pxGreater") || LayoutUtils.isActiveFeature(currentSpan, "newPage")))) {
              figureOrTableMarker = ""
              previousFigure = true
            }
            if (!(figureOrTableMarker == "") && (!LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater") || !LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry2pxGreater") || (LayoutUtils.isActiveFeature(currentSpan, "pixelsPerCharacter2pxGreater") && figureOrTableMarker.contains("table-marker")) || (LayoutUtils.isActiveFeature(currentSpan, "pixelsPerCharacterUndefined") && figureOrTableMarker.contains("table-marker")) || (LayoutUtils.isActiveFeature(currentSpan, "noWordsFromDictionary") && figureOrTableMarker.contains("table-marker")) || (LayoutUtils.isActiveFeature(currentSpan, "3wordFromDictLess") && figureOrTableMarker.contains("table-marker")) || (LayoutUtils.isActiveFeature(currentSpan, "lineHeight1pxLess") && figureOrTableMarker.contains("table-marker")) || (LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft") && !LayoutUtils.isActiveFeature(currentSpan, "endsInDot")))) {
              label = figureOrTableMarker
            }
            val futureLayout: Boolean = LayoutUtils.isAnyOfFeaturesInFutureSvg(data, i, relaxedFeaturesTableContent, 3, 10)
            if (LayoutUtils.isActiveFeature(currentSpan, "startsTableWord") && (LayoutUtils.isActiveFeature(currentSpan, "upAndToTheLeft") || (LayoutUtils.isActiveFeature(currentSpan, "up") && futureLayout) || (LayoutUtils.isActiveFeature(currentSpan, "up") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop")) || (LayoutUtils.isActiveFeature(currentSpan, "right") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop")) || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance100pxGreater") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop") && !LayoutUtils.isActiveFeature(currentSpan, "newPage")) || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance2pxGreater") && futureLayout && !LayoutUtils.isActiveFeature(currentSpan, "newPage")))) {
              label = "table-marker-begin"
              figureOrTableMarker = "table-marker-inside"
            }
            val futureFigureLayout: Boolean = LayoutUtils.isFigureInTheFutureSvg(data, i, if (LayoutUtils.isActiveFeature(currentSpan, "lineWidth20pxLess")) 15 else 6)
            if (LayoutUtils.isActiveFeature(currentSpan, "startsFigureWord") && (LayoutUtils.isActiveFeature(currentSpan, "upAndToTheLeft") || (LayoutUtils.isActiveFeature(currentSpan, "up") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop")) || (LayoutUtils.isActiveFeature(currentSpan, "right") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop")) || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance100pxGreater") && !LayoutUtils.isActiveFeature(currentSpan, "nearThe150PxOfTop")) || futureFigureLayout || (previousFigure && LayoutUtils.isActiveFeature(currentSpan, "lineWidth20pxLess") && previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance12pxGreater")))) {
              label = "figure-marker-begin"
              figureOrTableMarker = "figure-marker-inside"
            }
            if (((footerMarker == "footer-start") && !LayoutUtils.isActiveFeature(currentSpan, "up")) || LayoutUtils.isActiveFeature(currentSpan, "indexLinePattern")) {
              label = "notext"
            }
            else if ((footerMarker == "footer-start") && LayoutUtils.isActiveFeature(currentSpan, "up")) {
              footerMarker = ""
            }
            else if (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistanceUry4pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "startsEnum") && LayoutUtils.isActiveFeature(currentSpan, "nearThe100PxOfBottom") && LayoutUtils.isActiveFeature(currentSpan, "lineHeight1pxLess")) {
              footerMarker = "footer-start"
              label = "notext"
            }
            if ((label == "text-inside") || (label == "text-begin") || (((label == "section-marker-begin") || (label == "section-marker-inside")) && !LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft") && (!LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater") || !LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater")))) {
              val lastIndex: Int = getLastIndex(lastLabelIndexes, Array[String]("text-begin", "text-inside", "paragraph-begin", "paragraph-inside"))
              val lastSectionIndex: Int = getLastIndex(lastLabelIndexes, Array[String]("section-marker-begin", "section-marker-inside"))
              var lastTextLineRead: Span = null
              if (lastIndex > -1) {
                lastTextLineRead = data.getLineSpans(lastIndex).asInstanceOf[Span]
              }
              if ((LayoutUtils.isActiveFeature(currentSpan, "tabbedLeftMargin") && (lastTextLineRead == null || LayoutUtils.isActiveFeature(lastTextLineRead, "endsInDot")) && LayoutUtils.isActiveFeature(currentSpan, "startsCap")) || LayoutUtils.isActiveFeature(currentSpan, "newColumn") || LayoutUtils.isActiveFeature(currentSpan, "newPage") || LayoutUtils.isActiveFeature(currentSpan, "upAndToTheLeft") || (lastTextLineRead != null && LayoutUtils.isActiveFeature(lastTextLineRead, "rightMarginToTheLeft") && (LayoutUtils.isActiveFeature(lastTextLineRead, "endsInDot") || LayoutUtils.isActiveFeature(lastTextLineRead, "endsInDotAndNumber"))) || (lastTextLineRead != null && LayoutUtils.isActiveFeature(lastTextLineRead, "endsInColon") && LayoutUtils.isActiveFeature(currentSpan, "noWordsFromDictionary")) || (lastTextLineRead != null && LayoutUtils.isActiveFeature(lastTextLineRead, "noWordsFromDictionary") && LayoutUtils.isActiveFeature(lastTextLineRead, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(lastTextLineRead, "verticalDistanceUry2pxGreater")) || (lastSectionIndex == i - 1)) {
                if (isPossibleFormula(lastTextLineRead) && isPossibleFormula(currentSpan)) {
                  label = "paragraph-inside"
                }
                else {
                  if (isNewParagraph(lastTextLineRead, currentSpan)) {
                    paragraphId += 1
                  }
                  label = "paragraph-begin--_--" + "id=" + paragraphId
                }
              }
              else {
                if (isPossibleFormula(lastTextLineRead) && !isPossibleFormula(currentSpan) && (LayoutUtils.isActiveFeature(lastTextLineRead, "verticalDistance2pxGreater") || LayoutUtils.isActiveFeature(lastTextLineRead, "verticalDistanceUry2pxGreater"))) {
                  paragraphId += 1
                  label = "paragraph-begin--_--" + "id=" + paragraphId
                }
                else {
                  if (lastIndex < i - 1) {
                    label = "paragraph-begin--_--" + "id=" + paragraphId
                  }
                  else {
                    label = "paragraph-inside"
                  }
                }
              }
            }
            lastLabelIndexes.put(if (label.contains("paragraph-begin")) "paragraph-begin" else label, i)
            tokenId = addLabelToAllSpans(currentSpan, label, transducedData.asInstanceOf[TokenSequence], data, tokenId)
            if ((label == "section-marker-begin") || (label == "section-marker-inside")) {
              previousSectionMarker = true
            }
            else {
              previousSectionMarker = false
            }
          }
        }
        //not null if ends here

        ({
          i += 1; i - 1
        })
      }
//    }
    return transducedData
  }

  private def isNewParagraph(previousLineSpan: Span, currentLineSpan: Span): Boolean = {
    if ((LayoutUtils.isActiveFeature(currentLineSpan, "newColumn") || LayoutUtils.isActiveFeature(currentLineSpan, "newPage") || LayoutUtils.isActiveFeature(currentLineSpan, "upAndToTheLeft")) && (!LayoutUtils.isActiveFeature(previousLineSpan, "endsInDot") || !LayoutUtils.isActiveFeature(previousLineSpan, "startsCap") && (!LayoutUtils.isActiveFeature(previousLineSpan, "rightMarginToTheLeft") && (!LayoutUtils.isActiveFeature(currentLineSpan, "tabbedLeftMargin"))))) {
      return false
    }
    return true
  }

  private def isPossibleFormula(currentSpan: Span): Boolean = {
    if (currentSpan != null && LayoutUtils.isActiveFeature(currentSpan, "noWordsFromDictionary") && !LayoutUtils.isActiveFeature(currentSpan, "1wordFormOrGreater")) {
      return true
    }
    return false
  }

  private def getLastIndex(indexes: collection.mutable.Map[String, Int], keys: Array[String]): Int = {
    var lastIndex: Int = -1
    for (key <- keys) {
      val optValue: Option[Int] = indexes.get(key)
      if(optValue != None)
      {
        val kValue: Int = optValue.get //indexes.get(key).getOrElse(null)
        if (kValue != null && kValue > lastIndex) {
          lastIndex = kValue
        }
      }
    }
    return lastIndex
  }

  private def columnInFutureWithTitles(i: Int, data: NewHtmlTokenizationSvg, linesInFuture: Int): Boolean = {
//    {
      var cnt: Int = 0
      while (cnt < linesInFuture && cnt + i < data.getLineSpans.size) {
        {
          val currentSpan: Span = data.getLineSpans(cnt + i).asInstanceOf[Span]
          var nextSpan: Span = null
          if (cnt + i + 1 < data.getLineSpans.size) {
            nextSpan = data.getLineSpans(cnt + i + 1).asInstanceOf[Span]
          }
          var previousSpan: Span = null
          if (cnt > 0) {
            previousSpan = data.getLineSpans(cnt + i - 1).asInstanceOf[Span]
          }
          if (cnt > 0 && (LayoutUtils.isActiveFeature(currentSpan, "newColumn") || LayoutUtils.isActiveFeature(currentSpan, "newPage"))) {
            return false
          }
          else if (LayoutUtils.isActiveFeature(currentSpan, "columnLayoutChange")) {
            return true
          }
          else if ((((LayoutUtils.isActiveFeature(currentSpan, "firstLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "secondLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "thirdLevelSectionPtrn") || LayoutUtils.isActiveFeature(currentSpan, "allCaps") || (LayoutUtils.isActiveFeature(currentSpan, "centeredLine")) || (LayoutUtils.isActiveFeature(currentSpan, "rightMarginToTheLeft"))) && ((LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater")) || (previousSpan != null && LayoutUtils.isActiveFeature(previousSpan, "verticalDistance4pxGreater")) || (LayoutUtils.isActiveFeature(currentSpan, "lineHeight2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistanceUry2pxGreater") && LayoutUtils.isActiveFeature(currentSpan, "verticalDistance2pxGreater"))) && (!LayoutUtils.isActiveFeature(currentSpan, "endsInDot")))) && (!LayoutUtils.isActiveFeature(currentSpan, "noAlphabetic"))) {
            //continue //todo: continue is not supported
          }
          else
          {
            return false
          }
        }
        ({
          cnt += 1; cnt - 1
        })
      }
//    }
    return false
  }

  def addLabelToAllSpans(span: Span, label: String, transducedData: TokenSequence, data: NewHtmlTokenizationSvg, tokenId: Int): Int = {
    span.getEndIdx
    var tok: Span = null
    var tokenIdVar = tokenId
    var labelVar = label
    while (tokenIdVar < data.size &&
      data.getSpan(tokenIdVar).getEndIdx <= span.getEndIdx) {
      transducedData.add(label)
      tokenIdVar += 1
      if (labelVar == "text-begin") {
        labelVar = "text-inside"
      }
      if (labelVar == "section-marker-begin") {
        labelVar = "section-marker-inside"
      }
      if (labelVar == "figure-marker-begin") {
        labelVar = "figure-marker-inside"
      }
      if (labelVar == "table-marker-begin") {
        labelVar = "table-marker-inside"
      }
      labelVar = labelVar.replaceAll("paragraph\\-begin.*", "paragraph-inside")
    }
    return tokenIdVar
  }

  private[extractors] class SectionMarker {
    private var features: List[_] = null
  }

}