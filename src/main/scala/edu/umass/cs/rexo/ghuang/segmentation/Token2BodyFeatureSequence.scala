package edu.umass.cs.rexo.ghuang.segmentation

import org.rexo.extraction.NewHtmlTokenization
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils
import scala.collection.mutable.MutableList
import org.rexo.extra.extract.Span
import scala.collection.JavaConversions._
import org.rexo.util.EnglishDictionary
import java.util.regex.{Matcher, Pattern}
import org.rexo.base.{Instance, Pipe}


/**
 * Created by klimzaporojets on 10/1/14.
 */

object Token2BodyFeatureSequence {

  private var lonelyNumbers: String = "[1-9][\\.]{0,1}[\\s]+]"
  private var lonelyLetters: String = "[A-ZÁÉÍÓÚÀÈÌÒÙÇÑÏÜ][\\.]{0,1}[\\s]+]"
  private var wordForms: String = "(((^)|(\\s))([A-Z]{0,1}(([A-Z]{3,99})|([a-z]{3,99})))(($)|([\\s:\\.,]{0,1})))"
  private var wordsWithSubindex: String = "([A-Z]{1,1}[a-z]{0,1}[\\s]{1,1}[\\d\\.]{1,5})"
  private[segmentation] var ptrnLonelyNumbers: Pattern = Pattern.compile(lonelyNumbers)
  private[segmentation] var ptrnLonelyLetters: Pattern = Pattern.compile(lonelyLetters)
  private[segmentation] var ptrnWordForms: Pattern = Pattern.compile(wordForms)
  private[segmentation] var ptrnWordWithSubindex: Pattern = Pattern.compile(wordsWithSubindex)
  private[segmentation] var wordsInDictionaryPerLine: MutableList[LayoutUtils.Entry] = MutableList[LayoutUtils.Entry]()
  private var columnAcceptableErrorRight: Int = 5
  private var columnAcceptableErrorLeft: Int = 3


  private def computeLayoutFeatures(lineInfos: Array[LineInfo], data: NewHtmlTokenization) {
    var verticalDistance: collection.mutable.MutableList[LayoutUtils.Entry] = collection.mutable.MutableList[LayoutUtils.Entry]()
    var lineWidth: collection.mutable.MutableList[LayoutUtils.Entry] = collection.mutable.MutableList[LayoutUtils.Entry]()
    var pixelsPerCharacter: collection.mutable.MutableList[LayoutUtils.Entry] = collection.mutable.MutableList[LayoutUtils.Entry]()
    val widthLinePerPage: collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]] = collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]]()
    val columnsData: collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]] = collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]]()
    val leftMarginsData: collection.mutable.Map[Int, MutableList[LayoutUtils.Entry]] = collection.mutable.Map[Int, MutableList[LayoutUtils.Entry]]()
    val pagesData: collection.mutable.Map[Int, LayoutUtils.PageData] = collection.mutable.Map[Int, LayoutUtils.PageData]()
    val columns: collection.mutable.Map[Int, MutableList[LayoutUtils.ColumnData]] = collection.mutable.Map[Int, MutableList[LayoutUtils.ColumnData]]()
    var lineHeight: MutableList[LayoutUtils.Entry] = MutableList[LayoutUtils.Entry]()
    var prevPageNum: Int = 0
    var lineSpanCount: Int = -1
      var i: Int = 0
      while (i < data.getLineSpans.size) {
        {
          val currentSpan: Span = data.getLineSpans.get(i)
          try {
            val urx: Int = (LayoutUtils.getProperty(currentSpan, "urx").asInstanceOf[Double]).intValue
            val llx: Int = (LayoutUtils.getProperty(currentSpan, "llx").asInstanceOf[Double]).intValue
            val ury: Int = (LayoutUtils.getProperty(currentSpan, "ury").asInstanceOf[Double]).intValue
            val lly: Int = (LayoutUtils.getProperty(currentSpan, "lly").asInstanceOf[Double]).intValue
            val page: Int = (LayoutUtils.getProperty(currentSpan, "pageNum").asInstanceOf[Double]).intValue
            LayoutUtils.adjustPageData(urx, llx, ury, lly, page, pagesData)
          }
          catch {
            case ex: Exception => {
              ex.printStackTrace
            }
          }
        }
        ({
          i += 1; i - 1
        })
      }
    {
      var i: Int = 0
      while (i < lineInfos.length) {
        {
          lineSpanCount = updateLineSpanCounter(data, lineSpanCount + 1)

            val lineSpan: Span = data.getLineSpans.get(lineSpanCount)

            if (lineInfos(i).page != prevPageNum) {
              LayoutUtils.setFeatureValue(lineSpan, "newPage", 1.0)
              prevPageNum = lineInfos(i).page
              if (i > 0) {
                LayoutUtils.setFeatureValue(data.getLineSpans.get(i - 1), "lastLineOnPage", 1.0)
              }
            }
            else if (i > 0 && (lineInfos(i).llx > lineInfos(i - 1).urx && lineInfos(i).lly < lineInfos(i - 1).lly)) {
              LayoutUtils.setFeatureValue(lineSpan, "newColumn", 1.0)
            }
            else if (i > 0 && (lineInfos(i).llx <= lineInfos(i - 1).llx && lineInfos(i).lly < lineInfos(i - 1).lly)) {
              LayoutUtils.setFeatureValue(lineSpan, "upAndToTheLeft", 1.0)
            }
            if (i > 0 && (lineInfos(i).lly /*>*/ < lineInfos(i - 1).lly)) {
              LayoutUtils.setFeatureValue(lineSpan, "up", 1.0)
              if ( lineInfos(i - 1).lly - lineInfos(i).lly > 20) {
                LayoutUtils.setFeatureValue(lineSpan, "up20PxGreater", 1.0)
              }
            }
            if (i > 0 && (lineInfos(i).llx > lineInfos(i - 1).urx)) {
              LayoutUtils.setFeatureValue(lineSpan, "right", 1.0)
            }
            LayoutUtils.adjustLineHeight(lineInfos, i, lineHeight)
            LayoutUtils.adjustVerticalDistance(lineInfos, i, verticalDistance)
            LayoutUtils.adjustLineWidth(lineInfos, i, lineWidth)
            LayoutUtils.adjustLineWidthPerPage(lineInfos, i, widthLinePerPage)
            LayoutUtils.adjustColumnData(lineInfos, i, columnsData, true, columnAcceptableErrorLeft, columnAcceptableErrorRight, lineSpan)
            LayoutUtils.adjustColumnData(lineInfos, i, leftMarginsData, false, 0, 0, lineSpan)
            LayoutUtils.adjustPixelsPerCharacter(lineInfos, i, pixelsPerCharacter)
        }
        ({
          i += 1;
        })
      }
    }
    verticalDistance = verticalDistance.sortWith(_.getQty > _.getQty)
    lineHeight = lineHeight.sortWith(_.getQty > _.getQty)
    lineWidth = lineWidth.sortWith(_.getQty > _.getQty)
    pixelsPerCharacter = pixelsPerCharacter.sortWith(_.getQty > _.getQty)

    for (page <- widthLinePerPage.keySet) {
      widthLinePerPage.put(page, widthLinePerPage.get(page).get.sortWith(_.getQty > _.getQty))

      columnsData.put(page, columnsData.get(page).get.sortWith(_.getQty > _.getQty))

      leftMarginsData.put(page, leftMarginsData.get(page).get.sortWith(_.getQty > _.getQty))

      var currentPageCols: MutableList[LayoutUtils.ColumnData] = LayoutUtils.getColumnsV2(columnsData.get(page).get, pagesData.get(page).get)

      currentPageCols = currentPageCols.sortWith(_.getLeftX > _.getLeftX)

      columns.put(page, currentPageCols)
    }
    var lastLineColumn: LayoutUtils.ColumnData = null
    lineSpanCount = -1

      i=0;
      while (i < lineInfos.length) {
        {
          val currentLineColumn: LayoutUtils.ColumnData = LayoutUtils.getCurrentLineColumn(lineInfos, i, columns.get(lineInfos(i).page).get, true, columnAcceptableErrorLeft, columnAcceptableErrorRight, lastLineColumn, verticalDistance.get(0).get.getKey.asInstanceOf[Int])
          lineSpanCount = updateLineSpanCounter(data, lineSpanCount + 1)
          val lineSpan: Span = data.getLineSpans.get(lineSpanCount)
          var sloppyColumn: LayoutUtils.ColumnData = null
          if (currentLineColumn == null) {
            LayoutUtils.setFeatureValue(lineSpan, "noColumnAssociated", 1.0)
            sloppyColumn = LayoutUtils.getClosestCurrentLineColumn(lineInfos, i, columns.get(lineInfos(i).page).get, true, columnAcceptableErrorLeft, columnAcceptableErrorRight, true, false, -1)
            if (sloppyColumn != null) {
              LayoutUtils.setFeatureValue(lineSpan, "sloppyStrictLeft", 1.0)
              if (lineInfos(i).urx >= sloppyColumn.getRightX - columnAcceptableErrorRight && lineInfos(i).urx <= sloppyColumn.getRightX + 10) {
                LayoutUtils.setFeatureValue(lineSpan, "sloppyStrictLeft10PxFromColRight", 1.0)
              }
              if (lineInfos(i).urx >= sloppyColumn.getRightX - columnAcceptableErrorRight && lineInfos(i).urx <= sloppyColumn.getRightX + 15) {
                LayoutUtils.setFeatureValue(lineSpan, "sloppyStrictLeft15PxFromColRight", 1.0)
              }
            }
            else {
              sloppyColumn = LayoutUtils.getClosestCurrentLineColumn(lineInfos, i, columns.get(lineInfos(i).page).get, true, columnAcceptableErrorLeft, columnAcceptableErrorRight, false, false, -1)
              if (sloppyColumn != null) {
                LayoutUtils.setFeatureValue(lineSpan, "onlySloppy", 1.0)
              }
            }
            if (sloppyColumn != null) {
              var vertMarginColumnDiff: Int = 0
              if (lineInfos(i).lly > sloppyColumn.getBottomY) {
                vertMarginColumnDiff = lineInfos(i).lly - sloppyColumn.getBottomY
                LayoutUtils.setFeatureValue(lineSpan, "lineBelowColumn", 1.0)
              }
              if (lineInfos(i).ury < sloppyColumn.getTopY) {
                vertMarginColumnDiff = sloppyColumn.getTopY - lineInfos(i).ury
                LayoutUtils.setFeatureValue(lineSpan, "lineAboveColumn", 1.0)
              }
              if (vertMarginColumnDiff > 0) {
                if (vertMarginColumnDiff >= 10) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn10px", 1.0)
                }
                if (vertMarginColumnDiff >= 20) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn20px", 1.0)
                }
                if (vertMarginColumnDiff >= 30) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn30px", 1.0)
                }
                if (vertMarginColumnDiff >= 40) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn40px", 1.0)
                }
                if (vertMarginColumnDiff >= 50) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn50px", 1.0)
                }
                if (vertMarginColumnDiff >= 60) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn60px", 1.0)
                }
                if (vertMarginColumnDiff >= 100) {
                  LayoutUtils.setFeatureValue(lineSpan, "vertOutsideColumn100px", 1.0)
                }
              }
            }
          }
          else {
            if (lastLineColumn != null) {
              if (!(lastLineColumn == currentLineColumn)) {
                LayoutUtils.setFeatureValue(lineSpan, "columnLayoutChange", 1.0)
              }
            }
            else {
              LayoutUtils.setFeatureValue(lineSpan, "columnLayoutChange", 1.0)
            }
            lastLineColumn = currentLineColumn
          }
          var isContentNearTheTop: Boolean = LayoutUtils.isNearTheTop(lineInfos(i), pagesData.get(lineInfos(i).page).get, 100)
          if (isContentNearTheTop) {
            LayoutUtils.setFeatureValue(lineSpan, "nearThe100PxOfTop", 1.0)
          }
          isContentNearTheTop = LayoutUtils.isNearTheTop(lineInfos(i), pagesData.get(lineInfos(i).page).get, 150)
          if (isContentNearTheTop) {
            LayoutUtils.setFeatureValue(lineSpan, "nearThe150PxOfTop", 1.0)
          }
          val isContentNearTheBottom: Boolean = LayoutUtils.isNearTheBottom(lineInfos(i), pagesData.get(lineInfos(i).page).get, 100)
          if (isContentNearTheBottom) {
            LayoutUtils.setFeatureValue(lineSpan, "nearThe100PxOfBottom", 1.0)
          }
          val pxlsXCharacter: Int = LayoutUtils.getPixelsPerCharacter(lineInfos, i)
          val mostCommonPxlsXCharacter: Int = pixelsPerCharacter.get(0).get.getKey.asInstanceOf[Int]
          if (pxlsXCharacter > mostCommonPxlsXCharacter) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacter1pxGreater", 1.0)
          }
          if (pxlsXCharacter > mostCommonPxlsXCharacter + 1) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacter2pxGreater", 1.0)
          }
          if (pxlsXCharacter > mostCommonPxlsXCharacter + 2) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacter3pxGreater", 1.0)
          }
          if (pxlsXCharacter > mostCommonPxlsXCharacter + 3) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacter4pxGreater", 1.0)
          }
          if (pxlsXCharacter > mostCommonPxlsXCharacter + 4) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacter5pxGreater", 1.0)
          }
          if (pxlsXCharacter == -1) {
            LayoutUtils.setFeatureValue(lineSpan, "pixelsPerCharacterUndefined", 1.0)
          }
          val mostCommonLineWidth: Int = lineWidth.get(0).get.getKey.asInstanceOf[Int]
          val currentLineWidth: Int = LayoutUtils.getCurrentLineWidth(lineInfos, i)
          if (currentLineWidth < mostCommonLineWidth - 19) {
            LayoutUtils.setFeatureValue(lineSpan, "lineWidth20pxLess", 1.0)
          }
          if (currentLineWidth > mostCommonLineWidth + 9) {
            LayoutUtils.setFeatureValue(lineSpan, "lineWidth10pxGreater", 1.0)
          }
          val mostCommonVertDistance: Int = verticalDistance.get(0).get.getKey.asInstanceOf[Int]
          val currentVertDistance: Int = LayoutUtils.getCurrentVerticalDistance(lineInfos, i)
          if (currentVertDistance > mostCommonVertDistance) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance1pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 1) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance2pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 3) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance4pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 5) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance6pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 7) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance8pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 9) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance10pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 11) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance12pxGreater", 1.0)
          }
          if (currentVertDistance > mostCommonVertDistance + 99) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistance100pxGreater", 1.0)
          }
          val currentVertDistanceUry: Integer = LayoutUtils.getCurrentVerticalDistanceUry(lineInfos, i)
          if (currentVertDistanceUry > mostCommonVertDistance) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistanceUry1pxGreater", 1.0)
          }
          if (currentVertDistanceUry > mostCommonVertDistance + 1) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistanceUry2pxGreater", 1.0)
          }
          if (currentVertDistanceUry > mostCommonVertDistance + 3) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistanceUry4pxGreater", 1.0)
          }
          if (currentVertDistanceUry > mostCommonVertDistance + 5) {
            LayoutUtils.setFeatureValue(lineSpan, "verticalDistanceUry6pxGreater", 1.0)
          }
          val mostCommonLineHeight: Int = lineHeight.get(0).get.getKey.asInstanceOf[Int]
          val currentLineHeight: Integer = LayoutUtils.getCurrentLineHeight(lineInfos, i)
          if (currentLineHeight < mostCommonLineHeight) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight1pxLess", 1.0)
          }
          if (currentLineHeight < mostCommonLineHeight - 1) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight2pxLess", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight1pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 1) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight2pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 3) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight4pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 5) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight6pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 7) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight8pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 9) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight10pxGreater", 1.0)
          }
          if (currentLineHeight > mostCommonLineHeight + 29) {
            LayoutUtils.setFeatureValue(lineSpan, "lineHeight30pxGreater", 1.0)
          }
          val leftMarginColumn: Int = if (currentLineColumn != null) currentLineColumn.getLeftX else if (sloppyColumn != null) sloppyColumn.getLeftX else -1
          val rightMarginColumn: Int = if (currentLineColumn != null) currentLineColumn.getRightX else if (sloppyColumn != null) sloppyColumn.getRightX else -1
          if (LayoutUtils.isCentered(lineInfos(i), leftMarginColumn, rightMarginColumn, 3)) {
            LayoutUtils.setFeatureValue(lineSpan, "centeredLine", 1.0)
          }
          if (LayoutUtils.isRightMarginToTheLeft(lineInfos(i), if (currentLineColumn != null) currentLineColumn else sloppyColumn, 10)) {
            LayoutUtils.setFeatureValue(lineSpan, "rightMarginToTheLeft", 1.0)
          }
          if (LayoutUtils.isLeftMarginTabbed(lineInfos(i), if (currentLineColumn != null) currentLineColumn else sloppyColumn, 5)) {
            LayoutUtils.setFeatureValue(lineSpan, "tabbedLeftMargin", 1.0)
          }
          if (LayoutUtils.isShorterThanPrevious(lineInfos, i, 15)) {
            LayoutUtils.setFeatureValue(lineSpan, "shorterThanPreviousLine", 1.0)
          }
          if (LayoutUtils.isSameLeftMarginAsPrevious(lineInfos, i, 3)) {
            LayoutUtils.setFeatureValue(lineSpan, "sameLeftMarginAsPreviousLine", 1.0)
          }
        }
        ({
          i += 1; i - 1
        })
      }
  }

  private def updateLineSpanCounter(data: NewHtmlTokenization, currentCounter: Int): Int = {
    var currentCounterVar = currentCounter
    while (currentCounterVar < data.getLineSpans.size && LayoutUtils.isPropertySet(data.getLineSpans.get(currentCounterVar)/*.get.asInstanceOf[Span]*/, "isHeaderFooterLine")) {
      currentCounterVar += 1
    }
    return currentCounterVar
  }

  private def computeLexiconFeatures(data: NewHtmlTokenization, dictionary: EnglishDictionary) {
    val tableWords: Array[String] = Array("^T[\\s]{0,5}(?i:a[\\s]{0,5}b[\\s]{0,5}l[\\s]{0,5}e).*")
    val figureWords: Array[String] = Array("^F(?i:igure).*", "^F(?:ig\\.).*")
    val allCaps: String = "[#\\[\\]\\(\\);:\\.,'\"\\*A-ZÁÉÍÓÚÀÈÌÒÙÇÑÏÜ1-9]+"
    val initCap: String = "[A-ZÁÉÍÓÚÀÈÌÒÙÇÑÏÜ].*"
    val finalDot: String = "((.*)\\.)$"
    val finalDotAndNumber: String = "((.*)\\.[\\s]*[0-9,]+[\\s]*)$"
    val noAlphabetical: String = "^[^A-Za-z]+"
    val firstLevelSection: String = "^((\\s)*([\\d]+)([\\.]{0,1})([\\s]+)[A-Z0-9].*)"
    val secondLevelSection: String = "^((\\s)*([\\d]+)(\\.)([\\d]+)([\\.]{0,1})([\\s]+)[A-Z0-9].*)"
    val thirdLevelSection: String = "^((\\s)*([\\d]+)(\\.)([\\d]+)(\\.)([\\d]+)([\\.]{0,1})([\\s]+)[A-Z0-9].*)"
    val startsEnum: String = "(([#*])|([1-9]{1,1}(\\s))).*"
    val contentsPattern: String = "^C(?i:ontents)$"
    val indexLinePattern: String = ".*[\\.]{5,999}[\\s]{0,5}[\\d]+"
    val endsInEnum: String = ".*((\\([\\d]+\\))|(\\[[\\d]+\\])|(\\([\\d]+\\.[\\d]\\))|(\\[[\\d]+\\.[\\d]\\]))$"
    val endsInColon: String = ".*:$"

      var i: Int = 0
      while (i < data.getLineSpans.size) {
        {
          val lexiconFeatures: List[String] = List[String]() //new ArrayList[String]
          val ls: Span = data.getLineSpans.get(i) //.get.asInstanceOf[Span]
          val currentLineText: String = ls.getText.trim
          val squishedLineText: String = currentLineText.replaceAll("\\s", "");

            var j: Int = 0
            var breakMe: Boolean = false

            while (!breakMe && j < tableWords.length) {
              {
                if (currentLineText.matches(tableWords(j))) {
                  LayoutUtils.setFeatureValue(ls, "startsTableWord", 1.0)
                  breakMe = true
                }
              }
              ({
                j += 1; j - 1
              })
            }
          {
            var j: Int = 0
            var breakMe: Boolean = false

            while (!breakMe && j < figureWords.length) {
              {
                if (currentLineText.matches(figureWords(j))) {
                  LayoutUtils.setFeatureValue(ls, "startsFigureWord", 1.0)
                  breakMe = true;
                }
              }
              ({
                j += 1; j - 1
              })
            }
          }
          if (squishedLineText.matches(allCaps)) {
            LayoutUtils.setFeatureValue(ls, "allCaps", 1.0)
          }
          if (currentLineText.matches(endsInColon)) {
            LayoutUtils.setFeatureValue(ls, "endsInColon", 1.0)
          }
          if (currentLineText.matches(endsInEnum)) {
            LayoutUtils.setFeatureValue(ls, "endsInEnum", 1.0)
          }
          if (currentLineText.matches(contentsPattern)) {
            LayoutUtils.setFeatureValue(ls, "contentsPattern", 1.0)
          }
          if (currentLineText.matches(indexLinePattern)) {
            LayoutUtils.setFeatureValue(ls, "indexLinePattern", 1.0)
          }
          if (currentLineText.matches(initCap)) {
            LayoutUtils.setFeatureValue(ls, "startsCap", 1.0)
          }
          if (currentLineText.matches(finalDot)) {
            LayoutUtils.setFeatureValue(ls, "endsInDot", 1.0)
          }
          if (currentLineText.matches(finalDotAndNumber)) {
            LayoutUtils.setFeatureValue(ls, "endsInDotAndNumber", 1.0)
          }
          if (isUpFlagCount(currentLineText, ptrnLonelyLetters, 0.5)) {
            LayoutUtils.setFeatureValue(ls, "manyLonelyLetters", 1.0)
          }
          if (isUpFlagCount(currentLineText, ptrnLonelyNumbers, 0.5)) {
            LayoutUtils.setFeatureValue(ls, "manyLonelyNumbers", 1.0)
          }
          if (currentLineText.matches(firstLevelSection)) {
            LayoutUtils.setFeatureValue(ls, "firstLevelSectionPtrn", 1.0)
          }
          if (currentLineText.matches(secondLevelSection)) {
            LayoutUtils.setFeatureValue(ls, "secondLevelSectionPtrn", 1.0)
          }
          if (currentLineText.matches(thirdLevelSection)) {
            LayoutUtils.setFeatureValue(ls, "thirdLevelSectionPtrn", 1.0)
          }
          if (currentLineText.matches(noAlphabetical)) {
            LayoutUtils.setFeatureValue(ls, "noAlphabetic", 1.0)
          }
          if (ptrnWordForms.matcher(currentLineText).find) {
            LayoutUtils.setFeatureValue(ls, "1wordFormOrGreater", 1.0)
          }
          val mtchrWordWithSubindex: Matcher = ptrnWordWithSubindex.matcher(currentLineText)
          var counter: Int = 0
          while (mtchrWordWithSubindex.find) {
            counter += 1
          }
          if (counter >= 5) {
            LayoutUtils.setFeatureValue(ls, "wordsWithSubindex5OrMore", 1.0)
          }
          if (currentLineText.matches(startsEnum)) {
            LayoutUtils.setFeatureValue(ls, "startsEnum", 1.0)
          }
          LayoutUtils.adjustWordsInDictionaryPerLine(currentLineText, wordsInDictionaryPerLine, dictionary)
        }
        ({
          i += 1; i - 1
        })
      }


    wordsInDictionaryPerLine = wordsInDictionaryPerLine.sortWith(_.getQty > _.getQty);


    {
      var i: Int = 0
      while (i < data.getLineSpans.size) {
        {
          val ls: Span = data.getLineSpans.get(i) //.get.asInstanceOf[Span]
          val currentLineText: String = ls.getText.trim
          val mostCommonNumberOfDictWords: Int = wordsInDictionaryPerLine.get(0).get.getKey.asInstanceOf[Int]
          val currLineDictWords: Int = LayoutUtils.getWordsInDictionary(currentLineText, dictionary, true)
          if (currLineDictWords == 0) {
            LayoutUtils.setFeatureValue(ls, "noWordsFromDictionary", 1.0)
          }
          if (currLineDictWords == 1) {
            LayoutUtils.setFeatureValue(ls, "oneWordFromDictionary", 1.0)
          }
          if (mostCommonNumberOfDictWords > currLineDictWords) {
            LayoutUtils.setFeatureValue(ls, "1wordFromDictLess", 1.0)
          }
          if (mostCommonNumberOfDictWords > currLineDictWords + 1) {
            LayoutUtils.setFeatureValue(ls, "2wordFromDictLess", 1.0)
          }
          if (mostCommonNumberOfDictWords > currLineDictWords + 2) {
            LayoutUtils.setFeatureValue(ls, "3wordFromDictLess", 1.0)
          }
          if (mostCommonNumberOfDictWords > currLineDictWords + 3) {
            LayoutUtils.setFeatureValue(ls, "4wordFromDictLess", 1.0)
          }
          if (mostCommonNumberOfDictWords > currLineDictWords + 4) {
            LayoutUtils.setFeatureValue(ls, "5wordFromDictLess", 1.0)
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }

  private def isUpFlagCount(text: String, pattern: Pattern, ratioActivation: Double): Boolean = {
    val matcher: Matcher = pattern.matcher(text)
    var count: Int = 0
    while (matcher.find) {
      count += 1
    }
    if ( count.asInstanceOf[Double] /
           text.split(" ").length.asInstanceOf[Double] > ratioActivation) {
      return true
    }
    return false
  }
}

class Token2BodyFeatureSequence extends Pipe {
  def pipe(carrier: Instance): Instance = {
    Token2BodyFeatureSequence.wordsInDictionaryPerLine = MutableList[LayoutUtils.Entry]()
    val data: NewHtmlTokenization = carrier.getData.asInstanceOf[NewHtmlTokenization]
    val nhtml2LineInfo: NewHtmlTokenization2LineInfo = new NewHtmlTokenization2LineInfo
    val onlyLines: Instance = nhtml2LineInfo.pipe(carrier)
    val dictionary: EnglishDictionary = EnglishDictionary.createDefault(EnglishDictionary._defaultWords)
    computeFeatures(onlyLines.getData.asInstanceOf[Array[LineInfo]], data, dictionary)
    carrier.setData(data)
    return carrier
  }

  private def computeFeatures(lineInfos: Array[LineInfo], data: NewHtmlTokenization, dictionary: EnglishDictionary) {
    Token2BodyFeatureSequence.computeLexiconFeatures(data, dictionary)
    Token2BodyFeatureSequence.computeLayoutFeatures(lineInfos, data)
  }
}


