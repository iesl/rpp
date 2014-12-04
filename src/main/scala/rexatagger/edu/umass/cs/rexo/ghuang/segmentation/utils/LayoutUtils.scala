package edu.umass.cs.rexo.ghuang.segmentation.utils


import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
import org.rexo.extra.extract.{StringSpan, Span}
import org.rexo.span.CompositeSpan
import scala.collection.JavaConversions._
import edu.umass.cs.rexo.ghuang.segmentation.LineInfo
import org.rexo.util.EnglishDictionary
import scala.collection.mutable

/**
 * Created by klimzaporojets on 10/1/14.
 */

object LayoutUtils {
  def getProperty(span: AnyRef, property: String): Any = {
    if (span.isInstanceOf[StringSpan]) {
      return (span.asInstanceOf[StringSpan]).getProperty(property)
    }
    else if (span.isInstanceOf[CompositeSpan]) {
      return (span.asInstanceOf[CompositeSpan]).getProperty(property)
    }
    else if (span.isInstanceOf[NewHtmlTokenization]) {
      return (span.asInstanceOf[NewHtmlTokenization]).getProperty(property)
    }
    else {
      return null
    }
  }

  def isAnyOfFeaturesInFuture(data: NewHtmlTokenization, i: Int, features: mutable.MutableList[String], minOcurrences: Int, linesLookForward: Int): Boolean = {
    var qtyOcurrences: Int = 0
      var cnt: Int = i
      while (cnt < i + linesLookForward && cnt < data.getLineSpans.size) {
        {
          if (cnt >= data.getLineSpans.size) {
            return qtyOcurrences >= minOcurrences
          }
          val lineSpan: Span = data.getLineSpans.get(cnt)

          for (feature <- features) {
            if (isActiveFeature(lineSpan, feature)) {
              qtyOcurrences += 1
              return qtyOcurrences >= minOcurrences
            }
          }
        }
        ({
          cnt += 1; cnt - 1
        })
      }
    return qtyOcurrences >= minOcurrences
  }

  def isAnyOfFeaturesInFutureSvg(data: NewHtmlTokenizationSvg, i: Int, features: mutable.MutableList[String], minOcurrences: Int, linesLookForward: Int): Boolean = {
    var qtyOcurrences: Int = 0
    var cnt: Int = i
    while (cnt < i + linesLookForward && cnt < data.getLineSpans.size) {
      {
        if (cnt >= data.getLineSpans.size) {
          return qtyOcurrences >= minOcurrences
        }
        val lineSpan: Span = data.getLineSpans.get(cnt)

        for (feature <- features) {
          if (isActiveFeature(lineSpan, feature)) {
            qtyOcurrences += 1
            return qtyOcurrences >= minOcurrences
          }
        }
      }
      ({
        cnt += 1; cnt - 1
      })
    }
    return qtyOcurrences >= minOcurrences
  }

  def isFigureInTheFuture(data: NewHtmlTokenization, i: Int, linesLookForward: Int): Boolean = {
    var linesProcessed: Int = 0
      var cnt: Int = i
      while (cnt < i + linesLookForward && cnt < data.getLineSpans.size) {
        {
          linesProcessed += 1
          val lineSpan: Span = data.getLineSpans.get(cnt)
          if (lineSpan == null) {
            return true
          }
          var nextSpan: Span = null
          if (cnt + 1 < data.getLineSpans.size) {
            nextSpan = data.getLineSpans.get(cnt + 1)
          }
          if (cnt >= data.getLineSpans.size) {
            return false
          }
          if (linesProcessed == 1) {
            if (isActiveFeature(lineSpan, "rightMarginToTheLeft") && (nextSpan == null || isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
              return true
            }
          }
          else {
            if (!isActiveFeature(lineSpan, "shorterThanPreviousLine") && nextSpan != null && (isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
              return false
            }
            if (isActiveFeature(lineSpan, "sameLeftMarginAsPreviousLine") && isActiveFeature(lineSpan, "shorterThanPreviousLine") && (nextSpan == null || isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
              return true
            }
          }
        }
        ({
          cnt += 1; cnt - 1
        })
      }
    return false
  }

  def isFigureInTheFutureSvg(data: NewHtmlTokenizationSvg, i: Int, linesLookForward: Int): Boolean = {
    var linesProcessed: Int = 0
    var cnt: Int = i
    while (cnt < i + linesLookForward && cnt < data.getLineSpans.size) {
      {
        linesProcessed += 1
        val lineSpan: Span = data.getLineSpans.get(cnt)
        if (lineSpan == null) {
          return true
        }
        var nextSpan: Span = null
        if (cnt + 1 < data.getLineSpans.size) {
          nextSpan = data.getLineSpans.get(cnt + 1)
        }
        if (cnt >= data.getLineSpans.size) {
          return false
        }
        if (linesProcessed == 1) {
          if (isActiveFeature(lineSpan, "rightMarginToTheLeft") && (nextSpan == null || isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
            return true
          }
        }
        else {
          if (!isActiveFeature(lineSpan, "shorterThanPreviousLine") && nextSpan != null && (isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
            return false
          }
          if (isActiveFeature(lineSpan, "sameLeftMarginAsPreviousLine") && isActiveFeature(lineSpan, "shorterThanPreviousLine") && (nextSpan == null || isActiveFeature(nextSpan, "up") || isActiveFeature(nextSpan, "right") || isActiveFeature(lineSpan, "verticalDistance4pxGreater"))) {
            return true
          }
        }
      }
      ({
        cnt += 1; cnt - 1
      })
    }
    return false
  }

  def setFeatureValue(span: Span, property: String, value: Double) {
    if (span.isInstanceOf[CompositeSpan]) {
      (span.asInstanceOf[CompositeSpan]).setFeatureValue(property, value)
    }
    else {
      (span.asInstanceOf[StringSpan]).setFeatureValue(property, value)
    }
  }

  def isPropertySet(span: Span, property: String): Boolean = {
    if (span.isInstanceOf[CompositeSpan]) {
      return (span.asInstanceOf[CompositeSpan]).getNumericProperty(property) == 1.0
    }
    else {
      return (span.asInstanceOf[StringSpan]).getNumericProperty(property) == 1.0
    }
  }

  def isActiveFeature(span: Span, property: String): Boolean = {
    try {
      if (span.isInstanceOf[CompositeSpan]) {
        if ((span.asInstanceOf[CompositeSpan]).getFeatureValue(property) == 1.0) {
          return true
        }
      }
      else {
        if ((span.asInstanceOf[StringSpan]).getFeatureValue(property) == 1.0) {
          return true
        }
      }
      return false
    }
    catch {
      case ex: Exception => {
        ex.printStackTrace
        return false
      }
    }
  }

  def getCurrentLineColumn(lineInfos: Array[LineInfo], i: Int, columns: mutable.MutableList[LayoutUtils.ColumnData], equalsBothMargins: Boolean, acceptableMarginErrorLeft: Int, acceptableMarginErrorRight: Int, currentColumn: LayoutUtils.ColumnData, modeVerticalDistance: Int): LayoutUtils.ColumnData = {
    val lineInfo: LineInfo = lineInfos(i)
    val columnData: LayoutUtils.ColumnData = getColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, lineInfos(i))
    var sloppyColumn: LayoutUtils.ColumnData = null
    import scala.collection.JavaConversions._
    for (col <- columns) {
      val doesBelong: Boolean = doesBelongToColumnStrict(col, columnData)
      if (doesBelong) {
        return col
      }
      else if (doesBelongToColumnSloppy(col, columnData, false, false, -1) && doesBelongToColumnVert(col, columnData)) {
        return col
      }
      else if (doesBelongToColumnSloppy(col, columnData, false, false, -1)) {
        val distanceFromColumnVert: Int = getVerticalDistanceFromColumn(col, columnData)
        if (distanceFromColumnVert <= modeVerticalDistance + 2) {
          sloppyColumn = col
        }
      }
    }
    return sloppyColumn
  }

  def getClosestCurrentLineColumn(lineInfos: Array[LineInfo], i: Int, columns: mutable.MutableList[LayoutUtils.ColumnData], equalsBothMargins: Boolean, acceptableMarginErrorLeft: Int, acceptableMarginErrorRight: Int, strictLeft: Boolean, rightTops: Boolean, acceptableRightTopMarginError: Int): LayoutUtils.ColumnData = {
    val columnData: LayoutUtils.ColumnData = getColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, lineInfos(i))
    var colLeastDistance: LayoutUtils.ColumnData = null
    val distance: Int = -1
    import scala.collection.JavaConversions._
    for (col <- columns) {
      if (doesBelongToColumnSloppy(col, columnData, strictLeft, rightTops, acceptableRightTopMarginError)) {
        val currDistance: Int = getVerticalDistanceFromColumn(col, columnData)
        if (colLeastDistance == null || distance == -1 || currDistance < distance) {
          colLeastDistance = col
        }
      }
    }
    return colLeastDistance
  }

  private def doesBelongToColumnStrict(col: LayoutUtils.ColumnData, colToCompare: LayoutUtils.ColumnData): Boolean = {
    return col == colToCompare
  }

  private def doesBelongToColumnSloppy(col: LayoutUtils.ColumnData, colToCompare: LayoutUtils.ColumnData, strictLeft: Boolean, rightTops: Boolean, acceptableRightTopMarginError: Int): Boolean = {
    val relaxedColLeft: Int = col.getLeftX - col.getErrorMarginLeft
    val relaxedColRight: Int = col.getRightX + col.getErrorMarginRight
    val relaxedColRightLeft: Int = col.getRightX - col.getErrorMarginRight
    val relaxedColRightTops: Int = col.getRightX + acceptableRightTopMarginError
    if (!strictLeft && !rightTops && colToCompare.getLeftX > relaxedColLeft && colToCompare.getRightX < relaxedColRight) {
      return true
    }
    else if (strictLeft && !rightTops && (colToCompare.getLeftX >= col.getLeftX - 1 && colToCompare.getLeftX <= col.getLeftX + 1) && colToCompare.getRightX < relaxedColRight) {
      return true
    }
    else if (strictLeft && rightTops && (colToCompare.getLeftX >= col.getLeftX - 1 && colToCompare.getLeftX <= col.getLeftX + 1) && colToCompare.getRightX >= relaxedColRightLeft && colToCompare.getRightX <= relaxedColRightTops) {
      return true
    }
    else if (!strictLeft && rightTops && colToCompare.getLeftX > relaxedColLeft && colToCompare.getRightX >= relaxedColRightLeft && colToCompare.getRightX <= relaxedColRightTops) {
      return true
    }
    return false
  }

  def isRightMarginToTheLeft(lineInfo: LineInfo, columnData: LayoutUtils.ColumnData, margin: Int): Boolean = {
    if (columnData == null) {
      return false
    }
    if (lineInfo.urx <= columnData.getRightX - margin) {
      return true
    }
    return false
  }

  def isLeftMarginTabbed(lineInfo: LineInfo, columnData: LayoutUtils.ColumnData, margin: Int): Boolean = {
    if (columnData == null) {
      return false
    }
    if (lineInfo.llx >= columnData.getLeftX + margin) {
      return true
    }
    return false
  }

  def isShorterThanPrevious(lineInfo: Array[LineInfo], i: Int, margin: Int): Boolean = {
    var previousLine: LineInfo = null
    if (i <= 0 || lineInfo(i).page != lineInfo(i - 1).page) {
      return false
    }
    else {
      previousLine = lineInfo(i - 1)
    }
    val currentLine: LineInfo = lineInfo(i)
    if (previousLine.urx - previousLine.llx - margin > currentLine.urx - currentLine.llx) {
      return true
    }
    return false
  }

  def isSameLeftMarginAsPrevious(lineInfo: Array[LineInfo], i: Int, errMargin: Int): Boolean = {
    var previousLine: LineInfo = null
    if (i <= 0 || lineInfo(i).page != lineInfo(i - 1).page) {
      return false
    }
    else {
      previousLine = lineInfo(i - 1)
    }
    val currentLine: LineInfo = lineInfo(i)
    if (previousLine.llx - errMargin <= currentLine.llx && previousLine.llx + errMargin >= currentLine.llx) {
      return true
    }
    return false
  }

  private def doesBelongToColumnVert(col: LayoutUtils.ColumnData, colToCompare: LayoutUtils.ColumnData): Boolean = {
    if (colToCompare.getBottomY <=  col.getBottomY && colToCompare.getTopY >= col.getTopY) {
      return true
    }
    return false
  }

  private def getVerticalDistanceFromColumn(col: LayoutUtils.ColumnData, colToCompare: LayoutUtils.ColumnData): Int = {
    if (colToCompare.getBottomY  > col.getBottomY) {
      return colToCompare.getBottomY - col.getBottomY
    }
    else if (colToCompare.getTopY < col.getTopY) {
      return col.getTopY - colToCompare.getTopY
    }
    else {
      return -1
    }
  }

  def getColumns(allSpans: List[LayoutUtils.Entry], pageData: LayoutUtils.PageData): collection.mutable.MutableList[LayoutUtils.ColumnData] = {
    val columnList: collection.mutable.MutableList[LayoutUtils.ColumnData] = collection.mutable.MutableList[LayoutUtils.ColumnData]()
    val firstColumn: LayoutUtils.ColumnData = allSpans(0).getKey.asInstanceOf[LayoutUtils.ColumnData]
    columnList.+=(firstColumn)
    var widthSoFar: Int = firstColumn.getWidth
    if (firstColumn.getWidth > (pageData.getWidth.toDouble) / 2.0) {
      return columnList
    }
    for (colData <- allSpans) {
      if (!isOverlapping(columnList, colData.getKey.asInstanceOf[LayoutUtils.ColumnData]) && isWidthSimilar(columnList, colData.getKey.asInstanceOf[LayoutUtils.ColumnData], 0.05)) {
        columnList.+=(colData.getKey.asInstanceOf[LayoutUtils.ColumnData])
        widthSoFar += colData.getKey.asInstanceOf[LayoutUtils.ColumnData].getWidth
        if (widthSoFar + firstColumn.getWidth > pageData.getWidth) {
        }
      }
    }
    return columnList
  }

  def getColumnsV2(allSpans: mutable.MutableList[LayoutUtils.Entry], pageData: LayoutUtils.PageData): collection.mutable.MutableList[LayoutUtils.ColumnData] = {
    val columnList: collection.mutable.MutableList[LayoutUtils.ColumnData] = collection.mutable.MutableList[LayoutUtils.ColumnData]()
    val firstColumn: LayoutUtils.ColumnData = allSpans.get(0).get.getKey.asInstanceOf[LayoutUtils.ColumnData]
    if (firstColumn.getWidth > 50) {
      columnList.+=(firstColumn)
    }
    for (colData <- allSpans) {
      if (colData.qty >= 3 && contiguousCounterpartRatio(colData, 0.8) && smartOverlaps(columnList, colData.getKey.asInstanceOf[LayoutUtils.ColumnData]) && colData.key.asInstanceOf[LayoutUtils.ColumnData].getWidth > 50) {
        columnList.+=(colData.getKey.asInstanceOf[LayoutUtils.ColumnData])
      }
      if (colData.qty < 3) {
        return columnList
      }
    }
    return columnList
  }

  private def isEqualMargin(margin1: Int, margin2: Int, acceptedErr: Int): Boolean = {
    return (margin1 >= margin2 - acceptedErr && margin1 <= margin2 + acceptedErr)
  }

  private def smartOverlaps(columns: collection.mutable.MutableList[LayoutUtils.ColumnData], columnToCheck: LayoutUtils.ColumnData): Boolean = {
    var numberOverlapping: Int = 0
    var isSmart: Int = -1
    for (col <- columns) {
      if ((col.getLeftX >= columnToCheck.getLeftX && col.getLeftX <= columnToCheck.getRightX) || (col.getRightX >= columnToCheck.getLeftX && col.getRightX <= columnToCheck.getRightX) || (col.getLeftX <= columnToCheck.getLeftX && col.getRightX >= columnToCheck.getRightX) || (col.getLeftX >= columnToCheck.getLeftX && col.getRightX <= columnToCheck.getRightX)) {
        numberOverlapping += 1
        if (isEqualMargin(col.getLeftX, columnToCheck.getLeftX, 2) && ((col.getRightX > columnToCheck.getRightX && col.getRightX - columnToCheck.getRightX >= (col.getWidth.toDouble) * 0.15) || (col.getRightX < columnToCheck.getRightX && columnToCheck.getRightX - col.getRightX >= (columnToCheck.getWidth.toDouble ) * 0.15))) {
          isSmart = if (isSmart == -1) 1 else isSmart
        }
        else if (isEqualMargin(col.getRightX, columnToCheck.getRightX, 2) && ((col.getLeftX > columnToCheck.getLeftX && col.getLeftX - columnToCheck.getLeftX >= (columnToCheck.getWidth.toDouble) * 0.15) || (col.getLeftX < columnToCheck.getLeftX && columnToCheck.getLeftX - col.getLeftX >= (col.getWidth.toDouble ) * 0.15))) {
          isSmart = if (isSmart == -1) 1 else isSmart
        }
        else if (col.getBottomY < columnToCheck.getTopY || columnToCheck.getBottomY < col.getTopY) {
          isSmart = if (isSmart == -1) 1 else isSmart
        }
        else {
          isSmart = 0
        }
      }
    }
    if (numberOverlapping == 0 || (numberOverlapping == 1 && isSmart == 1)) {
      return true
    }
    return false
  }

  private def contiguousCounterpartRatio(columnData: LayoutUtils.Entry, ratio: Double): Boolean = {
    return (columnData.getKey.asInstanceOf[LayoutUtils.ColumnData].getContiguousCounterparts.toDouble) / (columnData.getQty.toDouble) >= ratio
  }

  private def isOverlapping(columns: collection.mutable.MutableList[LayoutUtils.ColumnData], columnToCheck: LayoutUtils.ColumnData): Boolean = {
    for (col <- columns) {
      if ((col.getLeftX >= columnToCheck.getLeftX && col.getLeftX <= columnToCheck.getRightX) || (col.getRightX >= columnToCheck.getLeftX && col.getRightX <= columnToCheck.getRightX) || (col.getLeftX <= columnToCheck.getLeftX && col.getRightX >= columnToCheck.getRightX) || (col.getLeftX >= columnToCheck.getLeftX && col.getRightX <= columnToCheck.getRightX)) {
        return true
      }
    }
    return false
  }

  def isNearTheTop(lineInfo: LineInfo, page: LayoutUtils.PageData, percentFromTop: Double): Boolean = {
    val diffTops: Int = lineInfo.ury - page.getTopY
    if ((diffTops.toDouble) / (page.getHeight.toDouble ) < percentFromTop) {
      return true
    }
    return false
  }

  def isNearTheTop(lineInfo: LineInfo, page: LayoutUtils.PageData, pixels: Int): Boolean = {
    val diffTops: Int = lineInfo.ury - page.getTopY
    if (diffTops <= pixels) {
      return true
    }
    return false
  }

  def isNearTheBottom(lineInfo: LineInfo, page: LayoutUtils.PageData, pixels: Int): Boolean = {
    val diffTops: Int = page.getBottomY - lineInfo.lly
    if (diffTops <= pixels) {
      return true
    }
    return false
  }

  private def isWidthSimilar(columns: collection.mutable.MutableList[LayoutUtils.ColumnData], columnToCheck: LayoutUtils.ColumnData, errorRatio: Double): Boolean = {
    for (col <- columns) {
      if (columnToCheck.getWidth < (col.getWidth.toDouble ) * (1.0 - errorRatio) || columnToCheck.getWidth > (col.getWidth.toDouble) * (1.0 + errorRatio)) {
        return false
      }
    }
    return true
  }

  def adjustPageData(urx: Int, llx: Int, ury: Int, lly: Int, page: Int, pagesData: collection.mutable.Map[Int, LayoutUtils.PageData]) {
    var pageData: LayoutUtils.PageData = pagesData.get(page).getOrElse(null) //pagesData.get(page)
    if (pageData == null) {
      pageData = new LayoutUtils.PageData
      pageData.setBottomY(lly)
      pageData.setTopY(ury)
      pageData.setLeftX(llx)
      pageData.setRightX(urx)
      pageData.setPageNumber(page)
      pagesData.put(page, pageData)
    }
    else {
      pageData.setBottomY(if (pageData.getBottomY /*>*/ < lly) lly else pageData.getBottomY)
      pageData.setTopY(if (pageData.getTopY /*<*/ > ury) ury else pageData.getTopY)
      pageData.setLeftX(if (pageData.getLeftX > llx) llx else pageData.getLeftX)
      pageData.setRightX(if (pageData.getRightX < urx) urx else pageData.getRightX)
    }
  }

  def adjustPageData(lineInfos: Array[LineInfo], i: Int, pagesData: collection.mutable.Map[Int, LayoutUtils.PageData]) {
    adjustPageData(lineInfos(i).urx, lineInfos(i).llx, lineInfos(i).ury, lineInfos(i).lly, lineInfos(i).page, pagesData)
  }

  def adjustLineHeight(lineInfos: Array[LineInfo], i: Int, lineHeight: collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/]) {
    val height: Int = getCurrentLineHeight(lineInfos, i)
    val currentHeightEntry: LayoutUtils.Entry = new LayoutUtils.Entry(height, 1)
    val iOf: Int = lineHeight.indexOf(currentHeightEntry)
    if (iOf > -1) {
      val actualData: LayoutUtils.Entry = lineHeight(iOf)
      actualData.setQty(actualData.getQty + 1)
    }
    else {
      lineHeight.+=(currentHeightEntry)
    }
  }

  def getCurrentLineHeight(lineInfos: Array[LineInfo], i: Int): Int = {
    return lineInfos(i).lly - lineInfos(i).ury
  }

  def isCentered(lineInfo: LineInfo, columnLeftMargin: Int, columnRightMargin: Int, errRatio: Int): Boolean = {
    val leftDiff: Int = lineInfo.llx - columnLeftMargin
    val rightDiff: Int = columnRightMargin - lineInfo.urx
    if (leftDiff >= 10 && rightDiff >= 10 && leftDiff >= rightDiff - errRatio && leftDiff <= rightDiff + errRatio) {
      return true
    }
    return false
  }

  def getWordsInDictionary(lineOfText: String, dictionary: EnglishDictionary, exact: Boolean): Int = {
    val cleanedText: String = lineOfText.replaceAll("[,\\.\\(\\)\\[\\]]", "").toLowerCase
    val tokenized: Array[String] = cleanedText.split(" ")
    val tokSet: collection.mutable.Set[String] = collection.mutable.Set[String]() //new HashSet[String]
    for (word <- tokenized) {
      tokSet.add(word.trim)
    }
    var cont: Int = 0
    if (tokenized.length < 3 && !exact) {
      return -1
    }
    for (word <- tokSet) {
      if (dictionary.contains(word)) {
        cont += 1
      }
    }
    return cont
  }

  def adjustWordsInDictionaryPerLine(currentLineText: String, wordsInDictionaryPerLine: collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/], dictionary: EnglishDictionary) {
    val dictWordsInLine: Int = getWordsInDictionary(currentLineText, dictionary, false)
    if (dictWordsInLine == -1) {
      return
    }
    val currEntry: LayoutUtils.Entry = new LayoutUtils.Entry(dictWordsInLine, 1)
    val iOf: Int = wordsInDictionaryPerLine.indexOf(currEntry)
    if (iOf > -1) {
      val actualData: LayoutUtils.Entry = wordsInDictionaryPerLine(iOf)
      actualData.setQty(actualData.getQty + 1)
    }
    else {
      wordsInDictionaryPerLine.+=(currEntry)
    }
  }

  def getPixelsPerCharacter(lineInfos: Array[LineInfo], i: Int): Int = {
    val width: Int = lineInfos(i).urx - lineInfos(i).llx
    val text: String = lineInfos(i).text
    if (width == 1) {
      return -1
    }
    val pxlsXCharacter: Int = Math.round((width.toDouble) / (text.length.toDouble)).toInt
    return pxlsXCharacter
  }

  def adjustPixelsPerCharacter(lineInfos: Array[LineInfo], i: Int, pixelsPerCharacter: collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/]) {
    val pxlsXCharacter: Int = getPixelsPerCharacter(lineInfos, i)
    if (pxlsXCharacter == -1) {
      return
    }
    val currEntry: LayoutUtils.Entry = new LayoutUtils.Entry(pxlsXCharacter, 1)
    val iOf: Int = pixelsPerCharacter.indexOf(currEntry)
    if (iOf > -1) {
      val actualData: LayoutUtils.Entry = pixelsPerCharacter(iOf)
      actualData.setQty(actualData.getQty + 1)
    }
    else {
      pixelsPerCharacter.+=(currEntry)
    }
  }

  def adjustLineWidth(lineInfos: Array[LineInfo], i: Int, lineWidth: collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/]) {
    val width: Int = getCurrentLineWidth(lineInfos, i)
    if (width <= 10) {
      return
    }
    val currentWidthEntry: LayoutUtils.Entry = new LayoutUtils.Entry(width, 1)
    val iOf: Int = lineWidth.indexOf(currentWidthEntry)
    if (iOf > -1) {
      val actualData: LayoutUtils.Entry = lineWidth(iOf)
      actualData.setQty(actualData.getQty + 1)
    }
    else {
      lineWidth.+=(currentWidthEntry)
    }
  }

  def getCurrentLineWidth(lineInfos: Array[LineInfo], i: Int): Int = {
    return lineInfos(i).urx - lineInfos(i).llx
  }

  def adjustLineWidthPerPage(lineInfos: Array[LineInfo], i: Int, widthLinePerPage: collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/]]) {
    val lineInfo: LineInfo = lineInfos(i)
    var entry: collection.mutable.MutableList[LayoutUtils.Entry] = widthLinePerPage.get(lineInfo.page).getOrElse(null)
    if (entry == null) {
      entry = collection.mutable.MutableList[LayoutUtils.Entry]()
    }
    adjustLineWidth(lineInfos, i, entry)
    widthLinePerPage.put(lineInfo.page, entry)
  }

  def getCurrentVerticalDistance(lineInfos: Array[LineInfo], i: Int): Int = {
    if (i + 1 < lineInfos.length && lineInfos(i).page == lineInfos(i + 1).page && lineInfos(i).lly < lineInfos(i + 1).lly) {
      return (lineInfos(i + 1).lly - lineInfos(i).lly)
    }
    return -1
  }

  def getCurrentVerticalDistanceUry(lineInfos: Array[LineInfo], i: Int): Int = {
    if (i + 1 < lineInfos.length && lineInfos(i).page == lineInfos(i + 1).page && lineInfos(i).ury < lineInfos(i + 1).ury) {
      return (lineInfos(i + 1).ury - lineInfos(i).ury)
    }
    return -1
  }

  def adjustVerticalDistance(lineInfos: Array[LineInfo], i: Int, verticalDistance: collection.mutable.MutableList[LayoutUtils.Entry/*[Integer]*/]) {
    if (i + 1 < lineInfos.length && lineInfos(i).page == lineInfos(i + 1).page && lineInfos(i).lly < lineInfos(i + 1).lly) {
      val vertDistance: Int = lineInfos(i + 1).lly - lineInfos(i).lly
      val initialEntry: LayoutUtils.Entry = new LayoutUtils.Entry (vertDistance, 1)
      val iOf: Int = verticalDistance.indexOf(initialEntry)
      if (iOf > -1) {
        val exEntry: LayoutUtils.Entry = verticalDistance(iOf)
        exEntry.setQty(exEntry.getQty + 1)
      }
      else {
        verticalDistance.+=(initialEntry)
      }
    }
  }

  private def getColumnData(equalsBothMargins: Boolean, acceptableMarginErrorLeft: Int, acceptableMarginErrorRight: Int, lineInfo: LineInfo): LayoutUtils.ColumnData = {
    val columnData: LayoutUtils.ColumnData = new LayoutUtils.ColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight)
    columnData.setLeftX(lineInfo.llx)
    columnData.setRightX(lineInfo.urx)
    columnData.setTopY(lineInfo.ury)
    columnData.setBottomY(lineInfo.lly)
    return columnData
  }

  def checkCounterparts(equalsBothMargins: Boolean, acceptableMarginErrorLeft: Int, acceptableMarginErrorRight: Int, columnData: LayoutUtils.ColumnData, lineInfos: Array[LineInfo], i: Int) {
    var columnData1: LayoutUtils.ColumnData = null
    if (i > 0) {
      columnData1 = getColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, lineInfos(i - 1))
      if (columnData == columnData1) {
        columnData.incrementContiguous
        return
      }
    }
    if (i < lineInfos.length - 1) {
      columnData1 = getColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, lineInfos(i + 1))
      if (columnData == columnData1) {
        columnData.incrementContiguous
        return
      }
    }
  }

  def adjustColumnData(lineInfos: Array[LineInfo], i: Int,
                       columnsData: collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]],
                       equalsBothMargins: Boolean, acceptableMarginErrorLeft: Int, acceptableMarginErrorRight: Int, currentSpan: Span) {
    val columnData: LayoutUtils.ColumnData = getColumnData(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, lineInfos(i))
    if (columnsData.get(lineInfos(i).page).getOrElse(null) == null) {
      val colData: collection.mutable.MutableList[LayoutUtils.Entry] = collection.mutable.MutableList[LayoutUtils.Entry]()
      colData.+=(new LayoutUtils.Entry(columnData, 1))
      checkCounterparts(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight, columnData, lineInfos, i)
      columnsData.put(lineInfos(i).page, colData)
    }
    else {
      val currEntry: LayoutUtils.Entry = new LayoutUtils.Entry(columnData, 1)
      val entriesInThePage: collection.mutable.MutableList[LayoutUtils.Entry] = columnsData.get(lineInfos(i).page).get
      val iOe: Int = entriesInThePage.indexOf(currEntry)
      if (iOe > -1) {
        val existentEntry: LayoutUtils.Entry = columnsData.get(lineInfos(i).page).get(iOe)
        existentEntry.setQty(existentEntry.getQty + 1)
        if (lineInfos(i).ury < existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].getTopY) {
          existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].setTopY(lineInfos(i).ury)
        }
        if (lineInfos(i).lly > existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].getBottomY) {
          existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].setBottomY(lineInfos(i).lly)
        }
        //TODO: check if it works, now tsting with result_test18.html, worked well on result_test18.html!,
        //TODO: any change to the following if should also work well with result_test18.html
        if (lineInfos(i).urx > existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].getRightX)
        {
          //only do when column width not greater than max allowed per collumn?
          existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData].setRightX(lineInfos(i).urx)
        }
        checkCounterparts(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight,
          existentEntry.getKey.asInstanceOf[LayoutUtils.ColumnData], lineInfos, i)
      }
      else {
        checkCounterparts(equalsBothMargins, acceptableMarginErrorLeft, acceptableMarginErrorRight,
              currEntry.getKey.asInstanceOf[LayoutUtils.ColumnData], lineInfos, i)
        columnsData.get(lineInfos(i).page).get.+=(currEntry)
      }
    }
  }

  class Entry extends Ordered[LayoutUtils.Entry] {
    private[utils] var key: Any = null
    private[utils] var qty: Int = 0

    def this(key: Any, qty: Int) {
      this()
      this.key = key
      this.qty = qty
    }

    def getQty: Int = {
      return qty
    }

    def setQty(qty: Int) {
      this.qty = qty
    }

    def getKey: Any = {
      return key
    }

    def setKey(key: Any) {
      this.key = key
    }



    override def equals(obj: Any) = obj match {
      case that: Entry =>
      {
        (obj.asInstanceOf[LayoutUtils.Entry]).getKey == this.key
      }
      case _ => false
    }

    override def hashCode = key.hashCode()
    override def compare(entry: LayoutUtils.Entry): Int = {
      val otherQty: Int = entry.getQty
      if (this.qty > otherQty) {
        return -1
      }
      else if (this.qty == otherQty) {
        return 0
      }
      else {
        return 1
      }
    }

  }

  class ColumnData {
    private var topY: Int = -1
    private var bottomY: Int = -1
    private var leftX: Int = -1
    private var rightX: Int = -1
    private[utils] var equalsBothMargins: Boolean = false
    private[utils] var errorMarginLeft: Int = 0
    private[utils] var errorMarginRight: Int = 0
    private[utils] var contiguousCounterparts: Int = 0

    def getErrorMarginLeft: Int = {
      return errorMarginLeft
    }

    def getErrorMarginRight: Int = {
      return errorMarginRight
    }

    def incrementContiguous {
      contiguousCounterparts += 1
    }

    def getContiguousCounterparts: Int = {
      return contiguousCounterparts
    }



    def this(equalsBothMargins: Boolean, errorMarginLeft: Int, errorMarginRight: Int) {
      this()
      this.equalsBothMargins = equalsBothMargins
      this.errorMarginLeft = errorMarginLeft
      this.errorMarginRight = errorMarginRight
    }

    def isInitialized: Boolean = {
      return !(topY == -1 && bottomY == -1 && leftX == -1 && rightX == -1)
    }

    def isEqualsBothMargins: Boolean = {
      return equalsBothMargins
    }

    def setEqualsBothMargins(equalsBothMargins: Boolean) {
      this.equalsBothMargins = equalsBothMargins
    }

    def getTopY: Int = {
      return topY
    }

    def setTopY(topY: Int) {
      this.topY = topY
    }

    def getBottomY: Int = {
      return bottomY
    }

    def setBottomY(bottomY: Int) {
      this.bottomY = bottomY
    }

    def getLeftX: Int = {
      return leftX
    }

    def setLeftX(leftX: Int) {
      this.leftX = leftX
    }

    def getRightX: Int = {
      return rightX
    }

    def setRightX(rightX: Int) {
      this.rightX = rightX
    }

    def getWidth: Int = {
      return (rightX - leftX)
    }


    override def equals(obj: Any) = obj match {
      case that: ColumnData =>  {
        if (!equalsBothMargins) {
          (obj.asInstanceOf[LayoutUtils.ColumnData]).leftX == this.leftX
        }
        else {
          ((obj.asInstanceOf[LayoutUtils.ColumnData]).rightX == this.rightX && (obj.asInstanceOf[LayoutUtils.ColumnData]).leftX == this.leftX) || ((obj.asInstanceOf[LayoutUtils.ColumnData]).rightX == this.rightX && ((obj.asInstanceOf[LayoutUtils.ColumnData]).leftX >= this.leftX - errorMarginLeft && (obj.asInstanceOf[LayoutUtils.ColumnData]).leftX <= this.leftX + errorMarginLeft)) || (((obj.asInstanceOf[LayoutUtils.ColumnData]).rightX >= this.rightX - errorMarginRight && (obj.asInstanceOf[LayoutUtils.ColumnData]).rightX <= this.rightX + errorMarginRight) && ((obj.asInstanceOf[LayoutUtils.ColumnData]).leftX >= this.leftX - 1 && (obj.asInstanceOf[LayoutUtils.ColumnData]).leftX <= this.leftX + 1))
        }
      }
      case _ => false
    }



    override def hashCode = Integer.valueOf(leftX).hashCode



  }

  class PageData {
    private var topY: Int = 0
    private var bottomY: Int = 0
    private var leftX: Int = 0
    private var rightX: Int = 0
    private var pageNumber: Int = 0
    def getPageNumber: Int = {
      return pageNumber
    }

    def setPageNumber(pageNumber: Int) {
      this.pageNumber = pageNumber
    }

    def getTopY: Int = {
      return topY
    }

    def setTopY(topY: Int) {
      this.topY = topY
    }

    def getBottomY: Int = {
      return bottomY
    }

    def setBottomY(bottomY: Int) {
      this.bottomY = bottomY
    }

    def getLeftX: Int = {
      return leftX
    }

    def setLeftX(leftX: Int) {
      this.leftX = leftX
    }

    def getRightX: Int = {
      return rightX
    }

    def setRightX(rightX: Int) {
      this.rightX = rightX
    }

    def getWidth: Int = {
      return (rightX - leftX)
    }

    def getHeight: Int = {
      return (bottomY - topY)
    }


  }

}

