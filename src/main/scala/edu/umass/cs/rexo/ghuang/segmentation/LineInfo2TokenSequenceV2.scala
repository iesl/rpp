package edu.umass.cs.rexo.ghuang.segmentation

import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils
import org.rexo.base.{Instance, Pipe}
import org.rexo.extra.types.{TokenSequence, Token}
import edu.umass.cs.rexo.ghuang.segmentation.LineInfo2TokenSequenceV2.{EnumerationType, IndentationType}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by klimzaporojets on 10/6/14.
 */
object LineInfo2TokenSequenceV2 {

  private def getSortedWidths(posX: Int, lineInfos: Array[LineInfo]): Array[Int] = {
    var sortedList = ArrayBuffer[Int]()
    lineInfos.foreach{lineInfo =>
      if (lineInfo.llx == posX)
        sortedList += lineInfo.urx - lineInfo.llx
    }
    sortedList.sortWith(_.intValue() > _.intValue()).toArray
  }

  private def allElementsTrue(arrBool: Array[Boolean]): Boolean = {
    var i = 0
    while(i < arrBool.length){
      if(!arrBool(i)) return false
      i += 1
    }
    true
  }

  private def getColumns(allSpans: List[LayoutUtils.Entry],
                         allLeftMargins: collection.mutable.MutableList[LayoutUtils.Entry],
                         pageData: LayoutUtils.PageData, firstLine: LineInfo,
                         isFirstLinePage: Boolean, indentationType: Int /*LineInfo2TokenSequenceV2.IndentationType*/,
                         lineInfos: Array[LineInfo]): collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.ColumnData]] = {
    var oneColumn: Boolean = false
    val pageWidth: Int = pageData.getWidth
    var maxColumnWidth: Int = -1
    var minColumnWidth: Int = -1
    if ((firstLine.urx - firstLine.llx) > pageWidth / 2) {
      oneColumn = true
      maxColumnWidth = pageData.getWidth
      minColumnWidth = Math.ceil(pageData.getWidth.toDouble / 1.5).toInt
    }
    else {
      maxColumnWidth = pageData.getWidth / 2 + 10
      minColumnWidth = pageData.getWidth / 3
    }
    val numberOfColumns = if(oneColumn) 1 else 2
    val firstIndentation = Array.fill(numberOfColumns)(new LayoutUtils.ColumnData)
    val secondIndentation = Array.fill(numberOfColumns)(new LayoutUtils.ColumnData)

    val completedIndentations = new Array[Boolean](4)
    var count = 0

    for (entry <- allLeftMargins if !allElementsTrue(completedIndentations)) {
      val sortW = getSortedWidths(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX, lineInfos)
      var currColumn = -1
      var idealWidth = 0
      var breakWhile = false
      while (!breakWhile && idealWidth < sortW.length) {
        if (sortW(idealWidth) <= maxColumnWidth) breakWhile = true
        if(!breakWhile) idealWidth += 1
      }
      idealWidth = if (idealWidth >= sortW.size) sortW.length - 1 else idealWidth
      if (sortW(idealWidth) > minColumnWidth && sortW(idealWidth) <= maxColumnWidth) {
        if (numberOfColumns > 1) {
          if ((entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX - sortW(idealWidth) <
                pageData.getLeftX && entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX + sortW(idealWidth) * 2 - 10 < pageData.getRightX)) {
            currColumn = 0
          }
          if (entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX - sortW(idealWidth) + 10 >
                pageData.getLeftX && entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX + sortW(idealWidth) * 2 - 10 > pageData.getRightX) {
            currColumn = 1
          }
        }
        else {
          currColumn = 0
        }
        if(!(currColumn == -1)){
          if (!firstIndentation(currColumn).isInitialized) {
            firstIndentation(currColumn).setLeftX(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX)
            firstIndentation(currColumn).setRightX(sortW(idealWidth) + entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX)
            completedIndentations(currColumn * 2 + 0) = true
          }
          else {
            if (firstIndentation(currColumn).getLeftX > entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX) {
              if (firstIndentation(currColumn).getLeftX - entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX < 10) {
                firstIndentation(currColumn).setLeftX(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX)
                //the previous if was commented because of the fact that it worked bad in documents such as
                if(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getRightX > firstIndentation(currColumn).getRightX){
                  firstIndentation(currColumn).setRightX(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getRightX)
                }
              }
              else {
                secondIndentation(currColumn) = firstIndentation(currColumn)
                firstIndentation(currColumn) = new LayoutUtils.ColumnData
                firstIndentation(currColumn).setLeftX(entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX)
                firstIndentation(currColumn).setRightX(sortW(idealWidth) + entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX)
                completedIndentations(currColumn * 2 + 1) = true
              }
            }
            else if (firstIndentation(currColumn).getLeftX < entry.getKey.asInstanceOf[LayoutUtils.ColumnData].getLeftX) {
//              println("inside commented if")
            }
          }
        }
      }
      count += 1
    }

    val toRMap = collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.ColumnData]]()
    var i = 0
    while (i < numberOfColumns) {
      val toReturn = collection.mutable.MutableList[LayoutUtils.ColumnData]() //new ArrayList[LayoutUtils.ColumnData]
      if (firstIndentation(i).isInitialized) toReturn += firstIndentation(i)
      if (secondIndentation(i).isInitialized) toReturn += secondIndentation(i)
      toRMap.put(i, toReturn)
      i += 1
    }
    toRMap
  }

  private def refEndsInPoint(i: Int, lineInfos: Array[LineInfo]): Int =
    if (i > 0 && !lineInfos(i - 1).presentFeatures.contains("bibliography")
      && !lineInfos(i - 1).presentFeatures.contains("noEndingPeriod")) 1 else 0

  private def refNotEndsInPoint(i: Int, lineInfos: Array[LineInfo]): Int =
    if (i > 0 && !lineInfos(i - 1).presentFeatures.contains("bibliography")
      && lineInfos(i - 1).presentFeatures.contains("noEndingPeriod")) 1 else 0

  private def vertDifFromPrevRef(i: Int, lineInfos: Array[LineInfo]): Int =
    if (i > 0 && !lineInfos(i - 1).presentFeatures.contains("bibliography")
        && lineInfos(i).page == lineInfos(i - 1).page
        && !lineInfos(i).presentFeatures.contains("newColumn")
        && lineInfos(i).lly > lineInfos(i - 1).lly)
      lineInfos(i).lly - lineInfos(i - 1).lly else 0

  private def specialPunctCounter(s: String): Int = {
    var count = 0
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c == ',' || c == '.' || c == ':') count += 1
      i += 1
    }
    return count
  }

  private final val serialVersionUID: Long = 1L

  private final object EnumerationType {
    final val PARENTHESIS:Int = 0
    final val SQUARE_BRACKETS:Int = 1
    final val NUMBER_CAPITAL:Int = 2
    final val CAP_INITIALS:Int = 3
    final val NONE:Int = 4
  }

  private final object IndentationType {
    final val INDENTED:Int = 0
    final val UNTABBED:Int = 1
    final val SAME:Int = 2
  }

}

class LineInfo2TokenSequenceV2 extends Pipe with Serializable {

  def pipe(carrier: Instance): Instance = {
    val oldData: Array[LineInfo] = carrier.getData.asInstanceOf[Array[LineInfo]]
    val dataTokens: Array[Token] = new Array[Token](oldData.length)
    val sourceTokens: Array[Token] = new Array[Token](oldData.length)
    try {
      computeFeatures(oldData)
    }
    catch {
      case ex: Exception => ex.printStackTrace
    }
    var i = 0
    while (i < oldData.length) {
      dataTokens(i) = new Token(oldData(i).text)
      sourceTokens(i) = new Token(oldData(i).text)
      val iter = oldData(i).presentFeatures.iterator
      while (iter.hasNext) {
        val featName = iter.next
        dataTokens(i).setFeatureValue(featName, 1)
//        TODO: delete this toString later
//        dataTokens(i).toString
      }
//      println(s"p2 data (ignore=${dataTokens(i).getFeatures.hasProperty("ignore")}): ${dataTokens(i).text}")
      dataTokens(i).setProperty("blockId", oldData(i).blockId)
      dataTokens(i).setProperty("page", oldData(i).page)
      i += 1
    }
    val dataTS = TokenSequence(dataTokens)
    val sourceTS = TokenSequence(sourceTokens)
    carrier.setData(dataTS)
    carrier.setSource(sourceTS)
    if (isTargetProcessing) {
      val targetTokens = new Array[Token](dataTokens.length)
      var i = 0
      while (i < dataTokens.length) {
        targetTokens(i) = new Token(oldData(i).trueLabel)
        i += 1
      }
      val targetTS = TokenSequence(targetTokens)
      carrier.setTarget(targetTS)
    }
    carrier
  }

  private def computeFeatures(lineInfos: Array[LineInfo]) {
    computeLexiconFeatures(lineInfos)
    computeLayoutFeatures(lineInfos)
  }

  private def computeLayoutFeatures(lineInfos: Array[LineInfo]) {
    var verticalDistance = collection.mutable.MutableList[LayoutUtils.Entry]()

    var widthLine = collection.mutable.MutableList[LayoutUtils.Entry]()

    val columnsData = collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.Entry]]()
    val pagesData = collection.mutable.Map[Int, LayoutUtils.PageData]()

    var prevPageNum: Int = 0
    var prevFontNum: String = ""

    var sumLineLengths: Int = 0
    val dist2prevLine = new Array[Int](lineInfos.length)
    val refFontCounts = collection.mutable.Map[String, Int]()
    var indentedAfterFirst: Int = 0
    var untabbedAfterFirst: Int = 0
    var sameAfterFirst: Int = 0
    var firstLine: LineInfo = null
    var enumerationType: Int = EnumerationType.NONE

    var i = 0
    while (i < lineInfos.length) {
      if (lineInfos(i).presentFeatures.contains("firstReferenceLine")) {
        firstLine = lineInfos(i)
      }
      if (lineInfos(i).presentFeatures.contains("firstReferenceLine") && lineInfos(i).presentFeatures.contains("seqHasBeginSquareBrackets") && lineInfos(i).presentFeatures.contains("beginSquareBrackets")) {
        enumerationType = EnumerationType.SQUARE_BRACKETS
      }
      else if (lineInfos(i).presentFeatures.contains("firstReferenceLine") && lineInfos(i).presentFeatures.contains("seqHasBeginParenthesis") && lineInfos(i).presentFeatures.contains("beginParenthesis")) {
        enumerationType = EnumerationType.PARENTHESIS
      }
      else if (lineInfos(i).presentFeatures.contains("firstReferenceLine") && lineInfos(i).presentFeatures.contains("seqHasBeginNumberCapital") && lineInfos(i).presentFeatures.contains("beginsNumberCapital")) {
        enumerationType = EnumerationType.NUMBER_CAPITAL
      }
      if ((enumerationType == EnumerationType.SQUARE_BRACKETS && lineInfos(i).presentFeatures.contains("beginSquareBrackets")) || (enumerationType == EnumerationType.PARENTHESIS && lineInfos(i).presentFeatures.contains("beginParenthesis")) || (enumerationType == EnumerationType.NUMBER_CAPITAL && lineInfos(i).presentFeatures.contains("beginsNumberCapital"))) {
        lineInfos(i).presentFeatures.add("samePatternAsInFirst")
      }
      if (lineInfos(i).page != prevPageNum) {
        lineInfos(i).presentFeatures.add("newPage")
        prevPageNum = lineInfos(i).page
        if (i > 0) lineInfos(i - 1).presentFeatures.add("lastLineOnPage")
      }
      else if (i > 0 && (lineInfos(i).llx >= lineInfos(i - 1).urx
              && lineInfos(i).lly < lineInfos(i - 1).lly))
        lineInfos(i).presentFeatures.add("newColumn")
      else if (i > 0 && lineInfos(i).llx > lineInfos(i - 1).llx && (lineInfos(i).llx - lineInfos(i - 1).llx) > 2) {
        lineInfos(i).presentFeatures.add("indentedFromPrevLine")
        if (lineInfos(i - 1).presentFeatures.contains("samePatternAsInFirst")) {
          indentedAfterFirst += 1
        }
      }
      else if (i > 0 && lineInfos(i).llx < lineInfos(i - 1).llx && (lineInfos(i - 1).llx - lineInfos(i).llx) > 2) {
        lineInfos(i).presentFeatures.add("unTabbedFromPrevLine")
        if (lineInfos(i - 1).presentFeatures.contains("samePatternAsInFirst") && (!lineInfos(i).presentFeatures.contains("samePatternAsInFirst"))) {
          untabbedAfterFirst += 1
        }
      }
      else if (i > 0 && lineInfos(i).llx == lineInfos(i - 1).llx) {
        lineInfos(i).presentFeatures.add("sameIndentationAsPrevLine")
        if (lineInfos(i - 1).presentFeatures.contains("samePatternAsInFirst") && (!lineInfos(i).presentFeatures.contains("samePatternAsInFirst"))) {
          sameAfterFirst += 1
        }
      }
      if (i > 0 && lineInfos(i).lly == lineInfos(i - 1).lly && lineInfos(i).llx > lineInfos(i - 1).llx && lineInfos(i).urx > lineInfos(i - 1).urx) {
        lineInfos(i).presentFeatures.add("sameLine")
      }
      LayoutUtils.adjustLineWidth(lineInfos, i, widthLine)
      LayoutUtils.adjustPageData(lineInfos, i, pagesData)
      LayoutUtils.adjustColumnData(lineInfos, i, columnsData, false, 0, 0, null)
      LayoutUtils.adjustVerticalDistance(lineInfos, i, verticalDistance)
      if (lineInfos(i).multibox) lineInfos(i).presentFeatures.add("containsMultiFonts")
      if (i == 0) prevFontNum = lineInfos(i).font
      else if (lineInfos(i).font != prevFontNum) {
        prevFontNum = lineInfos(i).font
        lineInfos(i).presentFeatures.add("startsNewFont")
      }
      if (lineInfos(i).presentFeatures.contains("beginBrackets") || (!lineInfos(i).presentFeatures.contains("seqHasBeginBrackets") && lineInfos(i).presentFeatures.contains("beginsNumberCapital")) || (!lineInfos(i).presentFeatures.contains("seqHasBeginBrackets") && !lineInfos(i).presentFeatures.contains("seqHasBeginNumberCapital") && lineInfos(i).presentFeatures.contains("beginsCapitalInitials"))) {
        val fontNum = lineInfos(i).font
        if (!refFontCounts.contains(fontNum)) refFontCounts.put(fontNum, 0)
        refFontCounts.put(fontNum, refFontCounts(fontNum) + 1)
      }
      sumLineLengths += lineInfos(i).urx - lineInfos(i).llx
      if (i > 0) dist2prevLine(i) = Math.abs(lineInfos(i).lly - lineInfos(i - 1).lly)
      i += 1
    }

    var indentationType: Int = LineInfo2TokenSequenceV2.IndentationType.SAME
    if ((sameAfterFirst.toDouble / (sameAfterFirst + indentedAfterFirst + untabbedAfterFirst).toDouble) > 0.5) {
      indentationType = IndentationType.SAME
    }
    else if (((indentedAfterFirst).toDouble /
         (sameAfterFirst + indentedAfterFirst + untabbedAfterFirst).toDouble) > 0.5) {
      indentationType = IndentationType.INDENTED
    }
    else if (((indentedAfterFirst).toDouble /
      (sameAfterFirst + indentedAfterFirst + untabbedAfterFirst).toDouble) > 0.5) {
      indentationType = IndentationType.UNTABBED
    }
    val tolerance: Int = 1
    val avgLineLength: Double = sumLineLengths / lineInfos.length
    var refFont: String = ""
    var maxCount: Int = 0
    val iter = refFontCounts.keySet.iterator
    while (iter.hasNext) {
      val fontNum = iter.next
      val count = refFontCounts(fontNum)
      if (count > maxCount) {
        maxCount = count
        refFont = fontNum
      }
    }
    verticalDistance = verticalDistance.sortWith(_.getQty > _.getQty)
    widthLine = widthLine.sortWith(_.getQty > _.getQty)

    /* This is just sorting each entry */
    val colsPerPage = collection.mutable.Map[Int, collection.mutable.Map[Int, collection.mutable.MutableList[LayoutUtils.ColumnData]]]()
    columnsData.keySet.foreach{ key =>
      columnsData(key) = columnsData(key).sortWith(_.getQty > _.getQty)
      colsPerPage(key) = LineInfo2TokenSequenceV2.getColumns(null, columnsData(key), pagesData(key), firstLine, false, indentationType, lineInfos)
    }

    //in case the indentations are in 0, it means that the pattern of the first line is not recognized, therefore
    //here they are re-calculated based on the cols margins. In the first version only based on the first col.
    if(sameAfterFirst == 0 && indentedAfterFirst == 0 && untabbedAfterFirst == 0){
      i = 0
      var tabulationHappened = false
      var breakLoop = false
      var startLooking = false
      var firstLlx = -1
      while (i < lineInfos.length && !(tabulationHappened || breakLoop )){
        if(lineInfos(i).presentFeatures.contains("firstReferenceLine")) {
          startLooking = true
          firstLlx = lineInfos(i).llx
        }
        if((lineInfos(i).presentFeatures.contains("newColumn") || lineInfos(i).presentFeatures.contains("newPage"))
            && !lineInfos(i).presentFeatures.contains("bibliography")){
          breakLoop = true
        }
        else if(startLooking){
          if(lineInfos(i).llx >= firstLlx + 5) {
            indentationType = LineInfo2TokenSequenceV2.IndentationType.INDENTED
            tabulationHappened = true
          }
          else if(lineInfos(i).llx <= firstLlx - 5 ) {
            indentationType = LineInfo2TokenSequenceV2.IndentationType.UNTABBED
            tabulationHappened = true
          }
        }
        i += 1
      }
    }

    var refsEndingInPoint: Int = 0
    var refsNotEndingInPoint: Int = 0
    var totRefsSoFar: Int = 0
    var sumVertDistRefs: Int = 0
    var currentPage: Int = lineInfos(0).page
    var movedMargin: Boolean = false
    val ignore: Ignore = new Ignore
    ignore.setIgnorePage(0)


    //indicates if the first line doesn't have any recognizable pattern
    var noFirstLinePattern = false
    i = 0
    while (i < lineInfos.length){

        //feature to see how close a particular line is to the first and second margins given in cols
        if(colsPerPage(lineInfos(i).page)(0) != None &&
          colsPerPage(lineInfos(i).page)(0).head != None &&
          colsPerPage(lineInfos(i).page)(0).head.getLeftX <= lineInfos(i).llx &&
          colsPerPage(lineInfos(i).page)(0).head.getLeftX >= lineInfos(i).llx - 2){
          lineInfos(i).presentFeatures.add("closeFirstMargin")
        }
        if(colsPerPage(lineInfos(i).page).contains(1) &&
          colsPerPage(lineInfos(i).page)(1).head != None &&
          colsPerPage(lineInfos(i).page)(1).head.getLeftX <= lineInfos(i).llx &&
          colsPerPage(lineInfos(i).page)(1).head.getLeftX >= lineInfos(i).llx - 2){
          lineInfos(i).presentFeatures.add("closeFirstMargin")
        }

        if(lineInfos(i).presentFeatures.contains("firstReferenceLine")
           && !lineInfos(i).presentFeatures.contains("samePatternAsInFirst")){
          noFirstLinePattern = true
        }
        if (ignore.getIgnoreType == IgnoreType.IGNORE || ignore.getIgnorePage != lineInfos(i).page) {
          ignore.setIgnoreType(IgnoreType.CLEAN)
          ignore.setIgnorePage(lineInfos(i).page)
        }
        if (ignore.getIgnoreType == IgnoreType.IGNORE_UNLESS_Y_SMALLER && lineInfos(i).ury > /*<*/ ignore.getIgnoreY) {
          ignore.setIgnoreType(IgnoreType.CLEAN)
        }
        if (lineInfos(i).urx - lineInfos(i).llx <= 0.75 * avgLineLength) lineInfos(i).presentFeatures.add("shortLineLength")
        if (i > 0 && !lineInfos(i).presentFeatures.contains("newPage") && !lineInfos(i).presentFeatures.contains("newColumn") && dist2prevLine(i) - dist2prevLine(i - 1) > tolerance) lineInfos(i).presentFeatures.add("bigVertSpaceBefore")
        if (lineInfos(i).font == refFont) lineInfos(i).presentFeatures.add("usesRefFont")
        else if (refFont != -1 && !lineInfos(i).presentFeatures.contains("containsMultiFonts")) lineInfos(i).presentFeatures.add("doesntUseRefFont")
        val currentWidth: Int = lineInfos(i).urx - lineInfos(i).llx
        val iOf: Int = widthLine.indexOf(new LayoutUtils.Entry(currentWidth, 0))
        if (iOf == 0) {
          lineInfos(i).presentFeatures.add("firstCommonWidth")
        }
        else if (iOf == 1) {
          lineInfos(i).presentFeatures.add("secondCommonWidth")
        }
        else if (iOf == 2) {
          lineInfos(i).presentFeatures.add("thirdCommonWidth")
        }
        if (i + 1 < lineInfos.length && lineInfos(i).page == lineInfos(i + 1).page && lineInfos(i).lly /*>*/ < lineInfos(i + 1).lly) {
          val currVertDistance: Int = lineInfos(i + 1).lly - lineInfos(i).lly //lineInfos(i).lly - lineInfos(i + 1).lly
          if ((verticalDistance.size > 1 && verticalDistance.indexOf(new LayoutUtils.Entry/*[Integer]*/(currVertDistance, 0)) > 1)) {
            lineInfos(i).presentFeatures.add("verticalOutlier")
          }
          else if ((verticalDistance.size > 1 && verticalDistance.indexOf(new LayoutUtils.Entry/*[Integer]*/(currVertDistance, 0)) == 1) && (verticalDistance(0).getQty.asInstanceOf[Double] / verticalDistance(1).getQty.asInstanceOf[Double] > 0.15)) {
            lineInfos(i).presentFeatures.add("verticalSpace")
          }
        }
        if (ignore.getIgnoreType == IgnoreType.CLEAN && lineInfos(i).presentFeatures.contains("shortLineLength") && lineInfos(i).presentFeatures.contains("lastLineOnPage") && lineInfos(i).presentFeatures.contains("bigVertSpaceBefore")) {
//          println(s"SETTING IGNORE A: ${lineInfos(i).text}")
          ignore.setIgnoreType(IgnoreType.IGNORE)
          ignore.setIgnorePage(lineInfos(i).page)
        }
        if (ignore.getIgnoreType == IgnoreType.CLEAN && i > 0 &&
              !lineInfos(i).presentFeatures.contains("newColumn") &&
                lineInfos(i).page == lineInfos(i - 1).page &&
                    lineInfos(i).lly /*>*/ < lineInfos(i - 1).lly) {
//          println(s"SETTING IGNORE B: ${lineInfos(i).text}")
          ignore.setIgnoreType(IgnoreType.IGNORE)
          ignore.setIgnorePage(lineInfos(i).page)
        }
        if (ignore.getIgnoreType == IgnoreType.CLEAN && !lineInfos(i).presentFeatures.contains("sameLine") && !lineInfos(i).presentFeatures.contains("newColumn")) {
          val colsInPage = colsPerPage(lineInfos(i).page)
          var ignoreMiddle = false
          var ignoreMargin = false
          for (indents <- colsInPage.values) {
            if(!indents.isEmpty){ // used to also check that first isn't null?
              val leftIndent = indents.head
              val rightIndent = if (indents.size > 1) indents(1) else new LayoutUtils.ColumnData
              val maxX: Int = if (leftIndent.getRightX > rightIndent.getRightX) leftIndent.getRightX else rightIndent.getRightX
              if (leftIndent.isInitialized) {
                if (lineInfos(i).llx >= leftIndent.getLeftX && lineInfos(i).llx <= maxX && (lineInfos(i).urx > maxX + 10)) {
                  ignoreMiddle = true
                }
                else if (lineInfos(i).llx >= leftIndent.getLeftX - 10 && lineInfos(i).llx <= maxX + 10 && (lineInfos(i).urx < maxX + 10)) {
                  ignoreMiddle = false
                }
                if (rightIndent.isInitialized && (indentationType == IndentationType.INDENTED || indentationType == IndentationType.UNTABBED) && lineInfos(i).llx >= leftIndent.getLeftX && lineInfos(i).llx <= maxX && lineInfos(i).llx - rightIndent.getLeftX > 5) {
                  ignoreMargin = true
                }
                else if (rightIndent.isInitialized && (indentationType == IndentationType.INDENTED || indentationType == IndentationType.UNTABBED) && lineInfos(i).llx >= leftIndent.getLeftX && lineInfos(i).llx <= maxX && lineInfos(i).llx - leftIndent.getLeftX <= 30) {
                  ignoreMargin = false
                }
              }
            }
          }
          if (ignoreMargin || ignoreMiddle) {
//            println(s"SETTING IGNORE (${if(ignoreMiddle) "ignoreMiddle" else "ignoreMargin"}): ${lineInfos(i).text}")
            ignore.setIgnoreType(IgnoreType.IGNORE)
            ignore.setIgnorePage(lineInfos(i).page)
          }
        }
//        if (ignore.getIgnoreType != IgnoreType.IGNORE_ALL_POSTERIOR && i > 0 &&
//          lineInfos(i).page == lineInfos(i - 1).page && lineInfos(i).urx < lineInfos(i - 1).llx &&
//          !lineInfos(i - 1).presentFeatures.contains("sameLine") &&
//          !(ignore.getIgnoreType == IgnoreType.IGNORE_UNLESS_Y_SMALLER &&
//                ignore.getIgnoreY > lineInfos(i - 1).lly)) {
//          println("IGNORE_UNLESS_Y_SMALLER")
//          println(s"lineInfos(i).urx = ${lineInfos(i).urx} < lineInfos(i - 1).llx = ${lineInfos(i - 1).llx}")
//          ignore.setIgnoreType(IgnoreType.IGNORE_UNLESS_Y_SMALLER)
//          ignore.setIgnoreY(lineInfos(i - 1).lly)
//          ignore.setIgnorePage(lineInfos(i).page)
//        }

        if (ignore.getIgnoreType != IgnoreType.IGNORE_ALL_POSTERIOR && i > 0 &&
          lineInfos(i).page == lineInfos(i - 1).page && lineInfos(i).lly < lineInfos(i - 1).ury &&
          lineInfos(i).llx < lineInfos(i - 1).llx &&
          !lineInfos(i - 1).presentFeatures.contains("sameLine") &&
          !(ignore.getIgnoreType == IgnoreType.IGNORE_UNLESS_Y_SMALLER &&
            ignore.getIgnoreY  > lineInfos(i - 1).lly)) {
          ignore.setIgnoreType(IgnoreType.IGNORE_UNLESS_Y_SMALLER)
          ignore.setIgnoreY(lineInfos(i - 1).lly)
          ignore.setIgnorePage(lineInfos(i).page)
        }


        if (ignore.getIgnoreType == IgnoreType.CLEAN && lineInfos(i).presentFeatures.contains("bibliography")) {
          ignore.setIgnoreType(IgnoreType.IGNORE)
        }
        if (totRefsSoFar > 0) {
          val avgDistBetwRef = (sumVertDistRefs.toDouble/totRefsSoFar).toInt
          val toAdd = Math.max(Math.ceil(avgDistBetwRef * 0.1), 2).toInt
          val maxLimitDist = avgDistBetwRef + toAdd
          val percentile = (pagesData(lineInfos(i).page).getBottomY - lineInfos(i).lly).toDouble / pagesData(lineInfos(i).page).getHeight
          if (i > 0 && indentationType == IndentationType.INDENTED &&
              !lineInfos(i).presentFeatures.contains("newColumn") &&
              lineInfos(i).page == lineInfos(i - 1).page &&
              lineInfos(i - 1).lly < lineInfos(i).lly &&
              lineInfos(i).lly - lineInfos(i - 1).lly > maxLimitDist && percentile < 0.08){
            ignore.setIgnoreType(IgnoreType.IGNORE_ALL_POSTERIOR)
            ignore.setIgnorePage(lineInfos(i).page)
            lineInfos(i).presentFeatures.add("ignoreAllPosteriorOnPage")
          }
        }
        if (ignore.getIgnoreType == IgnoreType.CLEAN) {
          if ((!movedMargin && lineInfos(i).presentFeatures.contains("samePatternAsInFirst")) ||
            (!movedMargin && i > 0 && !lineInfos(i).presentFeatures.contains("newPage") &&
              !lineInfos(i).presentFeatures.contains("newColumn") &&
              lineInfos(i - 1).presentFeatures.contains("possibleInit") &&
              !lineInfos(i).presentFeatures.contains("indentedFromPrevLine") &&
              indentationType  == IndentationType.INDENTED)){
            lineInfos(i).presentFeatures.add("possibleInit")
          }
          if (!movedMargin && indentationType == IndentationType.INDENTED && (i + 1) < lineInfos.length && lineInfos(i + 1).presentFeatures.contains("indentedFromPrevLine") && (!lineInfos(i).presentFeatures.contains("bibliography"))) {
            lineInfos(i).presentFeatures.add("possibleInit")
            movedMargin = true
          }

          //in case there is no numbering
          if(noFirstLinePattern && lineInfos(i).presentFeatures.contains("newPage")
              && lineInfos(i).presentFeatures.contains("closeFirstMargin") &&
            indentationType == LineInfo2TokenSequenceV2.IndentationType.INDENTED)
          {
            lineInfos(i).presentFeatures.add("possibleInit")
          }

          if (lineInfos(i).presentFeatures.contains("possibleInit")) {
            refsEndingInPoint = refsEndingInPoint + LineInfo2TokenSequenceV2.refEndsInPoint(i, lineInfos)
            refsNotEndingInPoint = refsNotEndingInPoint + LineInfo2TokenSequenceV2.refNotEndsInPoint(i, lineInfos)
            val vertDist: Int = LineInfo2TokenSequenceV2.vertDifFromPrevRef(i, lineInfos)
            if (vertDist > 0) {
              totRefsSoFar += 1
              sumVertDistRefs += LineInfo2TokenSequenceV2.vertDifFromPrevRef(i, lineInfos)
            }
          }
        }
        else {
//          println(s"IGNORE: ${lineInfos(i).text}, ignoreType=${ignore.getIgnoreType}")
          lineInfos(i).presentFeatures.add("ignore")
        }

        if (((indentationType == IndentationType.INDENTED && (i + 1) < lineInfos.length && lineInfos(i + 1).presentFeatures.contains("unTabbedFromPrevLine")) || ((i + 1) < lineInfos.length && lineInfos(i + 1).presentFeatures.contains("newColumn") && lineInfos(i + 1).presentFeatures.contains("samePatternAsInFirst")) || ((i + 1) < lineInfos.length && lineInfos(i + 1).presentFeatures.contains("newPage") && lineInfos(i + 1).presentFeatures.contains("samePatternAsInFirst")))) {
          movedMargin = false
        }
//        println(s"processing line (ignore=${lineInfos(i).presentFeatures.contains("ignore")}): ${lineInfos(i).text}")
        currentPage = lineInfos(i).page
        i += 1
    }
  }

//  private def overlapColumnDatas(columnData1: LayoutUtils.ColumnData, columnData2: LayoutUtils.ColumnData): Boolean =
//    ((columnData1.getLeftX >= columnData2.getLeftX && columnData1.getLeftX <= columnData2.getRightX)
//    || (columnData1.getRightX >= columnData2.getLeftX && columnData1.getRightX <= columnData2.getRightX)
//    || (columnData1.getLeftX <= columnData2.getLeftX && columnData1.getRightX >= columnData2.getRightX)
//    || (columnData1.getLeftX >= columnData2.getLeftX && columnData1.getRightX <= columnData2.getRightX))


  private def computeLexiconFeatures(lineInfos: Array[LineInfo]) {
    val keywords: Array[String] = Array("Proceedings", "Proc\\.", "Conference", "Workshop", "Technical ", "Tech\\. ", "Report", "Symposium", "Symp\\.", "Journal", "Lecture ", "Lect\\. ", "Notes ", "Computer ", "Science ")
    val postwords: Array[String] = Array("^[^A-Za-z]*Received[^A-Za-z]", "^[A-Za-z]*Figure(s)?[^A-Za-z]", "^[A-Za-z]*Table(s)?[^A-Za-z]", "^[A-Za-z]*Graph(s)?[^A-Za-z]", "We ", " we ", "She ", " she ", "He ", " he ", "Our ", " our ", "Her ", " her ", "His ", " his ", "These ", " these ", "Acknowledgements")
    val lowPostwords: Array[String] = Array("They ", " they ", "This ", " this ", " is ", " are ", " was ", " have ", " but ", "[a-z]+\\s+[a-z]+ed ")
    val months: Array[String] = Array("January", "Jan\\.?\\s", "February", "Feb\\.?\\s", "March", "Mar\\.?\\s", "April", "Apr\\.?\\s", "May", "June", "Jun\\.?\\s", "July", "Jul\\.\\s?", "August", "Aug\\.?\\s", "September", "Sept?\\.?\\s", "October", "Oct\\.?\\s", "November", "Nov\\.?\\s", "December", "Dec\\.?\\s")
    var numBeginBrackets: Int = 0
    var numBeginSquareBrackets: Int = 0
    var numBeginParenthesis: Int = 0
    var numBeginNumberCapital: Int = 0
    var numBeginCapInitials: Int = 0
    var numPages: Int = 1
    var prevPage: Int = 0
    var noFirstReferenceLine:Boolean = true
    var biblioTitleIndex: Int = -1

    var i = 0
    while (i < lineInfos.length) {
        if (i == 0) prevPage = lineInfos(i).page
        else if (lineInfos(i).page != prevPage) {
          numPages += 1
          prevPage = lineInfos(i).page
        }
        val squishedText: String = lineInfos(i).text.replaceAll("\\s", "")

        if(!(squishedText.length == 0))
        {

          val numPeriodCommas: Int = LineInfo2TokenSequenceV2.specialPunctCounter(squishedText)

          if (numPeriodCommas == 0) lineInfos(i).presentFeatures.add("noSpecialPuncts")
          else if (numPeriodCommas > 3) lineInfos(i).presentFeatures.add("manySpecialPuncts")
          else lineInfos(i).presentFeatures.add("someSpecialPuncts")
          if (squishedText.matches("^\\[.+\\].*")) {
            lineInfos(i).presentFeatures.add("beginBrackets")
            lineInfos(i).presentFeatures.add("beginSquareBrackets")
            numBeginBrackets += 1
            numBeginSquareBrackets += 1
          }
          if (squishedText.matches("^\\([0-9]+\\).*")) {
            lineInfos(i).presentFeatures.add("beginBrackets")
            lineInfos(i).presentFeatures.add("beginParenthesis")
            numBeginBrackets += 1
            numBeginParenthesis += 1
          }
          if (squishedText.matches("^\\([0-9]+\\).*")) {
            lineInfos(i).presentFeatures.add("beginNumericBrackets")
          }
          if (biblioTitleIndex > -1 && noFirstReferenceLine==true) {
            lineInfos(i).presentFeatures.add("firstReferenceLine")
            noFirstReferenceLine = false
          }
          if (biblioTitleIndex == -1 && squishedText.matches("^[#iIvVxX\\d\\.\\s]{0,5}(R(?i:eferences)|B(?i:ibliography)|R(?i:eferencesandNotes)|L(?i:iteratureCited)|(.*REFERENCES.*))\\s*$")) {
            biblioTitleIndex = i
            lineInfos(i).presentFeatures.add("bibliography")
          }
          if (squishedText.matches("^[0-9]+\\.?\\p{Lu}.*")) {
            lineInfos(i).presentFeatures.add("beginsNumberCapital")
            numBeginNumberCapital += 1
          }
          // TODO precompile all these regexes
          if (!squishedText.endsWith(".")) lineInfos(i).presentFeatures.add("noEndingPeriod")
          if (squishedText.matches(".*[^\\p{Ll}\\p{Lu}]\\p{Lu}\\.$")) lineInfos(i).presentFeatures.add("endsWithCapPeriod")
          if (squishedText.matches(".*[0-9]+-(-)?[0-9]+.*")) lineInfos(i).presentFeatures.add("containsPageRange")
          if (squishedText.matches(".*(19|20)\\d{2}.*")) lineInfos(i).presentFeatures.add("containsYear")
          if (squishedText.matches(".*(?i)appendix.*")) lineInfos(i).presentFeatures.add("containsAppendix")
          if (squishedText.matches(".*(?i)received.*")) lineInfos(i).presentFeatures.add("containsReceived")
          if (squishedText.matches(".*(?i)address.*")) lineInfos(i).presentFeatures.add("containsAddress")
          if (squishedText.matches(".*\\w+@\\w+\\.\\w+.*")) lineInfos(i).presentFeatures.add("containsEmail")
          if (squishedText.matches(".*(ftp|http)\\://\\w+\\.\\w+.*")) lineInfos(i).presentFeatures.add("containsURL")
          if (squishedText.matches(".*[,\\-\\:]$")) lineInfos(i).presentFeatures.add("endsWithPunctNotPeriod")
          if (squishedText.matches(".*\\d.*")) lineInfos(i).presentFeatures.add("containsDigit")
          if (lineInfos(i).text.matches(".*et(\\.)?\\sal.*")) lineInfos(i).presentFeatures.add("containsEtAl")
          if (lineInfos(i).text.matches("^(\\p{Lu}\\.\\s*)+\\s+[\\p{Lu}\\p{Ll}]+.*") || squishedText.matches("^\\p{Lu}[\\p{Lu}\\p{Ll}]+\\,\\p{Lu}[\\.,].*")) {
            lineInfos(i).presentFeatures.add("beginsCapitalInitials")
            numBeginCapInitials += 1
          }
          if (lineInfos(i).text.matches("^([a-z]).*")) {
            lineInfos(i).presentFeatures.add("beginsLowerCase")
          }
          var j = 0
          var foundMatch = false
          while (!foundMatch && j < keywords.length) {
              if (lineInfos(i).text.matches(".*" + keywords(j) + ".*")) {
                lineInfos(i).presentFeatures.add("containsKeyword")
                foundMatch = true
              }
              j += 1
          }
          j = 0
          foundMatch = false
          while (!foundMatch && j < postwords.length) {
              if (lineInfos(i).text.matches(".*" + postwords(j) + ".*")) {
                lineInfos(i).presentFeatures.add("containsPostword1")
                foundMatch = true
              }
              j += 1
          }
          j = 0
          foundMatch = false
          while (!foundMatch && j < lowPostwords.length) {
            if (lineInfos(i).text.matches(".*" + lowPostwords(j) + ".*")) {
              lineInfos(i).presentFeatures.add("containsPostword2")
              foundMatch = true
            }
            j += 1
          }
          j = 0
          foundMatch = false
          while (!foundMatch && j < months.length) {
            if (lineInfos(i).text.matches(".*" + months(j) + ".*")) {
              lineInfos(i).presentFeatures.add("containsMonth")
              foundMatch = true
            }
            j += 1
          }
        }
      i += 1
    }

    val threshold: Double = if ((numPages > 2)) 4 else 1
    var seqHasBeginNumberCapital: Boolean = false
    var seqHasBeginSquareBrackets: Boolean = false
    var seqHasBeginParenthesis: Boolean = false
    var max: Int = 0
    if (numBeginSquareBrackets > numBeginNumberCapital && numBeginSquareBrackets > numBeginCapInitials && numBeginSquareBrackets > numBeginParenthesis) {
      seqHasBeginSquareBrackets = true
      max = numBeginSquareBrackets
    }
    else if (numBeginParenthesis > numBeginNumberCapital && numBeginParenthesis > numBeginCapInitials && numBeginParenthesis > numBeginSquareBrackets) {
      seqHasBeginParenthesis = true
      max = numBeginParenthesis
    }
    else if (numBeginNumberCapital > numBeginSquareBrackets && numBeginNumberCapital > numBeginCapInitials && numBeginNumberCapital > numBeginParenthesis) {
      seqHasBeginNumberCapital = true
      max = numBeginNumberCapital
    }
    else max = numBeginCapInitials
    if (max <= threshold) return
    {
      var i = 0
      while (i < lineInfos.length) {
        if (seqHasBeginSquareBrackets)
          lineInfos(i).presentFeatures.add("seqHasBeginSquareBrackets")
        else if (seqHasBeginParenthesis)
          lineInfos(i).presentFeatures.add("seqHasBeginParenthesis")
        else if (seqHasBeginNumberCapital)
          lineInfos(i).presentFeatures.add("seqHasBeginNumberCapital")
        else lineInfos(i).presentFeatures.add("seqHasBeginCapInitials")
        i += 1
      }
    }
  }
}

final object IgnoreType {
  final val IGNORE:Int = 0
  final val IGNORE_UNLESS_Y_SMALLER:Int = 1
  final val IGNORE_ALL_POSTERIOR:Int = 2
  final val CLEAN:Int = 3
}

class Ignore {
  def getIgnorePage: Int = {
    return ignorePage
  }

  def setIgnorePage(ignorePage: Int) {
    this.ignorePage = ignorePage
  }

  def getIgnoreType: Int = {
    return ignoreType
  }

  def setIgnoreType(ignoreType: Int) {
    this.ignoreType = ignoreType
  }

  def getIgnoreY: Int = {
    return ignoreY
  }

  def setIgnoreY(ignoreY: Int) {
    this.ignoreY = ignoreY
  }

  private[segmentation] var ignoreType: Int = -1
  private var ignoreY: Int = 0
  private var ignorePage: Int = 0
}
