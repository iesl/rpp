package org.rexo.extraction

import edu.umass.cs.iesl.xml_annotator.Annotator
import Annotator._
import org.jdom2.{Parent, Document, Element}
import org.rexo.util.EnglishDictionary
import scala.collection.mutable

import java.util.regex.{Matcher, Pattern}
import org.rexo.extra.extract.Span
import java.util
import org.rexo.extra.types.{PropertyHolder, TokenSequence}
import org.rexo.base.extract.Tokenization
import scala.collection.immutable.IntMap


import org.rexo.span.CompositeSpan
import org.rexo.extra.extract.StringSpan
import org.rexo.extra.utils.CharSequenceLexer

/**
 * Created by klimzaporojets on 11/11/14.
 */
object NewHtmlTokenizationSvg {

  def getPageNumber(elem:Element, doc:Document):Int =
  {
    if(elem.getParent!= doc.getRootElement)
    {
      return getPageNumber(elem.getParent.asInstanceOf[Element], doc)
    }
    else
    {
      return doc.getRootElement.getChildren().indexOf(elem)
    }
  }
  var lastBracketId = 0;
  private def isPaginationText(counts: Map[String, Int], s: String, isTopOrBottomLine: Boolean,
                               llx: Double, lly: Double, fontnumber: String /*fontnumber: Int*/): Boolean = {
    val normalizedLine: String = headerFooterNormalize(s, isTopOrBottomLine, llx.toInt, lly.toInt, fontnumber)

    val count: Int = (counts.get(normalizedLine).get.asInstanceOf[Integer]).intValue //(counts.get(normalizedLine).asInstanceOf[Integer]).intValue
    return count >= 3
  }


  private def headerFooterNormalize(string: String, isTopOrBottomLine: Boolean,
                                    llx: Int, lly: Int, fontNum:String /*fontNum: Int*/): String = {
    var ret: String = NewHtmlTokenizationSvg.DIGIT_OR_SPACE.matcher(string).replaceAll("")
    ret = ret + " " + llx + "," + lly + "," + fontNum

//    println(string + " ---- header/footer normalized ---- " + ret)

    return ret
  }

  def createNewHtmlTokenization(anno: Annotator, globalDict: EnglishDictionary): NewHtmlTokenizationSvg = {
    return new NewHtmlTokenizationSvg(anno, globalDict)
  }

  final val LEXER_PATTERN: Pattern = Pattern.compile("(``|''|\\n|&amp;|1st|2nd|3rd|[4-9]th|Ph.D.|\\S+@\\S+|\\w\\.|\\+[A-Z]+\\+|\\p{Alpha}+|\\p{Digit}+|\\p{Punct}|^)\\s*")
  final val DIGIT_OR_SPACE: Pattern = Pattern.compile("\\d+| ")

}


class NewHtmlTokenizationSvg extends TokenSequence with Tokenization {
  private var _lineSpans: mutable.ArrayBuffer[Span] = null //mutable.MutableList[Span] = null // [StringSpan] = null //List[StringSpan] = null
  private var _document: CharSequence = null

   var _annotator: Annotator = null

  val regFontFamily = new scala.util.matching.Regex(""".*font\-family:([\-0-9a-zA-Z\s]+);.*""", "font")
  val regPageNumber = new scala.util.matching.Regex("""pageContainer([0-9]+)""", "page")
  val regFontSize = new scala.util.matching.Regex(""".*font\-size:[\s]+([0-9\.]+)px;.*""", "fontsize")
  val regLlx = new scala.util.matching.Regex("""([\-0-9\.]+)""", "llx") //new scala.util.matching.Regex(""".*left:([\s\-0-9\.]+)px;.*""", "llx")
  val regLly = new scala.util.matching.Regex("""([\-0-9\.]+)""", "top") //new scala.util.matching.Regex(""".*top:([\s\-0-9\.]+)px;.*""", "top")
  val regTransformationMatrix =
    new scala.util.matching.Regex("""matrix\(([\-0-9\.]+)\s([\-0-9\.]+)\s([\-0-9\.]+)\s([\-0-9\.]+)\s([\-0-9\.]+)\s([\-0-9\.]+)\)""", "a", "b", "c", "d", "e", "f")
  val regTransformationTranslate =
    new scala.util.matching.Regex("""translate\(([\-0-9\.]+)\s([\-0-9\.]+)\)""", "a", "b")

  private def this(document: CharSequence, tokenSpans: Array[AnyRef], activeSpanList: mutable.MutableList[Span],
                   lineSpans: mutable.ArrayBuffer[Span]) {
    this()
    this._document = document
    this.addAll(tokenSpans)
    this.activeSpanList = activeSpanList
    this._lineSpans = lineSpans
  }


  /**
   * set features to null, return true if they were not already null
   * @return true if features were actually cleared
   */
  def clearTokenFeatures: Boolean = {
    {
      var i: Int = 0
      while (i < size) {
        {
          val o: AnyRef = get(i)
          if (o.isInstanceOf[PropertyHolder]) {
            val token: PropertyHolder = o.asInstanceOf[PropertyHolder]
            if (null != token.getFeatures) {
              token.setFeatures(null)
              return true
            }
          }
        }
        i += 1; i - 1
      }
    }
    return false
  }

  private var _constructionInfo: NewHtmlTokenizationSvg#ConstructionInfo = null


  private class ConstructionInfo {
    var _fontNames: List[String] = null
    private[extraction] var _normalFontSize: Double = .0
    private[extraction] var _largestFontSize: Double = .0
    private[extraction] var _headerFooterLineCounts: Map[String, Int] = null
    private[extraction] var _wrappedSpans: mutable.MutableList[Array[StringSpan]] = null

    private[extraction] var fontName: String = ""
    private[extraction] var lastFontName: String = ""
    private[extraction] var fontSize: Double = -1
    private[extraction] var lastFontSize: Double = -1
    private[extraction] var textofs: Int = 0
    private[extraction] var globalLineNum: Int = -1
    var _hyphToken: StringSpan = null
    var _preHyphToken: StringSpan = null
    private[extraction] var _docText: StringBuffer = new StringBuffer
    private[extraction] var _localDict: mutable.TreeSet[String] = new mutable.TreeSet[String]()
  }
  /**
   * Creates a tokenization of .
   * <p/>
   * Tokens are added from all the matches of the given lexer in each line, as well as in between the the end and start of two successive lines if de-hyphonization can be applied.
   */
  def this(annotator: Annotator, globalDict: EnglishDictionary) {
    this()

    _constructionInfo = new ConstructionInfo

    _annotator = annotator 

    _constructionInfo._fontNames = findFontNames()
    //
    _constructionInfo._normalFontSize = findNormalFontSizeSvg()

    _constructionInfo._largestFontSize = findLargestFontSize()

    _constructionInfo._headerFooterLineCounts = initHeaderFooterLineCounts()


    _constructionInfo._wrappedSpans = mutable.MutableList()
    _lineSpans = mutable.ArrayBuffer()
    _constructionInfo._hyphToken = null
    _constructionInfo._preHyphToken = null
    val defGlobalDict =
      if(globalDict == null)
      {
        EnglishDictionary.createDefault(EnglishDictionary._defaultWords)
      }
      else
      {
        globalDict
      }
    tokenizePages(_annotator)
    deHyphenateDocument(defGlobalDict)
    this._document = _constructionInfo._docText.toString
  }


  /**
   *
   * @param x
   */
  private def tokenizePages(/*root: Element*/ x:Annotator) {

//    var i = 1;
    tokenizeLines(x)
  }

  /**
   *
   * @param page
   */
  private def tokenizeLines(page:Annotator) {

//    val parentElement = page.getDom().getRootElement

//    println("number of pages: " + parentElement.getChildren.size())

    val lineBIndexSet = page.getBIndexSet(Single(SegmentCon("line")))

    val grpByPage:List[(Int,Int,IntMap[Element])] =
    lineBIndexSet.toList.zipWithIndex.map {
      case (lineIndex, i) =>
        val elements = page.getElements("line")(lineIndex)
        val (blockIndex, charIndex) = _annotator.mkIndexPair(lineIndex)
        val currentPage = NewHtmlTokenizationSvg.getPageNumber(elements.get(blockIndex).get, page.getDom()) + 1
//        println("current page number: " + )
//        if(currentPage == pageNum){
//          Map(charIndex -> elements)
//        }
        //Map(currentPage -> Map(charIndex -> elements))
        (currentPage, i, elements)

//        val currentPage =
    }


//    print(grpByPage)

    val grpBy:Map[Int,List[(Int,Int,IntMap[Element])]] = grpByPage.groupBy(_._1)

    val grpBy2 = grpBy.map(x=>
        Map(x._1->x._2.filter(y=> y._1==x._1).groupBy(_._2))
    ).flatten.toMap

    val groupedByLineContent:Map[Int, Map[Int, IntMap[Element]]] = grpBy2.map(x=>
      Map(x._1 -> x._2.map(x2=>
        Map(x2._1->x2._2.filter(x3=> (x3._1 == x._1 && x3._2 == x2._1))(0)._3)
      ).flatten.toMap)
    ).flatten.toMap

    for( //this for iterates over pages
      currentPage <- groupedByLineContent.keys.toList.sorted
    ){
//      println("processing page: " + currentPage)
      val lines:Set[Int] = groupedByLineContent.get(currentPage).get.keySet

      val keysIterator:Iterator[Int] = groupedByLineContent.get(currentPage).get.keysIterator

      val sortedLines:List[Int] = keysIterator.toList.sorted

      var firstTokenOnPage: Boolean = true

      val lastLine:Int  = lines.max
      val firstLine:Int = lines.min

      for(
        currentLine <- sortedLines
      ) {
        val line: IntMap[Element] = groupedByLineContent.get(currentPage).get.get(currentLine).get

        val lineCompositeSpan: CompositeSpan = CompositeSpan.createSpan(_constructionInfo._docText)

        val tboxList:List[(Int,Element)] = line.toList
        _constructionInfo.globalLineNum += 1
        val lineStartOfs: Int = _constructionInfo.textofs
        val firstTbox: Element = line.get(line.firstKey).get

  //      val coords = getCoordinates(firstTbox, 0.0, 0.0)
  //      var llx:Double = coords._1
  //      var lly:Double = coords._2
        val coords2 = Annotator.getTransformedCoords(firstTbox, getTopPageAncestor(firstTbox, page.getDom()))
        var llx = coords2.xs(0)
        var lly = coords2.ys(0)

  //      var llx:Double = getLlxV2(firstTbox)
  //      var lly:Double = getLlyV2(firstTbox)
        val fontFamily: String = getFontFamily(firstTbox)

        val isTopOrBottomLine: Boolean = (currentLine == firstLine || currentLine == lastLine)


        val lineText: String = groupedByLineContent.get(currentPage).get.get(currentLine).get.map(x => x._2.getValue
        ).mkString("")


  //      println("the line text is: " + lineText)
        if (NewHtmlTokenizationSvg.isPaginationText(_constructionInfo._headerFooterLineCounts,
          lineText.toString, isTopOrBottomLine, llx, lly, fontFamily)) {
          _constructionInfo._docText.append(lineText)
          _constructionInfo.textofs += lineText.length
          val paginationToken: StringSpan = new StringSpan(_constructionInfo._docText, lineStartOfs, _constructionInfo.textofs)
          println("setting isHeaderFooterLine")
          paginationToken.setNumericProperty("isHeaderFooterLine", 1.0)
          val tboxI: Iterator[_] = tboxList.iterator
          var locUrx: Double = -1
          var locUry: Double = -1
          var locLlx: Double = -1
          var locLly: Double = -1

          while (tboxI.hasNext) {
            val tbox: (Int,Element) = tboxI.next.asInstanceOf[(Int,Element)]

  //          val coords = getCoordinates(tbox._2, 0.0, 0.0)
  //          val curLlx:Double = coords._1
  //          val curLly:Double = coords._2
            val coords2 = Annotator.getTransformedCoords(tbox._2, page.getDom().getRootElement())
            val curLlx = coords2.xs(0)
            val curLly = coords2.ys(0)


  //          val curLlx = getLlxV2(tbox._2)
  //          val curLly = getLlyV2(tbox._2)

            val curUry = getFontSize(tbox._2) + curLly
            //TODO: see if it is possible to get width of the span, if not try to get approx value
            val curUrx = curLlx //+ getWidth(tbox._2).toInt

            if (locUrx == -1 || curUrx  > locUrx) {
              locUrx = curUrx
            }
            if (locUry == -1 || curUry <  locUry) {
              locUry = curUry
            }
            if (locLlx == -1 || curLlx  < locLlx) {
              locLlx = curLlx
            }
            if (locLly == -1 || curLly  < locLly) {
              locLly = curLly
            }
          }
          paginationToken.setProperty("llx", locLlx)
          paginationToken.setProperty("lly", locLly)
          paginationToken.setProperty("urx", locUrx)
          paginationToken.setProperty("ury", locUry)
          paginationToken.setProperty("pageNum", currentPage.toDouble) //pageNum.toDouble)
          paginationToken.setProperty("divElement", tboxList.iterator.next())

          _lineSpans.+=(paginationToken.asInstanceOf[Span])
        }
        else
        {
          var tboxI: Iterator[_] = tboxList.iterator
          tboxI = tboxList.iterator
          var firstTokenInLine: Boolean = true
          while (tboxI.hasNext) {
            val tbox: (Int, Element) = tboxI.next.asInstanceOf[(Int, Element)] //asInstanceOf[Element]

  //          val coords = getCoordinates(tbox._2, 0.0, 0.0)
  //          val curLlx:Double = coords._1
  //          val curLly:Double = coords._2

            val coords2 = Annotator.getTransformedCoords(tbox._2, page.getDom().getRootElement())
            val curLlx = coords2.xs(0)
            val curLly = coords2.ys(0)

  //          val curLlx = getLlxV2(tbox._2)
  //          val curLly = getLlyV2(tbox._2)

            val curUry = getFontSize(tbox._2)/*.toInt*/ + curLly
            //TODO: ask Thomas if is it possible to get the width of a particular span
            val curUrx = coords2.endX //curLlx //+ getWidth(tbox._2).toInt

            llx = curLlx
            lly = curLly
            val urx: Double = curUrx
            val ury: Double = curUry


            _constructionInfo.fontSize = getFontSize(tbox._2)
            _constructionInfo.fontName = getFontFamily(tbox._2)
            val boxText: String = tbox._2.getText


            _constructionInfo._docText.append(boxText)
            var lexer: CharSequenceLexer = null
            lexer = CharSequenceLexer.apply(boxText, NewHtmlTokenizationSvg.LEXER_PATTERN)

            var firstTokenInBox: Boolean = true
            while (lexer.hasNext) {
              lexer.next
              val spanStart: Int = lexer.getStartOffset + _constructionInfo.textofs
              val spanEnd: Int = lexer.getEndOffset + _constructionInfo.textofs
              val token: StringSpan = new StringSpan(_constructionInfo._docText, spanStart, spanEnd)
              val ttext: String = token.getText

              val twsMatcher: Matcher = trailingWS.matcher(ttext)
              if (twsMatcher.find) {
                val twsLen: Int = twsMatcher.group.length
                token.setText(ttext.substring(0, ttext.length - twsLen))
                if (twsLen > 1) {
                  token.setNumericProperty("trailing-ws-1", twsLen - 1)
                }
              }
              else {
                token.setNumericProperty("trailing-ws-1", -1)
              }
              lineCompositeSpan.appendSpan(token)
              if (firstTokenInBox) {
                token.setNumericProperty("firstInTextBox", 1)
                firstTokenInBox = false
              }
              var termCombined: Boolean = false
              if (firstTokenInLine && _constructionInfo._hyphToken != null) {
                token.setNumericProperty("split-rhs", 1)
                _constructionInfo._wrappedSpans.+= (Array[StringSpan](_constructionInfo._preHyphToken, _constructionInfo._hyphToken, token)) //.add(Array[StringSpan](_constructionInfo._preHyphToken, _constructionInfo._hyphToken, token))
                _constructionInfo._hyphToken = null
                _constructionInfo._preHyphToken = null
                termCombined = true
              }
              token.setNumericProperty("lineNum", _constructionInfo.globalLineNum)
              if (_constructionInfo.fontSize >= _constructionInfo._largestFontSize - 1 && _constructionInfo.fontSize > _constructionInfo._normalFontSize + 1) {
                token.setNumericProperty("largestfont", 1)
              }
              else if (_constructionInfo.fontSize > _constructionInfo._normalFontSize + 1) {
                token.setNumericProperty("largefont", 1)
              }
              else if (_constructionInfo.fontSize < _constructionInfo._normalFontSize - 1) {
                token.setNumericProperty("smallfont", 1)
              }
              if (_constructionInfo.fontSize != _constructionInfo.lastFontSize) {
                token.setNumericProperty("newfontsize", 1)
              }
              if (_constructionInfo.fontName ne _constructionInfo.lastFontName) {
                token.setNumericProperty("newfontname", 1)
              }

              token.setNumericProperty("llx", llx)
              token.setNumericProperty("lly", lly)
              token.setNumericProperty("urx", urx)
              token.setNumericProperty("ury", ury)
              token.setProperty("divElement", tbox)

              token.setProperty("startOffset", lexer.getStartOffset)
              token.setProperty("endOffset", lexer.getEndOffset)

              token.setProperty("fontname", _constructionInfo.fontName)
              if (firstTokenOnPage) {
                token.setNumericProperty("newpage", 1)
                firstTokenOnPage = false
              }
              if (firstTokenInLine) {
                token.setNumericProperty("newline", 1)
                firstTokenInLine = false
              }
              token.setNumericProperty("pageNum", currentPage) // pageNum)

              _constructionInfo.lastFontSize = _constructionInfo.fontSize
              _constructionInfo.lastFontName = _constructionInfo.fontName
              val tokenText: String = token.getText
              val lineEndp: Boolean = !lexer.hasNext && !tboxI.hasNext
              if (lineEndp && (tokenText.trim == "-")) {
                if (size > 1) {
                  val lastToken: StringSpan = this.get(this.size - 1)./*asInstanceOf[Option[Any]].get.*/asInstanceOf[StringSpan]
                  lastToken.setNumericProperty("split-lhs", 1)
                  token.setNumericProperty("split-hyphen", 1)
                  _constructionInfo._preHyphToken = lastToken
                  _constructionInfo._hyphToken = token
                }
              }
              else {
                this.add(token)
                if (!termCombined) {
                  _constructionInfo._localDict.add(token.getText.toLowerCase)
                }
              }
              if (lineEndp && termCombined) {
                val phantomToken: StringSpan = new StringSpan(_constructionInfo._docText, spanStart, spanEnd)
                if (firstTokenOnPage) {
                  phantomToken.setNumericProperty("newpage", 1)
                  firstTokenOnPage = false
                }
                phantomToken.setNumericProperty("newline", 1)
                phantomToken.setNumericProperty("llx", llx)
                phantomToken.setNumericProperty("lly", lly)
                phantomToken.setNumericProperty("phantom", 1)
                phantomToken.setProperty("divElement", tbox)

                this.add(phantomToken)
              }
            }
            _constructionInfo.textofs += boxText.length
          }
          _lineSpans.+=(lineCompositeSpan.asInstanceOf[Span]) //.add(lineCompositeSpan)

        }//end of else
      }//here the main for for lines ends
    }


  } //tokenizeLines ends here


  /**
   *
   * @param page
   */
  private def tokenizeLinesOld(page:Annotator, pageNum:Int) {

    def getParent(elem:Element, doc:Document):Int =
    {
      if(elem.getParent!= doc.getRootElement)
      {
        return getParent(elem.getParent.asInstanceOf[Element], doc)
      }
      else
      {
        return doc.getRootElement.getChildren().indexOf(elem)
      }
    }

    val parentElement = page.getDom().getRootElement



//    println("number of pages: " + parentElement.getChildren.size())

    val lineBIndexSet = page.getBIndexSet(Single(SegmentCon("line")))

    val groupedByLineContent:Map[Int, IntMap[Element]] = lineBIndexSet.toList.zipWithIndex.map {
      case (lineBIndex, i) =>
        val elements = page.getElements("line")(lineBIndex)
        val (blockIndex, _) = _annotator.mkIndexPair(lineBIndex)
//        println("current page number: " + getParent(elements.get(blockIndex).get, page.getDom()))
        //        val currentPage =
        Map(i -> elements)
    }.flatten.toMap


    val lines:Set[Int] = groupedByLineContent.keySet

    val keysIterator:Iterator[Int] = groupedByLineContent.keysIterator

    val sortedLines:List[Int] = keysIterator.toList.sorted

    var firstTokenOnPage: Boolean = true

    val lastLine:Int  = lines.max
    val firstLine:Int = lines.min

    for(
      currentLine <- sortedLines
    ) {
      val line: IntMap[Element] = groupedByLineContent.get(currentLine).get

      val lineCompositeSpan: CompositeSpan = CompositeSpan.createSpan(_constructionInfo._docText)

      val tboxList:List[(Int,Element)] = line.toList
      _constructionInfo.globalLineNum += 1
      val lineStartOfs: Int = _constructionInfo.textofs
      val firstTbox: Element = line.get(line.firstKey).get

      //      val coords = getCoordinates(firstTbox, 0.0, 0.0)
      //      var llx:Double = coords._1
      //      var lly:Double = coords._2
      val coords2 = Annotator.getTransformedCoords(firstTbox, getTopAncestor(firstTbox))
      var llx = coords2.xs(0)
      var lly = coords2.ys(0)

      //      var llx:Double = getLlxV2(firstTbox)
      //      var lly:Double = getLlyV2(firstTbox)
      val fontFamily: String = getFontFamily(firstTbox)

      val isTopOrBottomLine: Boolean = (currentLine == firstLine || currentLine == lastLine)


      val lineText: String = groupedByLineContent.get(currentLine).get.map(x => x._2.getValue
      ).mkString("")


      //      println("the line text is: " + lineText)
      if (NewHtmlTokenizationSvg.isPaginationText(_constructionInfo._headerFooterLineCounts,
        lineText.toString, isTopOrBottomLine, llx, lly, fontFamily)) {
        _constructionInfo._docText.append(lineText)
        _constructionInfo.textofs += lineText.length
        val paginationToken: StringSpan = new StringSpan(_constructionInfo._docText, lineStartOfs, _constructionInfo.textofs)
        paginationToken.setNumericProperty("isHeaderFooterLine", 1.0)
        var tboxI: Iterator[_] = tboxList.iterator
        var locUrx: Double = -1
        var locUry: Double = -1
        var locLlx: Double = -1
        var locLly: Double = -1

        while (tboxI.hasNext) {
          val tbox: (Int,Element) = tboxI.next.asInstanceOf[(Int,Element)]

          //          val coords = getCoordinates(tbox._2, 0.0, 0.0)
          //          val curLlx:Double = coords._1
          //          val curLly:Double = coords._2
          val coords2 = Annotator.getTransformedCoords(tbox._2, getTopAncestor(tbox._2))
          val curLlx = coords2.xs(0)
          val curLly = coords2.ys(0)

          //          val curLlx = getLlxV2(tbox._2)
          //          val curLly = getLlyV2(tbox._2)

          val curUry = getFontSize(tbox._2) + curLly
          //TODO: see if it is possible to get width of the span, if not try to get approx value
          val curUrx = curLlx //+ getWidth(tbox._2).toInt

          if (locUrx == -1 || curUrx  > locUrx) {
            locUrx = curUrx
          }
          if (locUry == -1 || curUry <  locUry) {
            locUry = curUry
          }
          if (locLlx == -1 || curLlx  < locLlx) {
            locLlx = curLlx
          }
          if (locLly == -1 || curLly  < locLly) {
            locLly = curLly
          }
        }
        paginationToken.setProperty("llx", locLlx)
        paginationToken.setProperty("lly", locLly)
        paginationToken.setProperty("urx", locUrx)
        paginationToken.setProperty("ury", locUry)
        paginationToken.setProperty("pageNum", pageNum.toDouble)
        paginationToken.setProperty("divElement", tboxList.iterator.next())

        _lineSpans.+=(paginationToken.asInstanceOf[Span])
      }
      else
      {
        val tboxI = tboxList.iterator
        var firstTokenInLine: Boolean = true
        while (tboxI.hasNext) {
          val tbox = tboxI.next()

          //          val coords = getCoordinates(tbox._2, 0.0, 0.0)
          //          val curLlx:Double = coords._1
          //          val curLly:Double = coords._2
          val coords2 = Annotator.getTransformedCoords(tbox._2, getTopAncestor(tbox._2))
          val curLlx = coords2.xs(0)
          val curLly = coords2.ys(0)

          //          val curLlx = getLlxV2(tbox._2)
          //          val curLly = getLlyV2(tbox._2)

          val curUry = getFontSize(tbox._2)/*.toInt*/ + curLly
          //TODO: ask Thomas if is it possible to get the width of a particular span
          val curUrx = coords2.endX //curLlx //+ getWidth(tbox._2).toInt

          llx = curLlx
          lly = curLly
          val urx: Double = curUrx
          val ury: Double = curUry


          _constructionInfo.fontSize = getFontSize(tbox._2)
          _constructionInfo.fontName = getFontFamily(tbox._2)
          val boxText = tbox._2.getText


          _constructionInfo._docText.append(boxText)
          var lexer: CharSequenceLexer = null
          lexer = CharSequenceLexer.apply(boxText, NewHtmlTokenizationSvg.LEXER_PATTERN)

          var firstTokenInBox: Boolean = true
          while (lexer.hasNext) {
            lexer.next
            val spanStart: Int = lexer.getStartOffset + _constructionInfo.textofs
            val spanEnd: Int = lexer.getEndOffset + _constructionInfo.textofs
            val token: StringSpan = new StringSpan(_constructionInfo._docText, spanStart, spanEnd)
            val ttext: String = token.getText

            val twsMatcher: Matcher = trailingWS.matcher(ttext)
            if (twsMatcher.find) {
              val twsLen: Int = twsMatcher.group.length
              token.setText(ttext.substring(0, ttext.length - twsLen))
              if (twsLen > 1) {
                token.setNumericProperty("trailing-ws-1", twsLen - 1)
              }
            }
            else {
              token.setNumericProperty("trailing-ws-1", -1)
            }
            lineCompositeSpan.appendSpan(token)
            if (firstTokenInBox) {
              token.setNumericProperty("firstInTextBox", 1)
              firstTokenInBox = false
            }
            var termCombined: Boolean = false
            if (firstTokenInLine && _constructionInfo._hyphToken != null) {
              token.setNumericProperty("split-rhs", 1)
              _constructionInfo._wrappedSpans.+= (Array[StringSpan](_constructionInfo._preHyphToken, _constructionInfo._hyphToken, token)) //.add(Array[StringSpan](_constructionInfo._preHyphToken, _constructionInfo._hyphToken, token))
              _constructionInfo._hyphToken = null
              _constructionInfo._preHyphToken = null
              termCombined = true
            }
            token.setNumericProperty("lineNum", _constructionInfo.globalLineNum)
            if (_constructionInfo.fontSize >= _constructionInfo._largestFontSize - 1 && _constructionInfo.fontSize > _constructionInfo._normalFontSize + 1) {
              token.setNumericProperty("largestfont", 1)
            }
            else if (_constructionInfo.fontSize > _constructionInfo._normalFontSize + 1) {
              token.setNumericProperty("largefont", 1)
            }
            else if (_constructionInfo.fontSize < _constructionInfo._normalFontSize - 1) {
              token.setNumericProperty("smallfont", 1)
            }
            if (_constructionInfo.fontSize != _constructionInfo.lastFontSize) {
              token.setNumericProperty("newfontsize", 1)
            }
            if (_constructionInfo.fontName ne _constructionInfo.lastFontName) {
              token.setNumericProperty("newfontname", 1)
            }

            token.setNumericProperty("llx", llx)
            token.setNumericProperty("lly", lly)
            token.setNumericProperty("urx", urx)
            token.setNumericProperty("ury", ury)
            token.setProperty("divElement", tbox)

            token.setProperty("startOffset", lexer.getStartOffset)
            token.setProperty("endOffset", lexer.getEndOffset)

            token.setProperty("fontname", _constructionInfo.fontName)
            if (firstTokenOnPage) {
              token.setNumericProperty("newpage", 1)
              firstTokenOnPage = false
            }
            if (firstTokenInLine) {
              token.setNumericProperty("newline", 1)
              firstTokenInLine = false
            }
            token.setNumericProperty("pageNum", pageNum)

            _constructionInfo.lastFontSize = _constructionInfo.fontSize
            _constructionInfo.lastFontName = _constructionInfo.fontName
            val tokenText: String = token.getText
            val lineEndp: Boolean = !lexer.hasNext && !tboxI.hasNext
            if (lineEndp && (tokenText.trim == "-")) {
              if (size > 1) {
                val lastToken: StringSpan = this.get(this.size - 1).asInstanceOf[StringSpan]
                lastToken.setNumericProperty("split-lhs", 1)
                token.setNumericProperty("split-hyphen", 1)
                _constructionInfo._preHyphToken = lastToken
                _constructionInfo._hyphToken = token
              }
            }
            else {
              this.add(token)
              if (!termCombined) {
                _constructionInfo._localDict.add(token.getText.toLowerCase)
              }
            }
            if (lineEndp && termCombined) {
              val phantomToken: StringSpan = new StringSpan(_constructionInfo._docText, spanStart, spanEnd)
              if (firstTokenOnPage) {
                phantomToken.setNumericProperty("newpage", 1)
                firstTokenOnPage = false
              }
              phantomToken.setNumericProperty("newline", 1)
              phantomToken.setNumericProperty("llx", llx)
              phantomToken.setNumericProperty("lly", lly)
              phantomToken.setNumericProperty("phantom", 1)
              phantomToken.setProperty("divElement", tbox)

              this.add(phantomToken)
            }
          }
          _constructionInfo.textofs += boxText.length
        }
        _lineSpans += lineCompositeSpan.asInstanceOf[Span]

      }//end of else
    }


  } //tokenizeLines ends here



  private def deHyphenateDocument(globalDict: EnglishDictionary) {
    val termI: Iterator[_] = _constructionInfo._wrappedSpans.iterator
    while (termI.hasNext) {
      val terms: Array[StringSpan] = termI.next.asInstanceOf[Array[StringSpan]]
      val lhs: StringSpan = terms(0)
      val hyphen: StringSpan = terms(1)
      val rhs: StringSpan = terms(2)
      val combinedText: String = lhs.getText + rhs.getText
      var normalText: String = combinedText.replaceAll("\\W+", "")
      normalText = normalText.trim.toLowerCase
      if (globalDict.contains(normalText) || _constructionInfo._localDict.contains(normalText)) {
        lhs.setNumericProperty("invisible", 1)
        lhs.setProperty("original-text", lhs.getText)
        lhs.setText("")
        hyphen.setNumericProperty("invisible", 1)
        hyphen.setProperty("original-text", hyphen.getText)
        hyphen.setText("")
        rhs.setProperty("original-text", rhs.getText)
        rhs.setText(combinedText)
      }
    }
  }


  private def findFontNames(): List[String] = {
    val fonts = {
      val lineBIndexSet = _annotator.getBIndexSet(Single(SegmentCon("line")))
      val fonts: Set[String] = lineBIndexSet.map {
        case (index) =>
          val elements = _annotator.getElements("line")(index)
          elements.map { x =>
            x._2.getAttribute("font-family").getValue
          }
      }.flatten
      fonts
    }
    val disFonts = fonts.toList.distinct
    disFonts
  }
  private def getFontFamily(div:Element):String =
  {
    div.getAttribute("font-family").getValue
  }

  private def getPageNumber(div:Element):Int = {
    val found = regPageNumber.findAllIn(div.getAttribute("id").getValue)
    return found.group("page").toInt
  }

  private def getFontSize(div:Element):Double =
  {
    val rawValue:String = div.getAttribute("font-size").getValue
    val pxIdx = rawValue.indexOf("px")
    if (pxIdx >= 0) {
      rawValue.substring(0, pxIdx).toDouble
    } else rawValue.toDouble
  }

  private def getCoordLlx(element:Element):Double =
  {
    if(element.getAttribute("x")==null)
    {
      return 0.0
    }
    val foundX = regLlx.findAllIn(element.getAttribute("x").getValue)
    val newCoordLlx = foundX.group("llx").toDouble
    return newCoordLlx
  }

  private def getCoordLly(element:Element):Double =
  {
    if(element.getAttribute("y")==null)
    {
      return 0.0
    }
    val foundY = regLly.findAllIn(element.getAttribute("y").getValue)
//    if(foundY.isEmpty)
//    {
//      return 0.0
//    }
    val newCoordLly = foundY.group("top").toDouble
    return newCoordLly
  }

  private def getTransformationTypeV2(element:Element):String =
  {
    if(element.getAttribute("transform")==null)
    {
      return ""
    }
    var foundTVector = regTransformationMatrix.findAllIn(element.getAttribute("transform").getValue)
    if(!foundTVector.isEmpty)
    {
      return "matrix"
    }

    foundTVector = regTransformationTranslate.findAllIn(element.getAttribute("transform").getValue)
    if(!foundTVector.isEmpty)
    {
      return "translate"
    }

    return ""
  }

  private def getTransformationType(element:Element):String =
  {
    var foundTVector = regTransformationMatrix.findAllIn(element.getParentElement.getAttribute("transform").getValue)
    if(!foundTVector.isEmpty)
    {
        return "matrix"
    }

    foundTVector = regTransformationTranslate.findAllIn(element.getParentElement.getAttribute("transform").getValue)
    if(!foundTVector.isEmpty)
    {
      return "translate"
    }

    return ""
  }

  private def getTransformationMatrixVectorV2(element:Element):Vector[Double] =
  {
    val foundTVector = regTransformationMatrix.findAllIn(element.getAttribute("transform").getValue)
    foundTVector.hasNext
    Vector(foundTVector.group("a").toDouble,
      foundTVector.group("b").toDouble,
      foundTVector.group("c").toDouble,
      foundTVector.group("d").toDouble,
      foundTVector.group("e").toDouble,
      foundTVector.group("f").toDouble)
  }

  private def getTransformationTranslateVectorV2(element:Element):Vector[Double] =
  {
    val foundTVector = regTransformationTranslate.findAllIn(element.getAttribute("transform").getValue)
    foundTVector.hasNext
    Vector(foundTVector.group("a").toDouble,
      foundTVector.group("b").toDouble)
  }


  private def getTransformationMatrixVector(element:Element):Vector[Double] =
  {
    val foundTVector = regTransformationMatrix.findAllIn(element.getParentElement.getAttribute("transform").getValue)
    foundTVector.hasNext
    Vector(foundTVector.group("a").toDouble,
      foundTVector.group("b").toDouble,
      foundTVector.group("c").toDouble,
      foundTVector.group("d").toDouble,
      foundTVector.group("e").toDouble,
      foundTVector.group("f").toDouble)
  }

  private def getTransformationTranslateVector(element:Element):Vector[Double] =
  {
    val foundTVector = regTransformationTranslate.findAllIn(element.getParentElement.getAttribute("transform").getValue)
    foundTVector.hasNext
    Vector(foundTVector.group("a").toDouble,
      foundTVector.group("b").toDouble)
  }

  private def getLlx(newCoordLlx:Double, newCoordLly:Double, transformation:Vector[Double]):Double =
  {
    return transformation(0)*newCoordLlx + transformation(2)*newCoordLly + transformation(4)
  }
  private def getLly(newCoordLlx:Double, newCoordLly:Double, transformation:Vector[Double]):Double = {
    return transformation(1)*newCoordLlx + transformation(3)*newCoordLly + transformation(5)
  }

  private def getLlxTranslate(newCoordLlx:Double, transformation:Vector[Double]):Double =
  {
    return transformation(0) + newCoordLlx
  }
  private def getLlyTranslate(newCoordLly:Double, transformation:Vector[Double]):Double =
  {
    return transformation(1) + newCoordLly
  }

  private def getCoordinates(elem:Element, currMarginX:Double, currMarginY:Double):(Double, Double) =
  {
    val currX = getCoordLlx(elem)
    val currY = getCoordLly(elem)
    val transformationType:String = getTransformationTypeV2(elem)
    val retCoord = transformationType match {
      case "matrix" =>
        val transformationVector:Vector[Double] = getTransformationMatrixVectorV2(elem)
        val llx: Double = getLlx(currX + currMarginX, currY + currMarginY, transformationVector) //getLlx(firstTbox).toInt
        val lly: Double = getLly(currX + currMarginX, currY + currMarginY, transformationVector) //getLlx(firstTbox).toInt
        (llx, lly)
      case "translate" =>
        val transformationVector:Vector[Double] = getTransformationTranslateVectorV2(elem)
        val llx: Double = getLlxTranslate(currX + currMarginX, transformationVector) //getLlx(firstTbox).toInt
        val lly: Double = getLlyTranslate(currY + currMarginY, transformationVector)
        (llx, lly)
      case "" =>
        (currX + currMarginX, currY + currMarginY)
    }

    if(elem.getParentElement != null)
    {
      return getCoordinates(elem.getParentElement, retCoord._1, retCoord._2)
    }
    else
    {
      return (retCoord._1, retCoord._2)
    }
  }

  private def getLlxV2(element:Element):Double =
  {
    val transformationType:String = getTransformationType(element)

    val llx = transformationType match {
      case "matrix" =>
        val transformationVector:Vector[Double] = getTransformationMatrixVector(element)
        val coordLlx:Double = getCoordLlx(element)
        val coordLly:Double = getCoordLly(element)
        val llx: Double = getLlx(coordLlx, coordLly, transformationVector) //getLlx(firstTbox).toInt
        llx
      case "translate" =>
        val transformationVector:Vector[Double] = getTransformationTranslateVector(element)
        val coordLlx:Double = getCoordLlx(element)
        val llx: Double = getLlxTranslate(coordLlx, transformationVector) //getLlx(firstTbox).toInt
        llx
    }
    llx
  }
  private def getLlyV2(element:Element):Double = {

    val transformationType:String = getTransformationType(element)

    val lly = transformationType match {
      case "matrix" =>
        val transformationVector:Vector[Double] = getTransformationMatrixVector(element)
        val coordLlx:Double = getCoordLlx(element)
        val coordLly:Double = getCoordLly(element)
        val lly: Double = getLly(coordLlx, coordLly, transformationVector) //getLlx(firstTbox).toInt
        lly
      case "translate" =>
        val transformationVector:Vector[Double] = getTransformationTranslateVector(element)
        val coordLly:Double = getCoordLly(element)
        val lly: Double = getLlyTranslate(coordLly, transformationVector) //getLlx(firstTbox).toInt
        lly
    }
    lly
  }

  private def getWidth(element:Element):Double = element.getAttribute("data-canvas-width").getDoubleValue


  private def findNormalFontSizeSvg(/*xmlDoc: Document*/): Double = {
    val sizes = {
      val lineBIndexSet = _annotator.getBIndexSet(Single(SegmentCon("line")))
      lineBIndexSet.toList.flatMap{
        case (index) =>
          val elements = _annotator.getElements("line")(index)
          elements.map { x =>
            val currVal = x._2.getAttribute("font-size").getValue

            if (currVal.indexOf("px") >= 0) {
              currVal.substring(0, currVal.indexOf("px"))
            } else currVal
          }
      }
    }.groupBy(_.toString).mapValues(_.count(x => true))
    sizes.toList.maxBy(_._2)._1.toDouble
  }

  private def findNormalFontSize(xmlDoc:Iterable[Element]): Double = {
    val counts = new util.HashMap[Double, Int]
    for(currElem <- xmlDoc){
      val fontSize:Double = getFontSize(currElem)
      var count: Int = 1
      if (counts.containsKey(fontSize)) {
        count = count + counts.get(fontSize)
      }
      counts.put(fontSize, count)
    }

    var mostCommonSize: Double = -1
    var maxCount = -1
    val sizeI: java.util.Iterator[Double] = counts.keySet.iterator
    while (sizeI.hasNext) {
      val size: Double = sizeI.next
      val count = counts.get(size)
      if (count.compareTo(maxCount) > 0) {
        maxCount = count
        mostCommonSize = size
      }
    }
    if (mostCommonSize != -1) {
      return mostCommonSize
    }
    else {
      return -1
    }
  }

  private def findLargestFontSize(): Double = {
    val sizes = {
      val lineBIndexSet = _annotator.getBIndexSet(Single(SegmentCon("line")))
      lineBIndexSet.flatMap {
        case (index) =>
          val elements = _annotator.getElements("line")(index)
          elements.map{x=>
            val currVal = x._2.getAttribute("font-size").getValue
            if (currVal.indexOf("px") >= 0) {
              currVal.substring(0, currVal.indexOf("px"))
            } else currVal
          }
      }
    }
    sizes.map(_.toDouble).max
  }

  def getTopAncestor(node:Element): Element =
  {
    if(node.getParentElement!=null)
    {
      return getTopAncestor(node.getParentElement)
    }
    else
    {
      return node
    }
  }


  def getTopPageElement(node:Element, doc:Document): Element =
  {
    if(node.getParent!=doc.getRootElement)
    {
      return getTopPageElement(node.getParentElement, doc)
    }
    else
    {
      return node
    }
  }

  def getTopPageAncestor(node:Element, doc:Document): Element = {
    if(node.getParentElement != null)
      getTopPageAncestor(node.getParentElement, doc)
    else
      node
  }

  def getTopPageAncestorV2(node:Element, doc:Document): Element = {
    //TODO: top, but within the page
    if(node.getParent.getParent!=doc.getRootElement)
      getTopPageAncestorV2(node.getParentElement, doc)
    else
      node
  }

  private def initHeaderFooterLineCounts(): Map[String, Int] = {

    val lines = {
      val lineBIndexSet = _annotator.getBIndexSet(Single(SegmentCon("line")))
      val lastLine:Int  = lineBIndexSet.size
      val firstLine:Int = 1
      val res = lineBIndexSet.toList.zipWithIndex.map {
        case (lineBIndex, lineRelIndex) =>
          val lineText = _annotator.getTextOption("line")(lineBIndex).map(_._2).getOrElse("")

          val isTopOrBottomLine: Boolean = (lineRelIndex == firstLine || lineRelIndex == lastLine)
          val firstTbox:Element = _annotator.getElements("line")(lineBIndex).values.toList(0)
//          val coords = getCoordinates(firstTbox, 0.0, 0.0)

          val coords2 = Annotator.getTransformedCoords(firstTbox, _annotator.getDom().getRootElement)//getTopPageAncestor(firstTbox,_annotator.getDom()))
          val llx = coords2.xs(0)
          val lly = coords2.ys(0)

//          val llx:Double = coords._1
//          val lly:Double = coords._2
//          val llx:Double = getLlxV2(firstTbox)
//          val lly:Double = getLlyV2(firstTbox)

          val fontNumber: String = getFontFamily(firstTbox)
          NewHtmlTokenization.headerFooterNormalize(lineText, isTopOrBottomLine, llx.toInt, lly.toInt, fontNumber)
      }
      res
    }.groupBy(_.toString).mapValues(_.count(x=>true))

    lines
  }

  def getLineSpans: mutable.ArrayBuffer[Span] =  _lineSpans

  def getFormattedText: String = {
    val stringBuffer = new StringBuffer()
    var i = 0
    while (i < _lineSpans.size) {
      val span = _lineSpans(i)
      stringBuffer.append(span.getText).append("\n")
      i += 1
    }
    stringBuffer.toString
  }

  def getSubspanTokenization(startIdx: Int, endIdx: Int): NewHtmlTokenizationSvg = {
    val subTokens: mutable.MutableList[Span] = mutable.MutableList[Span]()
    val lines: mutable.ArrayBuffer[Span] = mutable.ArrayBuffer[Span]()
    val subActiveSpans: mutable.MutableList[Span] = mutable.MutableList[Span]()
    var foundStart: Boolean = false
    var foundEnd: Boolean = false

    var i: Int = 0
    var haveToBreak:Boolean = false;

    var tt:Long = System.currentTimeMillis()


    while (!haveToBreak && i < size) {
      val span = getToken(i).asInstanceOf[Span]
      if (!foundStart) {
        if (startIdx <= span.getStartIdx) {
          subTokens += span
          foundStart = true
          if (activeSpanList != null && activeSpanList.size > 0) {
            subActiveSpans += activeSpanList(i)
          }
        }
      }
      else if (!foundEnd) {
        if (span.getStartIdx >= endIdx) {
          foundEnd = true
        }
        else {
          subTokens += span
          if (activeSpanList != null && activeSpanList.size > 0) {
            subActiveSpans += activeSpanList(i)
          }
        }
      }
      else haveToBreak = true
      i += 1
    }

    haveToBreak = false
    foundStart = false
    foundEnd = false

    tt = System.currentTimeMillis()
    i = 0
    while (!haveToBreak && i < _lineSpans.size) {
      val span = _lineSpans(i)
      if (!foundStart) {
        if (startIdx <= span.getStartIdx) {
          lines += span
          foundStart = true
        }
      }
      else if (!foundEnd) {
        if (span.getStartIdx >= endIdx) {
          foundEnd = true
        }
        else {
          lines += span
        }
      }
      else {
        haveToBreak = true
      }
      i += 1
    }

    new NewHtmlTokenizationSvg(_document, subTokens.toArray /*.asInstanceOf[Array[Span]]*/,
      subActiveSpans, lines)
  }
  def getActiveSpanList: mutable.MutableList[Span] = activeSpanList

  def subspan(firstToken: Int, lastToken: Int): Span = {
    val firstSpan: StringSpan = getToken(firstToken).asInstanceOf[StringSpan]
    val startIdx: Int = firstSpan.getStartIdx
    var endIdx: Int = 0
    if (lastToken > size) {
      endIdx = _document.length
    }
    else {
      val lastSpan: StringSpan = getToken(lastToken - 1).asInstanceOf[StringSpan]
      endIdx = lastSpan.getEndIdx
    }
    new StringSpan(_document, startIdx, endIdx)
  }

  def getSpan(i: Int): Span = getToken(i).asInstanceOf[Span]

  def getDocument = _document
  private var activeSpanList: mutable.MutableList[Span] = mutable.MutableList()
  private val trailingWS: Pattern = Pattern.compile("\\s+$")

}
