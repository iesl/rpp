package org.rexo.extraction

import org.jdom2.input.SAXBuilder
import org.jdom2.Document
import org.rexo.util.EnglishDictionary
import scala.collection.mutable

import java.util.regex.{Matcher, Pattern}
import org.rexo.extra.extract.Span
import java.util
import org.rexo.extra.types.{PropertyHolder, TokenSequence}
import org.rexo.base.extract.Tokenization

//import old2.TokenSequence

//import old.base.extract.StringSpan
import org.jdom2.xpath.{XPathExpression, XPathFactory}
import org.jdom2.filter.Filters

import org.rexo.span.CompositeSpan
import org.rexo.extra.extract.StringSpan
import org.rexo.extra.utils.CharSequenceLexer
import org.jdom2.Element
import scala.collection.JavaConverters._

/**
 * Created by klimzaporojets on 9/17/14.
 */
object NewHtmlTokenization {

  var lastBracketId = 0;
  private def isPaginationText(counts: Map[String, Integer], s: String, isTopOrBottomLine: Boolean,
                               llx: Int, lly: Int, fontnumber: String /*fontnumber: Int*/): Boolean = {
    val normalizedLine: String = headerFooterNormalize(s, isTopOrBottomLine, llx, lly, fontnumber)
    val count: Int = (counts.get(normalizedLine).get.asInstanceOf[Integer]).intValue //(counts.get(normalizedLine).asInstanceOf[Integer]).intValue
    return count >= 3
  }


  def headerFooterNormalize(string: String, isTopOrBottomLine: Boolean,
                                    llx: Int, lly: Int, fontNum:String /*fontNum: Int*/): String = {
    var ret: String = NewHtmlTokenization.DIGIT_OR_SPACE.matcher(string).replaceAll("")
    ret = ret + " " + llx + "," + lly + "," + fontNum
    return ret
  }



  def createNewHtmlTokenization(xmlDoc: Document, globalDict: EnglishDictionary): NewHtmlTokenization = {
    return new NewHtmlTokenization(xmlDoc, globalDict)
  }

//  private final val serialVersionUID: Long = 1L
  final val LEXER_PATTERN: Pattern = Pattern.compile("(``|''|\\n|&amp;|1st|2nd|3rd|[4-9]th|Ph.D.|\\S+@\\S+|\\w\\.|\\+[A-Z]+\\+|\\p{Alpha}+|\\p{Digit}+|\\p{Punct}|^)\\s*")
//  final val LEXER_PATTERN: Pattern = Pattern.compile("(``|''|\n|&amp;|1st|2nd|3rd|[4-9]th|Ph.D.|\S+@\S+|\w\.|\+[A-Z]+\+|\p{Alpha}+|\p{Digit}+|\p{Punct}|^)\s*")
  final val DIGIT_OR_SPACE: Pattern = Pattern.compile("\\d+| ")

}

//class NewHtmlTokenization extends TokenSequence with Tokenization {

class NewHtmlTokenization extends TokenSequence with Tokenization{

  private var _lineSpans: mutable.ArrayBuffer[Span] = null //mutable.MutableList[Span] = null // [StringSpan] = null //List[StringSpan] = null
  private var _document: CharSequence = null
  //stores the complete html structure that was used as the input
  var _parsedDocument: Document = null

  val regFontFamily = new scala.util.matching.Regex(""".*font\-family:([\-0-9a-zA-Z\s]+);.*""", "font")
  val regPageNumber = new scala.util.matching.Regex("""pageContainer([0-9]+)""", "page")
  val regFontSize = new scala.util.matching.Regex(""".*font\-size:[\s]+([0-9\.]+)px;.*""", "fontsize")
  val regLlx = new scala.util.matching.Regex(""".*left:([\s\-0-9\.]+)px;.*""", "llx")
  val regLly = new scala.util.matching.Regex(""".*top:([\s\-0-9\.]+)px;.*""", "top")


  private def this(document: CharSequence, tokenSpans: Array[AnyRef], activeSpanList: mutable.MutableList[Span],
                   lineSpans: mutable.ArrayBuffer[Span] /*mutable.MutableList[Span]*/) {
    this()
    this._document = document
    this.addAll(tokenSpans) //addAll(tokenSpans)
    this.activeSpanList = activeSpanList
    this._lineSpans = lineSpans
  }


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

  private var _constructionInfo: NewHtmlTokenization#ConstructionInfo = null


  private class ConstructionInfo {
    var _fontNames: List[String] = null // Array[String] = null
    private[extraction] var _normalFontSize: Double = .0
    private[extraction] var _largestFontSize: Double = .0
    private[extraction] var _headerFooterLineCounts: Map[String, Integer] = null //originally HashMap
    private[extraction] var _wrappedSpans: mutable.MutableList[Array[StringSpan]] = null //List[StringSpan] = null //originally ArrayList


    private[extraction] var fontName: String = ""
    private[extraction] var lastFontName: String = ""
    private[extraction] var fontSize: Double = -1
    private[extraction] var lastFontSize: Double = -1
    private[extraction] var textofs: Int = 0
    private[extraction] var globalLineNum: Int = -1
    var _hyphToken: StringSpan = null
    var _preHyphToken: StringSpan = null
    private[extraction] var _docText: StringBuffer = new StringBuffer
    private[extraction] var _localDict: mutable.TreeSet[String] = new mutable.TreeSet[String]() //util.TreeSet[_] = new util.TreeSet[_]
  }
  /**
   * Creates a tokenization of .
   * <p/>
   * Tokens are added from all the matches of the given lexer in each line, as well as in between the the end and start of two successive lines if de-hyphonization can be applied.
   */
  def this(xmlDoc: Document, globalDict: EnglishDictionary) {
    this()
    _constructionInfo = new ConstructionInfo //NewHtmlTokenization#ConstructionInfo
    //kzaporojets: TODO, implement for the new html format, just commented to compile
    val xFactory:XPathFactory = XPathFactory.instance();
    val expr:XPathExpression[Element] = xFactory.compile("//div", Filters.element());
    val divs:util.Collection[Element] = expr.evaluate(xmlDoc);
    val onlyContent:Iterable[Element] = divs.asScala.filter(x => x.getAttribute("data-canvas-width")!=null)

    _constructionInfo._fontNames = findFontNames(xmlDoc)
    _constructionInfo._normalFontSize = findNormalFontSize(onlyContent)
    _constructionInfo._largestFontSize = findLargestFontSize(onlyContent)

    _constructionInfo._headerFooterLineCounts = initHeaderFooterLineCounts(xmlDoc)
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
    this._parsedDocument = xmlDoc
    val root: Element = xmlDoc.getRootElement
    tokenizePages(root)
    deHyphenateDocument(defGlobalDict/*globalDict*/)
    this._document = _constructionInfo._docText.toString
  }


  /**
   *
   * @param root
   */
  private def tokenizePages(root: Element) {

    val xFactory:XPathFactory = XPathFactory.instance();
    val expr:XPathExpression[Element] = xFactory.compile("//div[@class=\"page\"]", Filters.element());
    val pageList:util.Collection[Element] = expr.evaluate(root);

    val pageI: Iterator[Element] = pageList.iterator().asScala

    while (pageI.hasNext) {
      val page: Element = pageI.next.asInstanceOf[Element]
      tokenizeLines(page)
    }
  }

  /**
   *
   * @param page
   */
  private def tokenizeLines(page: Element) {

    val pageNum: Int = getPageNumber(page)

    val xFactory:XPathFactory = XPathFactory.instance();
    val expr:XPathExpression[Element] = xFactory.compile("div/div", Filters.element());
    val contentList:util.Collection[Element] = expr.evaluate(page);

    val onlyContent:Iterable[Element] = contentList.asScala.filter(x => x.getAttribute("data-canvas-width")!=null)

    //groups by lines
    val groupedByLineContent:Map[Int, Iterable[Element]] =
      onlyContent.groupBy(_.getAttribute("line-number").getIntValue);

    val lines:Set[Int] = groupedByLineContent.keySet

    val keysIterator:Iterator[Int] = groupedByLineContent.keysIterator

    val sortedLines:List[Int] = keysIterator.toList.sorted

    var firstTokenOnPage: Boolean = true

    //we are here
    val lastLine:Int  = lines.max
    val firstLine:Int = lines.min

    for(
      currentLine <- sortedLines
    ) {
      val line: Iterable[Element] = groupedByLineContent.get(currentLine).get

      val lineCompositeSpan: CompositeSpan = CompositeSpan.createSpan(_constructionInfo._docText)
      val tboxList: List[Element] = line.toList

      _constructionInfo.globalLineNum += 1
      val lineStartOfs: Int = _constructionInfo.textofs
      val firstTbox: Element = tboxList(0)

      var llx: Int = getLlx(firstTbox).toInt
      var lly: Int = getLly(firstTbox).toInt
      val fontFamily: String = getFontFamily(firstTbox)

      val isTopOrBottomLine: Boolean = (currentLine == firstLine || currentLine == lastLine)


      val lineText: String = groupedByLineContent.get(currentLine).get.map(x => x.getValue
      ).mkString("")
      //      val lineText: String = lineText(line)


      if (NewHtmlTokenization.isPaginationText(_constructionInfo._headerFooterLineCounts,
        lineText.toString, isTopOrBottomLine, llx, lly, fontFamily /*fontNumber*/)) {
        _constructionInfo._docText.append(lineText)
        _constructionInfo.textofs += lineText.length
        val paginationToken: StringSpan = new StringSpan(_constructionInfo._docText, lineStartOfs, _constructionInfo.textofs)
        paginationToken.setNumericProperty("isHeaderFooterLine", 1.0)
        var tboxI: Iterator[_] = tboxList.iterator
        tboxI = tboxList.iterator
        var locUrx: Int = -1
        var locUry: Int = -1
        var locLlx: Int = -1
        var locLly: Int = -1

        while (tboxI.hasNext) {
          val tbox: Element = tboxI.next.asInstanceOf[Element]
          val curLlx = getLlx(tbox).toInt
          val curLly = getLly(tbox).toInt

          val curUry = getFontSize(tbox).toInt + curLly
          val curUrx = curLlx + getWidth(tbox).toInt

          if (locUrx == -1 || curUrx  > locUrx) {
            locUrx = curUrx
          }
          if (locUry == -1 || curUry  <  locUry) {
            locUry = curUry
          }
          if (locLlx == -1 || curLlx  < locLlx) {
            locLlx = curLlx
          }
          if (locLly == -1 || curLly  < locLly) {
            locLly = curLly
          }
        }
        paginationToken.setProperty("llx", locLlx.toDouble)
        paginationToken.setProperty("lly", locLly.toDouble)
        paginationToken.setProperty("urx", locUrx.toDouble)
        paginationToken.setProperty("ury", locUry.toDouble )
        paginationToken.setProperty("pageNum", pageNum.toDouble )
        paginationToken.setProperty("divElement", tboxI)

        _lineSpans.+=(paginationToken.asInstanceOf[Span])
      }
      else
      {
        var tboxI: Iterator[_] = tboxList.iterator
        tboxI = tboxList.iterator
        var firstTokenInLine: Boolean = true
        while (tboxI.hasNext) {
          val tbox: Element = tboxI.next.asInstanceOf[Element]

          val curLlx = getLlx(tbox).toInt
          val curLly = getLly(tbox).toInt

          val curUry = getFontSize(tbox).toInt + curLly
          val curUrx = curLlx + getWidth(tbox).toInt

          llx = curLlx
          lly = curLly
          val urx: Int = curUrx
          val ury: Int = curUry

          _constructionInfo.fontSize = getFontSize(tbox)
          _constructionInfo.fontName = getFontFamily(tbox)
          var boxText: String = tbox.getText

          _constructionInfo._docText.append(boxText)
          var lexer: CharSequenceLexer = null
          lexer = CharSequenceLexer.apply(boxText, NewHtmlTokenization.LEXER_PATTERN)

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
              this.add(phantomToken)
            }
          }
          _constructionInfo.textofs += boxText.length
        }
        _lineSpans.+=(lineCompositeSpan.asInstanceOf[Span])

      }//end of else
    }


  }

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


  //new findFontNames scans the divs and extracts the fonts
  private def findFontNames(xmlDoc: Document): List[String] //Array[String]
  = {
    val xFactory:XPathFactory = XPathFactory.instance();
    val expr:XPathExpression[Element] = xFactory.compile("//div", Filters.element());
    val divs:util.Collection[Element] = expr.evaluate(xmlDoc);
    val reg = new scala.util.matching.Regex(""".*font\-family:([\-1-9a-zA-Z\s]+);.*""", "font")

    val values:Iterable[String] = for{
      currDivKey <- divs.asScala
      if currDivKey.getAttribute("style")!=null && currDivKey.getAttribute("style").getValue.contains("font-family")
    }
    yield {
      getFontFamily(currDivKey)
    }
    val distValues:List[String] = values.toList.distinct

    distValues;
  }
  private def getFontFamily(div:Element):String =
  {
    val found = regFontFamily.findAllIn(div.getAttribute("style").getValue)
    found.hasNext
    return found.group("font")
  }

  private def getPageNumber(div:Element):Int =
  {
    val found = regPageNumber.findAllIn(div.getAttribute("id").getValue)
    found.hasNext
    return found.group("page").toInt
  }

  private def getFontSize(div:Element):Double =
  {

    val values = regFontSize.findAllIn(div.getAttribute("style").getValue)
    values.hasNext
    val fontSize: Double = (values.group("fontsize")).toDouble
    return fontSize
  }
  private def getLlx(element:Element):Double =
  {
    val found = regLlx.findAllIn(element.getAttribute("style").getValue)
    found.hasNext
    return found.group("llx").toDouble
  }

  private def getLly(element:Element):Double =
  {
    val found = regLly.findAllIn(element.getAttribute("style").getValue)
    found.hasNext
    val top:Double = found group "top" toDouble
    val fontSize:Double = getFontSize(element)

    return top + fontSize

  }

  private def getWidth(element:Element):Double =
  {
    val width = element.getAttribute("data-canvas-width").getDoubleValue
    return width;
  }



  private def findNormalFontSize(xmlDoc:Iterable[Element]): Double = {
    val counts: util.HashMap[Double, Integer] = new util.HashMap[Double, Integer]
    for(currElem <- xmlDoc)
    {
      val fontSize:Double = getFontSize(currElem)
      var count: Int = 1
      if (counts.containsKey(fontSize)) {
        count = count + (counts.get(fontSize).asInstanceOf[Integer]).intValue
      }
      counts.put(fontSize, new Integer(count))
    }

    var mostCommonSize: Double = -1
    var maxCount: Integer = new Integer(-1)
    val sizeI: java.util.Iterator[Double] = counts.keySet.iterator
    while (sizeI.hasNext) {
      val size: Double = sizeI.next
      val count: Integer = counts.get(size).asInstanceOf[Integer]
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
//

  private def findLargestFontSize(xmlDoc:Iterable[Element] ): Double = {
    var largestSize: Double = -1

    for(currElem <- xmlDoc)
    {
      val style:String = currElem.getAttribute("style").getValue

      val reg = new scala.util.matching.Regex(""".*font\-size:[\s]+([0-9\.]+)px;.*""", "fontsize")

      val values = reg.findAllIn(style)
      values.hasNext
      val fontSize: Double = (values.group("fontsize")).toDouble
      if(fontSize>largestSize)
      {
        largestSize = fontSize
      }
    }
    return largestSize
  }

  //adapted from Java, TODO: use Scala to optimize the code
  private def initHeaderFooterLineCounts(xmlDoc: Document): Map[String, Integer] = {
    val counts: util.HashMap[String, Integer] = new util.HashMap[String, Integer]

    val xFactory:XPathFactory = XPathFactory.instance();
    val expr:XPathExpression[Element] = xFactory.compile("//div[@class=\"page\"]", Filters.element());
    val pageList:util.Collection[Element] = expr.evaluate(xmlDoc);

    val pageI: Iterator[Element] = pageList.iterator().asScala
    while (pageI.hasNext) {
      val page: Element = pageI.next.asInstanceOf[Element]
      val expr:XPathExpression[Element] = xFactory.compile("div/div", Filters.element());
      val contentList:util.Collection[Element] = expr.evaluate(page);

      val onlyContent:Iterable[Element] = contentList.asScala.filter(x => x.getAttribute("data-canvas-width")!=null)

      //groups by lines
      val groupedByLineContent:Map[Int, Iterable[Element]] =
                          onlyContent.groupBy(_.getAttribute("line-number").getIntValue);

      val lines:Set[Int] = groupedByLineContent.keySet

      val lastLine:Int  = lines.max
      val firstLine:Int = lines.min

      val keysIterator:Iterator[Int] = groupedByLineContent.keysIterator

      while ( keysIterator.hasNext ) {
        val line: Int = keysIterator.next
        val tboxList: List[Element] = groupedByLineContent.get(line).get.toList

        val firstTbox: Element = tboxList(0).asInstanceOf[Element]

        //todo: see if it is necessary to transform to Int or if it can be used with more precision
        //by using double, in the original metatagger, int is used
        val llx: Int = getLlx(firstTbox).toInt
        val lly: Int = getLly(firstTbox).toInt
        //it will be font Family in the new format
        val fontNumber: String = getFontFamily(firstTbox)

        val isTopOrBottomLine: Boolean = (line == firstLine || line == lastLine)

        val lineText: String = groupedByLineContent.get(line).get.map(x=>x.getValue
        ).mkString("")

        val normalizedLine: String = NewHtmlTokenization.headerFooterNormalize(lineText, isTopOrBottomLine, llx, lly, fontNumber)
        var count: Int = 1
        if (counts.containsKey(normalizedLine)) {
          count += (counts.get(normalizedLine).asInstanceOf[Integer]).intValue
        }
        counts.put(normalizedLine, new Integer(count))
      }
    }
    return counts.asScala.toMap
  }

  def getLineSpans: mutable.ArrayBuffer[Span] = {
    return _lineSpans
  }
//
  def getFormattedText: String = {
    val stringBuffer: StringBuffer = new StringBuffer();
    {
      var i: Int = 0
      while (i < _lineSpans.size) {
        {
          val span: Span = _lineSpans(i)
          stringBuffer.append(span.getText).append("\n")
        }
        i += 1;
      }
    }
    return stringBuffer.toString
  }

  def getSubspanTokenization(startIdx: Int, endIdx: Int): NewHtmlTokenization = {
    val subTokens: mutable.MutableList[Span] = mutable.MutableList[Span]() //ArrayList[_] = new ArrayList[_]
    val lines: mutable.ArrayBuffer[Span] = mutable.ArrayBuffer[Span]() //mutable.MutableList[Span] = mutable.MutableList[Span]() //util.ArrayList[_] = new ArrayList[_]
    val subActiveSpans: mutable.MutableList[Span] = mutable.MutableList[Span]()  //ArrayList[_] = new ArrayList[_]
    var foundStart: Boolean = false
    var foundEnd: Boolean = false


    var i: Int = 0
    var haveToBreak:Boolean = false;

    var tt:Long = System.currentTimeMillis()


    while (!haveToBreak && i < size) {
      {
        val span: Span = getToken(i).asInstanceOf[Span]
        if (!foundStart) {
          if (startIdx <= span.getStartIdx) {
            subTokens.+=(span)
            foundStart = true
            if (activeSpanList != null && activeSpanList.size > 0) {
              subActiveSpans.+=(activeSpanList(i))
            }
          }
        }
        else if (!foundEnd) {
          if (span.getStartIdx >= endIdx) {
            foundEnd = true
          }
          else {
            subTokens.+=(span)
            if (activeSpanList != null && activeSpanList.size > 0) {
              subActiveSpans.+=(activeSpanList.get(i).get)
            }
          }
        }
        else {
          haveToBreak = true
        }
      }
      i = i + 1;
    }

    haveToBreak = false;
    foundStart = false
    foundEnd = false

    tt = System.currentTimeMillis()
    i = 0;
    while (!haveToBreak && i < _lineSpans.size) {
      {
        val span: Span = _lineSpans(i)
        if (!foundStart) {
          if (startIdx <= span.getStartIdx) {
            lines.+=(span)
            foundStart = true
          }
        }
        else if (!foundEnd) {
          if (span.getStartIdx >= endIdx) {
            foundEnd = true
          }
          else {
            lines.+=(span)
          }
        }
        else {
          //break //todo: break is not supported
          haveToBreak = true
        }
      }
      i += 1;
    }

    return new NewHtmlTokenization(_document, subTokens.toArray /*.asInstanceOf[Array[Span]]*/,
                subActiveSpans, lines)
  }
  def getActiveSpanList: mutable.MutableList[Span] = {
    return activeSpanList
  }
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
    return new StringSpan(_document, startIdx, endIdx)
  }
//
  def getSpan(i: Int): Span = {
    return getToken(i).asInstanceOf[Span]
  }

  def getDocument: AnyRef = {
    return _document
  }
  private var activeSpanList: mutable.MutableList[Span] = mutable.MutableList()
  private var trailingWS: Pattern = Pattern.compile("\\s+$")

}
