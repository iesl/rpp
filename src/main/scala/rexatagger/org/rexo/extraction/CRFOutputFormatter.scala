package org.rexo.extraction

//import org.apache.log4j.Logger
import org.jdom2.{Namespace, Attribute, DataConversionException, Element}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import org.rexo.span.CompositeSpan
import org.rexo.extra.types.{PropertyHolder, Sequence}
import java.util.jar.Attributes
import scala.collection.mutable
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils
import scala.collection.mutable.ArrayBuffer

//import edu.umass.cs.mallet.base.extract.Span

//import old.base.types.{PropertyHolder, Sequence}
import org.rexo.extra.extract.{StringSpan, Span}
//import old.org.rexo.CompositeSpan
import scala.collection.JavaConverters._


/**
* Created by klimzaporojets on 9/26/14.
*/
/**
* Author: saunders Created Nov 16, 2005 Copyright (C) Univ. of Massachusetts Amherst, Computer Science Dept.
*/
object CRFOutputFormatter {
//  private var log: Logger = Logger.getLogger(classOf[CRFOutputFormatter])
//  val log = Logger(LoggerFactory.getLogger("ScalaTagger"))
}

class CRFOutputFormatter {
  val log = Logger(LoggerFactory.getLogger("ScalaTagger"))

  //the Span also includes the offset inside a particular div ("startOffset" and "endOffset")
  val openBrackets:collection.mutable.Map[String, Span] = collection.mutable.Map[String, Span]()

  //where the brackets actually were opened (bracket-begin tag)
  val startBrackets:collection.mutable.Map[String, Span] = collection.mutable.Map[String, Span]()

  //the ids of different labels
  val labelIds:collection.mutable.Map[String, String] = collection.mutable.Map[String, String]()

  //the labels for which bracket-outside/bracket-inside apply
  val bracketOutsideInsideApply:ArrayBuffer[String] = ArrayBuffer[String]("paragraph-marker")

  private def updateToken(token:Span, labelParts: Array[String],
                          previousLabels: Array[String]) {

    var diffLabels = labelParts.filter(x => !previousLabels.exists(y=> y.indexOf(x)>=0)) //labelParts.toSet.&~(previousLabels.toSet)

//    println("the value of diffLabels is: " + diffLabels)

    for(
      diffLabel <- diffLabels
    )
    {
      var labelPart = diffLabel
      //the ^ symbol indicates that should start new bracket-begin of the label (closing others that might be opened)
      if(diffLabel.startsWith("^"))
      {
        labelPart = diffLabel.replaceFirst("^\\^", "")
        if(labelPart.indexOf("--_--") > -1)
        {
          labelPart = labelPart.substring(0, labelPart.indexOf("--_--"))

        }
        val extraAttrs:collection.mutable.Map[String, String] =  getExtraAttrs(diffLabel);
        //if the id already exist, it either does nothing or inserts bracket-outside/bracket-inside
        val idValue = extraAttrs.get("id")
        val idLabel = labelIds.get(labelPart)
        if(idValue!=None && idLabel!=None && idLabel.get == idValue.get)
        {

          if(bracketOutsideApplies(token,labelPart))
          {
            //TODO: repeated code, make function of it
            val children = LayoutUtils.getProperty(startBrackets.get(labelPart).get, "divElement")
              .asInstanceOf[Element].getChildren.asScala

            val elem = children.find(x =>
              if(x.getAttribute("type") != null
                && x.getAttribute("type").getValue.equals(labelPart))
              {
                true
              }
              else
              {
                false
              }
            ).get

            updateBracketSpan(openBrackets.get(labelPart).get,false,labelPart, "bracket-outside", elem.getAttribute("id-n").getIntValue)
            updateBracketSpan(token,true,labelPart, "bracket-inside", elem.getAttribute("id-n").getIntValue)
          }
        }
        else
        {
          startNewBracket(labelPart, token)
        }

        if(idValue!=None)
        {
          labelIds.put(labelPart, idValue.get)
        }
      }
      else
      {
        if(openBrackets.contains(labelPart))
        {
          try{
              val elem = LayoutUtils.getProperty(startBrackets.get(labelPart).get, "divElement")
                          .asInstanceOf[Element].getChildren.asScala.find(x => x.getAttribute("type") != null && x.getAttribute("type").getValue == diffLabel).get

              if(bracketOutsideInsideApply.contains(labelPart))
              {
                updateBracketSpan(openBrackets.get(labelPart).get,false,labelPart, "bracket-outside", elem.getAttribute("id-n").getIntValue)
                updateBracketSpan(token,true,labelPart, "bracket-inside", elem.getAttribute("id-n").getIntValue)

              }
              else
              {
                startNewBracket(labelPart, token)
//                updateBracketSpan(openBrackets.get(labelPart).get,false,labelPart, "bracket-end", elem.getAttribute("id-n").getIntValue)
//                updateBracketSpan(token,true,labelPart, "bracket-begin", NewHtmlTokenization.lastBracketId)
//                NewHtmlTokenization.lastBracketId += 1
              }
            }
            catch {
              case e: Exception => {
                e.printStackTrace()
              }
          }
        }
        else
        {
//          updateBracketSpan(token,true,diffLabel, "bracket-begin", NewHtmlTokenization.lastBracketId)
//          startBrackets.put(labelPart, token)
//          NewHtmlTokenization.lastBracketId += 1
          startNewBracket(labelPart, token)
        }
      }
      openBrackets.put(labelPart, token)
    }

    //another situation may happen when the labels are common between the previous and current, but
    //the end index of the previous doesn't correspond to the start index of the current, in this case
    //it means that another element (such as header or footer) is in between. So, "bracket-inside" and
    // "bracket-outside" should be added
    diffLabels = labelParts.filter(x => previousLabels.exists(y=> y.indexOf(x)>=0)) //labelParts.toSet.&(previousLabels.toSet)

//    println("the value of diffLabels is: " + diffLabels)

    for(
      diffLabel <- diffLabels
    )
    {
      var labelPart = diffLabel

      if(diffLabel.startsWith("^")) {
        labelPart = diffLabel.replaceFirst("^\\^", "")
        if(labelPart.indexOf("--_--") > -1)
        {
          labelPart = labelPart.substring(0, labelPart.indexOf("--_--"))
        }
      }
      if (bracketOutsideApplies(token,labelPart))
      {
        val children = LayoutUtils.getProperty(startBrackets.get(labelPart).get, "divElement")
          .asInstanceOf[Element].getChildren.asScala

        val elem = children.find(x =>
          if(x.getAttribute("type") != null
            && x.getAttribute("type").getValue.equals(labelPart))
          {
            true
          }
          else
          {
            false
          }
          ).get

        updateBracketSpan(openBrackets.get(labelPart).get,false,labelPart, "bracket-outside", elem.getAttribute("id-n").getIntValue)
        updateBracketSpan(token,true,labelPart, "bracket-inside", elem.getAttribute("id-n").getIntValue)
      }
      openBrackets.put(labelPart, token)
    }
  }

  def bracketOutsideApplies(token:Span, labelPart:String):Boolean =
  {
    return (token.getStartIdx!=openBrackets.get(labelPart).get.getEndIdx &&
        !(LayoutUtils.getProperty(token, "split-rhs")!= null &&
          LayoutUtils.getProperty(token, "split-rhs").toString == "1.0" &&
          token.getStartIdx == token.getStartIdx!=openBrackets.get(labelPart).get.getEndIdx+1) &&
        LayoutUtils.getProperty(token, "lineNum") != LayoutUtils.getProperty(openBrackets.get(labelPart).get, "lineNum") &&
         bracketOutsideInsideApply.contains(labelPart));
  }

  def startNewBracket(labelPart:String, token:Span)
  {

    //if at some point, there is an open bracket of the same label, close it at the last visited point
    if(openBrackets.contains(labelPart))
    {
      val elem = LayoutUtils.getProperty(startBrackets.get(labelPart).get, "divElement")
        .asInstanceOf[Element].getChildren.asScala.find(x => x.getAttribute("type") != null && x.getAttribute("type").getValue == labelPart).get

      updateBracketSpan(openBrackets.get(labelPart).get, false /*false means that it has to append instead of prepend the bracket tag*/
        , labelPart , "bracket-end", elem.getAttribute("id-n").getIntValue)
    }
    //now, adds the begin bracket to the current span (prepending)
    updateBracketSpan(token,true,labelPart, "bracket-begin", NewHtmlTokenization.lastBracketId)
    startBrackets.put(labelPart, token)
    NewHtmlTokenization.lastBracketId += 1
  }

  def updateBracketSpan (token:Span, prepend:Boolean, labelPart:String, bracketType:String, idN:Integer)
  {

    val attrs:mutable.MutableList[Attribute] = collection.mutable.MutableList[Attribute]()
    if(idN!= null) {
      attrs.+=(new Attribute("id-n", idN.toString, Namespace.NO_NAMESPACE))
    }
    else
    {
      attrs.+=(new Attribute("id-n", NewHtmlTokenization.lastBracketId.toString, Namespace.NO_NAMESPACE))
    }
    attrs.+=(new Attribute("type", labelPart, Namespace.NO_NAMESPACE))
    val childElem:Element = new Element(bracketType,"")
    childElem.setAttributes(attrs.asJavaCollection)
    val elem:Element = LayoutUtils.getProperty(token, "divElement").asInstanceOf[Element]

    if(prepend)
    {
      val textIdx:Int = LayoutUtils.getProperty(token,"startOffset").toString.toInt

      addChildElem(textIdx, childElem, elem)

    }
    else
    {
      val textIdx = LayoutUtils.getProperty(token, "endOffset").toString.toInt

      addChildElem(textIdx, childElem, elem)

    }

  }

  def addChildElem(textIdx:Int, childElem:Element, elem:Element)
  {
    val content = elem.getContent
    var currIndex = 0;
    var currIndexContent = 0;
//    var hasToBreak:Boolean = false;
    for(
      currContent <- content.asScala
//      if(!hasToBreak)
    )
    {
      currIndexContent += 1
      if(currContent.isInstanceOf[org.jdom2.Text])
      {
        val currText = currContent.getValue
        if(textIdx>= currIndex && textIdx<=(currIndex + currContent.getValue.length))
        {
          //then we are good to split
          val (strLeft, strRight) = currText.splitAt(textIdx - currIndex)
          var elemLeft:org.jdom2.Content = null;
          var elemRight:org.jdom2.Content = null;
          if(strLeft.length>0)
          {
            elemLeft = new org.jdom2.Text(strLeft)
          }

          if(strRight.length>0)
          {
            elemRight = new org.jdom2.Text(strRight)
          }
          elem.setContent(


            (((elem.getContent().asScala.slice(0,currIndexContent-1)

              ++

              {if(elemLeft!=null)
              {
                List(elemLeft)
              }
              else
              {
                List()
              }}

              :+ childElem)

              ++

              {if(elemRight!=null)
              {
                List(elemRight)
              }
              else
              {
                List()
              }}

              )
              ++

              elem.getContent().asScala.slice(currIndexContent, elem.getContent().size())


              )

              .asJava
/*          ((elem.getContent().asScala.slice(0,currIndexContent)
                  :+ elemLeft
                  :+ childElem
                  :+ elemRight)
                  ++ elem.getContent().asScala.slice(currIndexContent, elem.getContent().size())
              )

            .asJava*/ )

//          println("elem after modification: " + elem )
//          hasToBreak = true
          return
        }
      }
    }
  }


  def updateSvgTags(input:NewHtmlTokenizationSvg, tokenLabels:Sequence): Unit =
  {
    //just saves the labels to be used when tagging in ScalaTaggerSvg
    val inputTokens = input.tokens
    println(inputTokens)
  }

  def updateHtmlTags(input:NewHtmlTokenization, tokenLabels: Sequence, parentName: String): Unit /*Element*/ =
  {
//    println(input)
    var i: Int = 0

    var adjacentOrTheSame:Boolean = false;
    var previousElement:Element = null;
    var previousLabels:Array[String] = "".split("")
    while(i < input.size){
      var span: Span = input.get(i).asInstanceOf[Span]
//      input.getLineSpans.find(x=> x.equals(span))

//      println(span.getText + "--- span start index: " + span.getStartIdx + "    span end index: " + span.getEndIdx)

      val testRes = input.getSubspanTokenization(span.getStartIdx, span.getEndIdx)
      if(tokenLabels!=null)
      {
        //println(LayoutUtils.getProperty(span,"isHeaderFooterLine"))
//        if(LayoutUtils.isPropertySet(span,"isHeaderFooterLine"))
//        {
//          println("header/footer in true: " + span.getText)
//        }
        /*things to take into account:
        * 1- whenever the div is notext, and the next is not ...-begin, then put bracket-outside before notext and
        * bracket-inside just after. */
//        var labels: String = tokenLabels.get(i).toString
//        println (span.getText + ": " + labels)

        var labels: String = tokenLabels.get(i).toString
        labels = labels.replaceAll("author-begin", "authors:^author")
        labels = labels.replaceAll("author-inside", "authors:author")
        labels = labels.replaceAll("text-begin", "^text")
        labels = labels.replaceAll("text-inside", "text")
        labels = labels.replaceAll("section-marker-begin", "^section-marker")
        labels = labels.replaceAll("section-marker-inside", "section-marker")
        labels = labels.replaceAll("table-marker-begin", "^table-marker")
        labels = labels.replaceAll("table-marker-inside", "table-marker")
        labels = labels.replaceAll("figure-marker-begin", "^figure-marker")
        labels = labels.replaceAll("figure-marker-inside", "figure-marker")
        labels = labels.replaceAll("paragraph-begin", "^paragraph-marker")
        labels = labels.replaceAll("paragraph-inside", "paragraph-marker")
        labels = labels.replaceAll("B-", "^")
        labels = labels.replaceAll("I-", "")


        val labelParts: Array[String] = labels.split("[:|]")

        updateToken(span,labelParts,previousLabels)

        previousLabels = labelParts
        //.map(x=> x.replaceFirst("^\\^", "").substring(0,x.indexOf("--_--"))
        //)

/*        private def updateToken(token:Span, labelParts: Array[String],
                                previousLabels: Array[String])*/
      }
      else
      {
        //no sequence labels involved, only work with parent, checking the siblings and that's it
        print(span.getText + " ")

        val currentElement:Element = getElement(span);
        if((previousElement != null && previousElement.getParent.eq(currentElement.getParent) &&
          (previousElement.getParent.indexOf(previousElement) != currentElement.getParent.indexOf(currentElement) - 2 &&
            previousElement.getParent.indexOf(previousElement) != currentElement.getParent.indexOf(currentElement))) ||
          (previousElement!=null && !previousElement.getParent.eq(currentElement.getParent)))
        {
            updateBracketsSegments(currentElement, true, /*"reference"*/ parentName , "bracket-inside")
            updateBracketsSegments(previousElement, false, /*"reference"*/ parentName , "bracket-outside")
        }
        else if(previousElement==null)
        {
          updateBracketsSegments(currentElement, true, /*"reference"*/ parentName , "bracket-begin")
        }

        if(!adjacentOrTheSame)
        {
//          updateBracketsSegments(currentElement, true, "reference" , "bracket-inside")
        }

        if(i+1==input.size)
        {
          updateBracketsSegments(currentElement, false, /*"reference"*/ parentName , "bracket-end")
        }
      //  println ("the current element content: " + currentElement.getText)
        previousElement = currentElement
      }
      i+=1
    }
    NewHtmlTokenization.lastBracketId+=1
    println("")
    println("--------------------------------------------------")
    println("")
  }

  def updateHtmlTagsSvd(input:NewHtmlTokenizationSvg, tokenLabels: Sequence, parentName: String): Unit /*Element*/ =
  {
    //    println(input)
    var i: Int = 0

    var adjacentOrTheSame:Boolean = false;
    var previousElement:Element = null;
    var previousLabels:Array[String] = "".split("")
    while(i < input.size){
      var span: Span = input.get(i).asInstanceOf[Span]
      //      input.getLineSpans.find(x=> x.equals(span))

      //      println(span.getText + "--- span start index: " + span.getStartIdx + "    span end index: " + span.getEndIdx)

      val testRes = input.getSubspanTokenization(span.getStartIdx, span.getEndIdx)
      if(tokenLabels!=null)
      {
        //println(LayoutUtils.getProperty(span,"isHeaderFooterLine"))
        //        if(LayoutUtils.isPropertySet(span,"isHeaderFooterLine"))
        //        {
        //          println("header/footer in true: " + span.getText)
        //        }
        /*things to take into account:
        * 1- whenever the div is notext, and the next is not ...-begin, then put bracket-outside before notext and
        * bracket-inside just after. */
        //        var labels: String = tokenLabels.get(i).toString
        //        println (span.getText + ": " + labels)

        var labels: String = tokenLabels.get(i).toString
        labels = labels.replaceAll("author-begin", "authors:^author")
        labels = labels.replaceAll("author-inside", "authors:author")
        labels = labels.replaceAll("text-begin", "^text")
        labels = labels.replaceAll("text-inside", "text")
        labels = labels.replaceAll("section-marker-begin", "^section-marker")
        labels = labels.replaceAll("section-marker-inside", "section-marker")
        labels = labels.replaceAll("table-marker-begin", "^table-marker")
        labels = labels.replaceAll("table-marker-inside", "table-marker")
        labels = labels.replaceAll("figure-marker-begin", "^figure-marker")
        labels = labels.replaceAll("figure-marker-inside", "figure-marker")
        labels = labels.replaceAll("paragraph-begin", "^paragraph-marker")
        labels = labels.replaceAll("paragraph-inside", "paragraph-marker")
        labels = labels.replaceAll("B-", "^")
        labels = labels.replaceAll("I-", "")


        val labelParts: Array[String] = labels.split("[:|]")

        updateToken(span,labelParts,previousLabels)

        previousLabels = labelParts
        //.map(x=> x.replaceFirst("^\\^", "").substring(0,x.indexOf("--_--"))
        //)

        /*        private def updateToken(token:Span, labelParts: Array[String],
                                        previousLabels: Array[String])*/
      }
      else
      {
        //no sequence labels involved, only work with parent, checking the siblings and that's it
        print(span.getText + " ")

        val currentElement:Element = getElement(span);
        if((previousElement != null && previousElement.getParent.eq(currentElement.getParent) &&
          (previousElement.getParent.indexOf(previousElement) != currentElement.getParent.indexOf(currentElement) - 2 &&
            previousElement.getParent.indexOf(previousElement) != currentElement.getParent.indexOf(currentElement))) ||
          (previousElement!=null && !previousElement.getParent.eq(currentElement.getParent)))
        {
          updateBracketsSegments(currentElement, true, /*"reference"*/ parentName , "bracket-inside")
          updateBracketsSegments(previousElement, false, /*"reference"*/ parentName , "bracket-outside")
        }
        else if(previousElement==null)
        {
          updateBracketsSegments(currentElement, true, /*"reference"*/ parentName , "bracket-begin")
        }

        if(!adjacentOrTheSame)
        {
          //          updateBracketsSegments(currentElement, true, "reference" , "bracket-inside")
        }

        if(i+1==input.size)
        {
          updateBracketsSegments(currentElement, false, /*"reference"*/ parentName , "bracket-end")
        }
        //  println ("the current element content: " + currentElement.getText)
        previousElement = currentElement
      }
      i+=1
    }
    NewHtmlTokenization.lastBracketId+=1
    println("")
    println("--------------------------------------------------")
    println("")
  }

  def getElement(span:Span):Element = {
    var element:Element = null;
    if(span.isInstanceOf[StringSpan])
    {
      element = span.asInstanceOf[StringSpan].getProperty("divElement").asInstanceOf[Element]
    }
    else
    {
      //if not it is compositeSpan
      element = span.asInstanceOf[CompositeSpan].getProperty("divElement").asInstanceOf[Element]
    }
    return element;
  }

  def updateBracketsSegments(elem:Element, prepend:Boolean, tpe:String, elemName:String)
  {
    val attrs:mutable.MutableList[Attribute] = collection.mutable.MutableList[Attribute]()
    attrs.+=(new Attribute("id-n", NewHtmlTokenization.lastBracketId.toString, Namespace.NO_NAMESPACE))
    attrs.+=(new Attribute("type", tpe, Namespace.NO_NAMESPACE))
    val childElem:Element = new Element(elemName,"")
    childElem.setAttributes(attrs.asJavaCollection)

    if(prepend)
    {
      elem.setContent((childElem +: elem.getContent.asScala).asJavaCollection)
    }
    else
    {
      elem.setContent((elem.getContent.asScala :+ childElem).asJavaCollection)
    }
  }
  def updateSegmentation(segments:collection.mutable.Map[String, /*NewHtmlTokenization*/ Any])
  {

    val headerSegment:NewHtmlTokenization = segments.get("headerTokenization").get.asInstanceOf[NewHtmlTokenization]
    updateBracketsSegments(headerSegment.getToken(0).getProperty("divElement").asInstanceOf[Element], true, "header", "bracket-begin")
    updateBracketsSegments(headerSegment.getToken(headerSegment.size-1).getProperty("divElement").asInstanceOf[Element], false, "header", "bracket-end")

    NewHtmlTokenization.lastBracketId+=1

    val bodySegment:NewHtmlTokenization = segments.get("bodyTokenization").get.asInstanceOf[NewHtmlTokenization]
    updateBracketsSegments(bodySegment.getToken(0).getProperty("divElement").asInstanceOf[Element], true, "body", "bracket-begin")
    updateBracketsSegments(bodySegment.getToken(bodySegment.size-1).getProperty("divElement").asInstanceOf[Element], false, "body", "bracket-end")
    NewHtmlTokenization.lastBracketId+=1

    val referenceSegment:NewHtmlTokenization = segments.get("referencesTokenization").get.asInstanceOf[NewHtmlTokenization]
    updateBracketsSegments(referenceSegment.getToken(0).getProperty("divElement").asInstanceOf[Element], true, "references", "bracket-begin")
    updateBracketsSegments(referenceSegment.getToken(referenceSegment.size-1).getProperty("divElement").asInstanceOf[Element], false, "references", "bracket-end")
    NewHtmlTokenization.lastBracketId+=1

  }


  /**
   *
   * @param input
   * @param tokenLabels
   * @param parentName
   * @return jdom fragment hierarchically representing the labelled tokens
   */
  def toXmlElement(input: NewHtmlTokenization, tokenLabels: Sequence, parentName: String): Element = {
    val rootElement: Element = new Element(parentName)
    var columns: List[BoxCoordinates] = null
    var currentColumn: Int = 1
    if (parentName == "reference") {
      columns = getColumnData(input.getLineSpans.toList ).toList
    }
    val page: Int = -1

      var i: Int = 0
      while (i < input.size) {
        {
          val span: Span = input.get(i).asInstanceOf[Span]

          var labels: String = tokenLabels.get(i).toString
          labels = labels.replaceAll("author-begin", "authors:^author")
          labels = labels.replaceAll("author-inside", "authors:author")
          labels = labels.replaceAll("text-begin", "^text")
          labels = labels.replaceAll("text-inside", "text")
          labels = labels.replaceAll("section-marker-begin", "^section-marker")
          labels = labels.replaceAll("section-marker-inside", "section-marker")
          labels = labels.replaceAll("table-marker-begin", "^table-marker")
          labels = labels.replaceAll("table-marker-inside", "table-marker")
          labels = labels.replaceAll("figure-marker-begin", "^figure-marker")
          labels = labels.replaceAll("figure-marker-inside", "figure-marker")
          labels = labels.replaceAll("paragraph-begin", "^paragraph-marker")
          labels = labels.replaceAll("paragraph-inside", "paragraph-marker")
          val labelParts: Array[String] = labels.split("[:|]")
          val bcord: BoxCoordinates = getSpanBoxCoordinates(span)
          if (parentName == "reference") {
            currentColumn = getCurrentColumn(columns, span)
          }
          insertTokenPosition(rootElement, getSpanText(span), labelParts, bcord, currentColumn)
        }
        ({
          i += 1; i - 1
        })
      }

    return rootElement
  }

  private[extraction] def getCurrentColumn(columns: List[BoxCoordinates], span: Span): Int = {
    val llxSpan: Double = (span.asInstanceOf[PropertyHolder]).getNumericProperty("llx")
    val urxSpan: Double = (span.asInstanceOf[PropertyHolder]).getNumericProperty("urx")
    var col: Int = 1
    var estimateCol: Int = 1
    val estimateDistance: Double = 10000
    import scala.collection.JavaConversions._
    for (bc <- columns) {
      if (bc.getLlx <= llxSpan && urxSpan <= bc.getUrx) {
        return col
      }
      else {
        if (bc.getLlx <= llxSpan && bc.getUrx >= llxSpan) {
          estimateCol = col
        }
        else if (bc.getLlx <= urxSpan && bc.getUrx >= urxSpan) {
          estimateCol = col
        }
        else if (bc.getLlx >= llxSpan && bc.getUrx <= urxSpan) {
          estimateCol = col
        }
        else if (bc.getUrx <= urxSpan && bc.getUrx <= llxSpan) {
          estimateCol = col
        }
      }
      col += 1
    }
    return estimateCol
  }

  private[extraction] def getColumnData(lineSpan: List[Span]): collection.mutable.MutableList[BoxCoordinates] = {
    val retVal: collection.mutable.MutableList[BoxCoordinates] = collection.mutable.MutableList() //new ArrayList[BoxCoordinates]
    var currCol: Int = 0
    import scala.collection.JavaConversions._
    for (span <- lineSpan) {
      if (span.isInstanceOf[CompositeSpan]) {
        val llx: Double = (span.asInstanceOf[CompositeSpan]).getProperty("llx").toString.toDouble
        val urx: Double = (span.asInstanceOf[CompositeSpan]).getProperty("urx").toString.toDouble //Double.valueOf((span.asInstanceOf[CompositeSpan]).getProperty("urx").toString)
        if (retVal.size <= currCol) {
          retVal.add(new BoxCoordinates(-1, urx, -1, llx, -1))
        }
        else {
          val bc: BoxCoordinates = retVal(0) //retVal.get(currCol)
          if (bc.getUrx < llx) {
            currCol = retVal.size
            retVal.add(new BoxCoordinates(-1, urx, -1, llx, -1))
          }
          else if (urx < bc.getLlx) {
            retVal.add(0, new BoxCoordinates(-1, urx, -1, llx, -1))
          }
          else {
            if (bc.getLlx > llx) {
              bc.setLlx(llx)
            }
            if (bc.getUrx < urx) {
              bc.setUrx(urx)
            }
          }
        }
      }
    }
    if (retVal.size > 1) {
      var prevBc: BoxCoordinates = retVal(0) //retVal.get(0)
      import scala.collection.JavaConversions._
      for (bc <- retVal.subList(1, retVal.size)) {
        if (bc.getLlx < prevBc.getUrx) {
          bc.setLlx(prevBc.getUrx + 1)
        }
        prevBc = bc
      }
    }
    return retVal
  }

  def toXmlElement(tokenStrings: Array[String], tokenLabels: Sequence, parentName: String): Element = {
    val rootElement: Element = new Element(parentName)

      var i: Int = 0
      while (i < tokenStrings.length) {
        {
          val span: String = tokenStrings(i)
          var labels: String = tokenLabels.get(i).toString
          labels = labels.replaceAll("B-", "^")
          labels = labels.replaceAll("I-", "")
          val labelParts: Array[String] = labels.split("[:|]")
          insertToken(rootElement, span + " ", labelParts)
        }
        ({
          i += 1; i - 1
        })
      }

    return rootElement
  }

  private def getSpanText(span: Span): String = {
    val numericProperty: Double = (span.asInstanceOf[PropertyHolder]).getNumericProperty("trailing-ws-1") + 1
    val stringBuffer: StringBuffer = new StringBuffer
    stringBuffer.append(span.getText)
//    {
      var i: Int = 0
      while (i < numericProperty) {
        {
          stringBuffer.append(" ")
        }
        ({
          i += 1; i - 1
        })
      }
//    }
    return stringBuffer.toString
  }

  private def getSpanBoxCoordinates(span: Span): BoxCoordinates = {
    val boxCoordinates: BoxCoordinates = new BoxCoordinates
    boxCoordinates.setLlx((span.asInstanceOf[PropertyHolder]).getNumericProperty("llx"))
    boxCoordinates.setLly((span.asInstanceOf[PropertyHolder]).getNumericProperty("lly"))
    boxCoordinates.setUrx((span.asInstanceOf[PropertyHolder]).getNumericProperty("urx"))
    boxCoordinates.setUry((span.asInstanceOf[PropertyHolder]).getNumericProperty("ury"))
    boxCoordinates.setPageNum((span.asInstanceOf[PropertyHolder]).getNumericProperty("pageNum").asInstanceOf[Int])
    return boxCoordinates
  }

  /**
   * insert a token into a jdom tree under the element specified by labelParts
   * @param parent
   * @param span
   * @param labelParts
   */
  private def insertToken(parent: Element, span: String, labelParts: Array[String]) {
    if (labelParts.length > 0) {
      var labelPart: String = labelParts(0)
      var child: Element = null
      if ((({
        child = lastChild(parent); child
      })) == null || labelPart.startsWith("^") || !(labelPart == child.getName)) {
        labelPart = labelPart.replaceFirst("^\\^", "")
        child = new Element(labelPart)
        parent.addContent(child)
      }
      val tails: List[_] =  labelParts.slice(1, labelParts.length).toList //  Arrays.asList(labelParts).subList(1, labelParts.length)
      val labelTail: Array[String] = tails.toArray.asInstanceOf[Array[String]] //tails.toArray(new Array[String](tails.size)).asInstanceOf[Array[String]]
      insertToken(child, span, labelTail)
    }
    else {
      parent.addContent(span)
    }
  }

  def getExtraAttrs(label:String):collection.mutable.Map[String,String] =
  {
    val retMap:collection.mutable.Map[String,String] = collection.mutable.Map[String,String]()
    var extraAttrs: Array[String] = null
    if (label.indexOf("--_--") > -1) {
      extraAttrs = label.substring(label.indexOf("--_--")).replace("--_--", "").split(";")
    }
    if (extraAttrs != null && extraAttrs.length > 0) {
      for (currAttr <- extraAttrs) {
        val attr: String = currAttr.substring(0, currAttr.indexOf("="))
        val value: String = currAttr.substring(currAttr.indexOf("=") + 1, currAttr.length)
        retMap.put(attr,value)
      }
    }

    return retMap;
  }
  /**
   * kzaporojets
   * insert a token into a jdom tree under the element specified by labelParts, includes the position of the element
   * @param parent
   * @param span
   * @param labelParts
   */
  private def insertTokenPosition(parent: Element, span: String, labelParts: Array[String], positionSpan: BoxCoordinates, currentColumn: Int) {
    adjustPosition(parent, positionSpan, currentColumn)
    if (labelParts.length > 0) {
      var labelPart: String = labelParts(0)
      var child: Element = null
      if ((({
        child = lastChild(parent); child
      })) == null || labelPart.startsWith("^") || !(labelPart == child.getName)) {
        labelPart = labelPart.replaceFirst("^\\^", "")
        var extraAttrs: Array[String] = null
        if (labelPart.indexOf("--_--") > -1) {
          extraAttrs = labelPart.substring(labelPart.indexOf("--_--")).replace("--_--", "").split(";")
          labelPart = labelPart.replace(labelPart.substring(labelPart.indexOf("--_--")), "")
        }
        child = new Element(labelPart)
        if (extraAttrs != null && extraAttrs.length > 0) {
          for (currAttr <- extraAttrs) {
            val attr: String = currAttr.substring(0, currAttr.indexOf("="))
            val value: String = currAttr.substring(currAttr.indexOf("=") + 1, currAttr.length)
            child.setAttribute(attr, value)
          }
        }
        parent.addContent(child)
      }
      val tails: List[_] = labelParts.slice(1, labelParts.length).toList //Arrays.asList(labelParts).subList(1, labelParts.length)
      val labelTail: Array[String] = tails.toArray.asInstanceOf[Array[String]] //tails.toArray(new Array[String](tails.size)).asInstanceOf[Array[String]]
      insertTokenPosition(child, span, labelTail, positionSpan, currentColumn)
    }
    else {
      parent.addContent(span)
    }
  }

  private def adjustPosition(elem: Element, pos: BoxCoordinates, currentColumn: Int) {
    try {
      val initCol: Int = 1
      val llxAttr: String = "llx"
      val llyAttr: String = "lly"
      val urxAttr: String = "urx"
      val uryAttr: String = "ury"
      if (elem.getAttribute(llxAttr) != null && (elem.getName == "reference")) {
        if (Math.abs(elem.getAttribute(llyAttr).getDoubleValue - pos.getLly) > 400 && Math.abs(elem.getAttribute(uryAttr).getDoubleValue - pos.getUry) > 400) {
          return
        }
      }
      if (elem.getAttribute(llxAttr) == null || elem.getAttribute(llxAttr).getDoubleValue > pos.getLlx) {
        elem.setAttribute(llxAttr, String.valueOf(pos.getLlx))
      }
      if (elem.getAttribute(llyAttr) == null || elem.getAttribute(llyAttr).getDoubleValue > pos.getLly) {
        elem.setAttribute(llyAttr, String.valueOf(pos.getLly))
      }
      if (elem.getAttribute(urxAttr) == null || elem.getAttribute(urxAttr).getDoubleValue < pos.getUrx) {
        elem.setAttribute(urxAttr, String.valueOf(pos.getUrx))
      }
      if (elem.getAttribute(uryAttr) == null || elem.getAttribute(uryAttr).getDoubleValue < pos.getUry) {
        elem.setAttribute(uryAttr, String.valueOf(pos.getUry))
      }
      if (elem.getAttribute("pageNum") == null) {
        elem.setAttribute("pageNum", String.valueOf(pos.getPageNum))
      }
    }
    catch {
      case ex: DataConversionException => {
        log.error(ex.getClass.getName + ": " + ex.getMessage)
      }
    }
  }

  private def lastChild(parent: Element): Element = {
    val children: List[Element] = parent.getChildren.asScala.toList
    return if (children.isEmpty) null else children(children.size - 1).asInstanceOf[Element] //children.get(children.size - 1).asInstanceOf[Element]
  }
}

