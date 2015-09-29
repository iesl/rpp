package edu.umass.cs.rexo.ghuang.segmentation

import org.rexo.base.{Instance, Pipe}
import org.rexo.extraction.NewHtmlTokenizationSvg
import org.rexo.extra.types.Token
import org.rexo.extra.extract.Span
import org.rexo.span.CompositeSpan
import edu.umass.cs.rexo.ghuang.segmentation.utils.LayoutUtils

/**
 * Created by klimzaporojets on 11/24/14.
 */
object NewHtmlTokenization2LineInfoSvg {
  private final val serialVersionUID: Long = 1L
}

//TODO: here is where information about div elements should be added from NewHtmlTokenization
class NewHtmlTokenization2LineInfoSvg extends Pipe with Serializable {

  def getBlockId(elem:Span):String = {
    val realElem:Span = elem match {
      case e: CompositeSpan if e.getSpans.size > 0 =>
        e.getSpans(0).asInstanceOf[Span]
      case _ => elem
    }

    val blockId = LayoutUtils.getProperty(realElem.asInstanceOf[Span], "divElement") match {
      case null => -1
      case (x, y) => x.toString.toInt
    }

    val pageNum =  LayoutUtils.getProperty(realElem.asInstanceOf[Span], "pageNum")

    blockId + "_" +  pageNum.asInstanceOf[Int]
  }

  def pipe(carrier: Instance): Instance = {
    val htmlTokenization = carrier.getData.asInstanceOf[NewHtmlTokenizationSvg]
    val lineInfos = collection.mutable.MutableList[LineInfo]()
    var prevLineNum: Int = -1
    var lineInfo: LineInfo = null
    var lineText: StringBuffer = new StringBuffer
    var ti: Int = 0
    while (ti < htmlTokenization.size) {
      val token: Token = htmlTokenization.getToken(ti)
      val tokLineNum: Int = token.getNumericProperty("lineNum").asInstanceOf[Int]
      if (tokLineNum == 0) {
        //continue //todo: continue is not supported
      }
      else if (tokLineNum != prevLineNum) {
        prevLineNum = tokLineNum
        if (lineText.length > 0) {
          lineInfo.text = lineText.toString
          lineInfos += lineInfo
        }
        lineInfo = new LineInfo
        lineText = new StringBuffer(token.getText + " ")
        val divElement = LayoutUtils.getProperty(token, "divElement").asInstanceOf[(Int, Any)]
        lineInfo.blockId = divElement._1.toInt

        lineInfo.page = token.getNumericProperty("pageNum").toInt
        lineInfo.llx = token.getNumericProperty("llx").toInt
        lineInfo.lly = token.getNumericProperty("lly").toInt
        lineInfo.urx = token.getNumericProperty("urx").toInt
        lineInfo.ury = token.getNumericProperty("ury").toInt
        lineInfo.font = token.getProperty("fontname").asInstanceOf[String]
      }
      else {
        lineText.append(token.getText + " ")
        if (token.getNumericProperty("firstInTextBox") > 0) {
          lineInfo.multibox = true
          lineInfo.llx = Math.min(lineInfo.llx, token.getNumericProperty("llx")).toInt
          lineInfo.lly = Math.min(lineInfo.lly, token.getNumericProperty("lly")).toInt
          lineInfo.urx = Math.max(lineInfo.urx, token.getNumericProperty("urx")).toInt
          lineInfo.ury = Math.max(lineInfo.ury, token.getNumericProperty("ury")).toInt
        }
      }
      ti += 1
    }

    assert(lineInfo != null)
    if (lineText.toString != null) {
      lineInfo.text = lineText.toString
      lineInfos += lineInfo
    }
    val newData = new Array[LineInfo](lineInfos.size)

    var i = 0
    while (i < lineInfos.size) {
      newData(i) = lineInfos.get(i).get
      i += 1
    }

    carrier.setData(newData)
    carrier
  }
}
