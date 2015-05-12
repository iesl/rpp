package edu.umass.cs.rexo.ghuang.segmentation

import org.rexo.base.{Instance, Pipe}
import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
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

    blockId + "_" +  pageNum.asInstanceOf[Double].intValue
  }

  def pipe(carrier: Instance): Instance = {
    val htmlTokenization: NewHtmlTokenizationSvg = carrier.getData.asInstanceOf[NewHtmlTokenizationSvg]
    val lineInfos: collection.mutable.MutableList[LineInfo] = collection.mutable.MutableList[LineInfo]() // ArrayList[_] = new ArrayList[_]
    var prevLineNum: Int = -1
    var lineInfo: LineInfo = null
    var lineText: StringBuffer = new StringBuffer

    var ti: Int = 0
    while (ti < htmlTokenization.size) {
      {
        val token: Token = htmlTokenization.getToken(ti)


        val tokLineNum: Int = token.getNumericProperty("lineNum").asInstanceOf[Int]
        if (tokLineNum == 0) {
          //continue //todo: continue is not supported
        }
        else if (tokLineNum != prevLineNum) {
          prevLineNum = tokLineNum
          if (lineText.length > 0) {
            lineInfo.text = lineText.toString
            lineInfos.+=(lineInfo)  //.add(lineInfo)
          }
          lineInfo = new LineInfo
          lineText = new StringBuffer(token.getText + " ")
          val divElement:scala.Tuple2[Integer,Any] = LayoutUtils.getProperty(token, "divElement").asInstanceOf[scala.Tuple2[Integer, Any]]
          lineInfo.blockId = divElement._1.toInt

          lineInfo.page = token.getNumericProperty("pageNum").asInstanceOf[Int]
          lineInfo.llx = token.getNumericProperty("llx").asInstanceOf[Int]
          lineInfo.lly = token.getNumericProperty("lly").asInstanceOf[Int]
          lineInfo.urx = token.getNumericProperty("urx").asInstanceOf[Int]
          lineInfo.ury = token.getNumericProperty("ury").asInstanceOf[Int]
          lineInfo.font = token.getProperty("fontname").asInstanceOf[String]
        }
        else {
          lineText.append(token.getText + " ")
          if (token.getNumericProperty("firstInTextBox") > 0) {
            lineInfo.multibox = true
            lineInfo.llx = Math.min(lineInfo.llx, token.getNumericProperty("llx")).asInstanceOf[Int]
            lineInfo.lly = Math.min(lineInfo.lly, token.getNumericProperty("lly")).asInstanceOf[Int]
            lineInfo.urx = Math.max(lineInfo.urx, token.getNumericProperty("urx")).asInstanceOf[Int]
            lineInfo.ury = Math.max(lineInfo.ury, token.getNumericProperty("ury")).asInstanceOf[Int]
          }
        }
      }
      ({
        ti += 1; ti - 1
      })
    }

    assert((lineInfo != null))
    if (lineText.toString != null) {
      lineInfo.text = lineText.toString
      lineInfos.+=(lineInfo)
    }
    val newData: Array[LineInfo] = new Array[LineInfo](lineInfos.size)

    var i: Int = 0
    while (i < lineInfos.size) {
      newData(i) = lineInfos.get(i).get.asInstanceOf[LineInfo]
      ({
        i += 1; i - 1
      })
    }

    carrier.setData(newData)
    return carrier
  }
}
