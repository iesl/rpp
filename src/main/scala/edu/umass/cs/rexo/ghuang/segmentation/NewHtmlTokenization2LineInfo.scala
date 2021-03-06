package edu.umass.cs.rexo.ghuang.segmentation

import org.rexo.base.{Instance, Pipe}
import org.rexo.extraction.NewHtmlTokenization
import org.rexo.extra.types.Token

/**
 * Created by klimzaporojets on 10/2/14.
 */
/**
 * Convert a NewHtmlTokenization in the data field of an Instance to LineInfo[]
 *
 * @author ghuang
 *
 */
object NewHtmlTokenization2LineInfo {
  private final val serialVersionUID: Long = 1L
}

//TODO: here is where information about div elements should be added from NewHtmlTokenization
class NewHtmlTokenization2LineInfo extends Pipe with Serializable {
  def pipe(carrier: Instance): Instance = {
    val htmlTokenization: NewHtmlTokenization = carrier.getData.asInstanceOf[NewHtmlTokenization]
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
