package edu.umass.cs.rexo.ghuang.segmentation

import java.util.HashSet

/**
 * Created by klimzaporojets on 10/1/14.
 */
/**
 * Intermediate representation for both labeled segmentation data and pstotext data.
 *
 * @author adingle, ghuang
 *
 */
class LineInfo {


  var text: String = null
  var page: Int = 0
  var llx: Int = 0
  var lly: Int = 0
  var urx: Int = 0
  var ury: Int = 0
  var blockId: Int = 0
  var font: String = ""
  var multibox: Boolean = false
  //the label such as I-Biblio, B-Biblio,.....
  var label:String = ""

  private[segmentation] var newRef: Boolean = false
  private[segmentation] var trueLabel: String = null
  var presentFeatures: collection.mutable.Set[String] = collection.mutable.Set[String]()
  private[segmentation] var textTokens: Array[String] = null

  def this(line: String) {
    this()
    val lineT = new StringBuffer(line).reverse.toString
    val parts: Array[String] = lineT.split("@", 2)
    assert((parts.length == 2))
    parts(0) = new StringBuffer(parts(0)).reverse.toString
    parts(1) = new StringBuffer(parts(1)).reverse.toString
    val subParts: Array[String] = parts(0).split(",")
    this.page = Integer.parseInt(subParts(0))
    this.llx = Integer.parseInt(subParts(1))
    this.lly = Integer.parseInt(subParts(2))
    this.urx = Integer.parseInt(subParts(3))
    this.ury = Integer.parseInt(subParts(4))
    this.font = subParts(5).toString
    this.multibox = (subParts(6) == "true")
    this.newRef = (subParts(7) == "true")
    this.trueLabel = subParts(8)
    this.text = parts(1)
  }

}
