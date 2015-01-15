package org.rexo.extraction

/**
 * Created by klimzaporojets on 9/26/14.
 */

class BoxCoordinates {
  private var ury: Double = .0
  private var urx: Double = .0
  private var lly: Double = .0
  private var llx: Double = .0
  private var pageNum: Int = 0
  def this(ury: Double, urx: Double, lly: Double, llx: Double, pageNum: Int) {
    this()
    this.ury = ury
    this.urx = urx
    this.lly = lly
    this.llx = llx
    this.pageNum = pageNum
  }

  def getPageNum: Int = {
    return pageNum
  }

  def setPageNum(pageNum: Int) {
    this.pageNum = pageNum
  }

  def getUry: Double = {
    return ury
  }

  def setUry(ury: Double) {
    this.ury = ury
  }

  def getUrx: Double = {
    return urx
  }

  def setUrx(urx: Double) {
    this.urx = urx
  }

  def getLly: Double = {
    return lly
  }

  def setLly(lly: Double) {
    this.lly = lly
  }

  def getLlx: Double = {
    return llx
  }

  def setLlx(llx: Double) {
    this.llx = llx
  }
}
