package org.rexo.span

import org.rexo.extra.extract.{StringSpan, Span}
import org.rexo.extra.types.PropertyHolder
import org.rexo.extra.utils.PropertyList

/**
 * Author: saunders Created Nov 10, 2005 Copyright (C) Univ. of Massachusetts Amherst, Computer Science Dept.
 */
object CompositeSpan {
  def createSpan(document: CharSequence): CompositeSpan = {
    return new CompositeSpan(document)
  }
}

class CompositeSpan extends Span with PropertyHolder {
  private def this(document: CharSequence) {
    this()
    _document = document
  }

  def appendSpan(s: Span) {
    _spans.+=(s)
  }

  def getSpans: collection.mutable.MutableList[Any] = {
    return _spans
  }

  def getText: String = {
    val rangeText: StringBuffer = new StringBuffer

      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).get.asInstanceOf[StringSpan]
          val isVisible: Boolean = s.getNumericProperty("invisible") == 0
          if (isVisible) {
            rangeText.append(s.getText)
            val numericProperty: Double = s.getNumericProperty("trailing-ws-1") + 1

              var j: Int = 0
              while (j < numericProperty) {
                {
                  rangeText.append(" ")
                }
                  j += 1;
              }

          }
        }
          i += 1;
      }

    return rangeText.toString
  }

  def intersection(r: Span): Span = {
    return null
  }

  def getDocument: AnyRef = {
    return _document
  }

  def intersects(r: Span): Boolean = {
    return false
  }

  def isSubspan(r: Span): Boolean = {
    return false
  }

  def getStartIdx: Int = {
    if (_spans.isEmpty) {
      return 0
    }
    val s: Span = _spans.get(0).get.asInstanceOf[Span]
    return s.getStartIdx
  }

  def getEndIdx: Int = {
    if (_spans.isEmpty) {
      return 0
    }
    val s: Span = _spans.get(_spans.size - 1).get.asInstanceOf[Span]
    return s.getEndIdx
  }

  override def toString: String = {
    return "[" + _spans.size + "]: " + getText
  }

  def setProperty(key: String, value: Any) {
    throw new UnsupportedOperationException("")
  }

  def getProperty(key: String): Any = {
    {
      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).get.asInstanceOf[StringSpan]
          if (s.hasProperty(key)) {
            return s.getProperty(key)
          }
        }
        ({
          i += 1;
        })
      }
    }
    return null
  }

  def setNumericProperty(key: String, value: Double) {
    throw new UnsupportedOperationException("")
  }

  def getNumericProperty(key: String): Double = {
    {
      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).get.asInstanceOf[StringSpan]
          if (s.hasProperty(key)) {
            return s.getNumericProperty(key)
          }
        }
        i += 1;
      }
    }
    return 0.0
  }

  def getProperties: PropertyList = {
    throw new UnsupportedOperationException("")
  }

  def setProperties(newProperties: PropertyList) {
    throw new UnsupportedOperationException("")
  }

  def hasProperty(key: String): Boolean = {
    {
      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).asInstanceOf[StringSpan]
          if (s.hasProperty(key)) {
            return true
          }
        }
        i += 1;
      }
    }
    return false
  }

  def setFeatureValue(key: String, value: Double) {
    {
      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).get.asInstanceOf[StringSpan]
          s.setFeatureValue(key, value)
        }
        i += 1;
      }
    }
  }

  def getFeatureValue(key: String): Double = {
    if (!_spans.isEmpty) {
      val s: StringSpan = _spans.get(0).get.asInstanceOf[StringSpan]
      return s.getFeatureValue(key)
    }
    return 0.0
  }

  def getFeatures: PropertyList = {
    if (!_spans.isEmpty) {
      val s: StringSpan = _spans.get(0).get.asInstanceOf[StringSpan]
      return s.getFeatures
    }
    return null
  }

  def setFeatures(pl: PropertyList) {
    {
      var i: Int = 0
      while (i < _spans.size) {
        {
          val s: StringSpan = _spans.get(i).get.asInstanceOf[StringSpan]
          s.setFeatures(pl)
        }
        i += 1;
      }
    }
  }

  def getBeginTokenIndex: Int = {
    if (_spans.isEmpty) {
      return 0
    }
    val s: StringSpan = _spans.get(0).get.asInstanceOf[StringSpan]
    return 0
  }

  def getEndTokenIndex: Int = {
    if (_spans.isEmpty) {
      return 0
    }
    val s: StringSpan = _spans.get(0).get.asInstanceOf[StringSpan]
    return 0
  }

  private var _spans: collection.mutable.MutableList[Any] = new collection.mutable.MutableList[Any]()
  private var _document: CharSequence = null
}

