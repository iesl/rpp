package org.rexo.extra.types

import java.io.{ObjectInputStream, ObjectOutputStream, Serializable}
import org.rexo.extra.utils.PropertyList

/**
 * Created by klimzaporojets on 9/26/14.
 */
object Token {
  private final val serialVersionUID: Long = 1
  private final val CURRENT_SERIAL_VERSION: Int = 0
}

class Token(var text:String) extends Serializable with PropertyHolder {

  text = text.intern()

  def getText: String = {
    return text
  }

  def setText(t: String) {
    text = t.intern
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    sb.append(getText)
    if (features != null) {
      val iter: PropertyList#Iterador = PropertyList.iterator(features)
      while (iter.hasNext) {
        iter.next
        sb.append(" feature(" + iter.getKey + ")=" + iter.getNumericValue)
      }
    }
    if (properties != null) {
      val iter: PropertyList#Iterador = PropertyList.iterator(properties)
      while (iter.hasNext) {
        iter.next
        if (iter.isNumeric) sb.append(" property(" + iter.getKey + ")=" + iter.getNumericValue)
        else sb.append(" property(" + iter.getKey + ")=" + iter.getObjectValue)
      }
    }
    return sb.toString
  }

  def setProperty(key: String, value: Any) {
    properties = PropertyList.add(key, value, properties)
  }

  def setNumericProperty(key: String, value: Double) {
    properties = PropertyList.add(key, value, properties)
  }

  def getProperties: PropertyList = {
    return properties
  }

  def setProperties(newProperties: PropertyList) {
    properties = newProperties
  }

  def getProperty(key: String): Any = {
    return if (properties == null) null else properties.lookupObject(key)
  }

  def getNumericProperty(key: String): Double = {
    return (if (properties == null) 0.0 else properties.lookupNumber(key))
  }

  def hasProperty(key: String): Boolean = {
    return (properties != null && properties.hasProperty(key))
  }

  def setFeatureValue(key: String, value: Double) {
    features = PropertyList.add(key, value, features)
  }

  def getFeatureValue(key: String): Double = {
    return (if (features == null) 0.0 else features.lookupNumber(key))
  }

  def getFeatures: PropertyList = {
    return features
  }

  def setFeatures(pl: PropertyList) {
    features = pl
  }

  private def writeObject(out: ObjectOutputStream) {
    out.writeInt(Token.CURRENT_SERIAL_VERSION)
    out.defaultWriteObject
  }

  private def readObject(in: ObjectInputStream) {
    val version: Int = in.readInt
    in.defaultReadObject
  }

//  private var text: String = null
  private[types] var properties: PropertyList = null
  private[types] var features: PropertyList = null
}
