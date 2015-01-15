package org.rexo.extra.types

import org.rexo.extra.utils.PropertyList

/**
 * Created by klimzaporojets on 9/26/14.
 */
/**
 * Created by klimzaporojets on 9/17/14.
 */
abstract trait PropertyHolder {
  def setProperty(key: String, value: Any)

  def getProperty(key: String): Any

  def setNumericProperty(key: String, value: Double)

  def getNumericProperty(key: String): Double

  def getProperties: PropertyList

  def setProperties(newProperties: PropertyList)

  def hasProperty(key: String): Boolean

  def setFeatureValue(key: String, value: Double)

  def getFeatureValue(key: String): Double

  def getFeatures: PropertyList

  def setFeatures(pl: PropertyList)
}
