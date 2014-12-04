package org.rexo.extra.extract

import java.util.{HashSet, HashMap}
import java.io.Serializable
import scala.Serializable

/**
 * Created by klimzaporojets on 9/17/14.
 */
//object PropertyList {
//  def add(key: String, value: AnyRef, rest: PropertyList): PropertyList = {
//    assert((key != null))
//    return new PropertyList.ObjectProperty(key, value, rest)
//  }
//
//  def add(key: String, value: String, rest: PropertyList): PropertyList = {
//    assert((key != null))
//    return new PropertyList.ObjectProperty(key, value, rest)
//  }
//
//  def add(key: String, value: Double, rest: PropertyList): PropertyList = {
//    assert((key != null))
//    return new PropertyList.NumericProperty(key, value, rest)
//  }
//
//  def remove(key: String, rest: PropertyList): PropertyList = {
//    assert((key != null))
//    return new PropertyList.ObjectProperty(key, null, rest)
//  }
//
//  def sumDuplicateKeyValues(pl: PropertyList): PropertyList = {
//    if (!(pl.isInstanceOf[PropertyList.NumericProperty])) throw new IllegalArgumentException("PropertyList must be Numeric to sum values")
//    val key2value: HashMap[String, Double] = new HashMap[String, Double]
//    val iter: PropertyList#Iterator = pl.numericIterator
//    while (iter.hasNext) {
//      iter.nextProperty
//      val key: String = iter.getKey
//      val vall: Double = iter.getNumericValue
//      val storedValue: Double = key2value.get(key).asInstanceOf[Double]
//      if (storedValue == null) key2value.put(key, vall)
//      else key2value.put(key, storedValue.doubleValue + vall)
//    }
//    var ret: PropertyList = null
//    val hashIter: Iterator[_] = key2value.keySet.iterator
//    while (hashIter.hasNext) {
//      val key: String = hashIter.next.asInstanceOf[String]
//      val `val`: Double = (key2value.get(key).asInstanceOf[Double]).doubleValue
//      ret = PropertyList.add(key, `val`, ret)
//    }
//    return ret
//  }
//
//  private final val serialVersionUID: Long = 1
//  private final val CURRENT_SERIAL_VERSION: Int = 0
//
//  private object NumericProperty {
//    private final val serialVersionUID: Long = 1
//    private final val CURRENT_SERIAL_VERSION: Int = 0
//  }
//
//  private class NumericProperty extends PropertyList with Serializable {
//    def this(key: String, value: Double, rest: PropertyList) {
//      this()
//      `super`(key, rest)
//      this.value = value
//    }
//
//    private def writeObject(out: ObjectOutputStream) {
//      out.writeInt(CURRENT_SERIAL_VERSION)
//      out.writeDouble(value)
//    }
//
//    private def readObject(in: ObjectInputStream) {
//      val version: Int = in.readInt
//      value = in.readDouble
//    }
//
//    protected var value: Double = .0
//  }
//
//  private object ObjectProperty {
//    private final val serialVersionUID: Long = 1
//    private final val CURRENT_SERIAL_VERSION: Int = 0
//  }
//
//  private class ObjectProperty extends PropertyList {
//    def this(key: String, value: AnyRef, rest: PropertyList) {
//      this()
//      `super`(key, rest)
//      this.value = value
//    }
//
//    private def writeObject(out: ObjectOutputStream) {
//      out.writeInt(CURRENT_SERIAL_VERSION)
//      out.writeObject(value)
//    }
//
//    private def readObject(in: ObjectInputStream) {
//      val version: Int = in.readInt
//      value = in.readObject.asInstanceOf[AnyRef]
//    }
//
//    protected var value: AnyRef = null
//  }
//
//}
//
//class PropertyList extends Serializable {
//  def lookupObject(key: String): AnyRef = {
//    if (this.key == key) {
//      if (this.isInstanceOf[PropertyList.ObjectProperty]) return (this.asInstanceOf[PropertyList.ObjectProperty]).value
//      else if (this.isInstanceOf[PropertyList.NumericProperty]) return new Double((this.asInstanceOf[PropertyList.NumericProperty]).value)
//      else throw new IllegalStateException("Unrecognitized PropertyList entry.")
//    }
//    else if (this.next == null) {
//      return null
//    }
//    else {
//      return next.lookupObject(key)
//    }
//  }
//
//  def lookupNumber(key: String): Double = {
//    if (this.key == key) {
//      if (this.isInstanceOf[PropertyList.NumericProperty]) return (this.asInstanceOf[PropertyList.NumericProperty]).value
//      else if (this.isInstanceOf[PropertyList.ObjectProperty]) {
//        val obj: AnyRef = (this.asInstanceOf[PropertyList.ObjectProperty]).value
//        if (obj == null) return 0
//        if (obj.isInstanceOf[Double]) return (obj.asInstanceOf[Double]).doubleValue
//        if (obj.isInstanceOf[Integer]) return (obj.asInstanceOf[Double]).intValue
//        if (obj.isInstanceOf[Float]) return (obj.asInstanceOf[Double]).floatValue
//        if (obj.isInstanceOf[Short]) return (obj.asInstanceOf[Double]).shortValue
//        if (obj.isInstanceOf[Long]) return (obj.asInstanceOf[Double]).longValue
//        return 0
//      }
//      else throw new IllegalStateException("Unrecognitized PropertyList entry.")
//    }
//    else if (this.next == null) {
//      return 0
//    }
//    else {
//      return next.lookupNumber(key)
//    }
//  }
//
//  def hasProperty(key: String): Boolean = {
//    if (this.key == key) {
//      if (this.isInstanceOf[PropertyList.ObjectProperty] && (this.asInstanceOf[PropertyList.ObjectProperty]).value == null) return false
//      else return true
//    }
//    else if (this.next == null) {
//      return false
//    }
//    else {
//      return next.hasProperty(key)
//    }
//  }
//
//  def iterator: PropertyList#Iterator = {
//    return new PropertyList#Iterator(this)
//  }
//
//  def numericIterator: PropertyList#Iterator = {
//    return new PropertyList#NumericIterator(this)
//  }
//
//  def objectIterator: PropertyList#Iterator = {
//    return new PropertyList#ObjectIterator(this)
//  }
//
//  protected def this() {
//    this()
//    throw new IllegalArgumentException("Zero args constructor not allowed.")
//  }
//
//  protected def this(key: String, rest: PropertyList) {
//    this()
//    this.key = key
//    this.next = rest
//  }
//
//  def print {
//    if (this.isInstanceOf[PropertyList.NumericProperty]) System.out.println(this.key.toString + "=" + (this.asInstanceOf[PropertyList.NumericProperty]).value)
//    else if (this.isInstanceOf[PropertyList.ObjectProperty]) System.out.println(this.key.toString + "=" + (this.asInstanceOf[PropertyList.ObjectProperty]).value)
//    else throw new IllegalArgumentException("Unrecognized PropertyList type")
//    if (this.next != null) this.next.print
//  }
//
//  private def writeObject(out: ObjectOutputStream) {
//    out.writeInt(CURRENT_SERIAL_VERSION)
//    out.writeObject(next)
//    out.writeObject(key)
//  }
//
//  private def readObject(in: ObjectInputStream) {
//    val version: Int = in.readInt
//    next = in.readObject.asInstanceOf[PropertyList]
//    key = in.readObject.asInstanceOf[String]
//  }
//
//  protected var next: PropertyList = null
//  protected var key: String = null
//
//  object Iterator {
//    private final val serialVersionUID: Long = 1
//    private final val CURRENT_SERIAL_VERSION: Int = 0
//  }
//
//  class Iterator extends java.util.Iterator with Serializable {
//    def this(pl: PropertyList) {
//      this()
//      property = findReturnablePropertyAtOrAfter(pl)
//      if (property == null) nextProperty = null
//      else nextProperty = findReturnablePropertyAtOrAfter(property.next)
//    }
//
//    private def findReturnablePropertyAtOrAfter(property: PropertyList): PropertyList = {
//      while (property != null) {
//        if (property.isInstanceOf[PropertyList.NumericProperty] && returnNumeric) {
//          if ((property.asInstanceOf[PropertyList.NumericProperty]).value == 0.0) {
//            if (deletedKeys == null) deletedKeys = new HashSet[_]
//            deletedKeys.add(property.key)
//            property = property.next
//          }
//          else break //todo: break is not supported
//        }
//        else if (property.isInstanceOf[PropertyList.ObjectProperty] && returnObject) {
//          if ((property.asInstanceOf[PropertyList.ObjectProperty]).value == null) {
//            if (deletedKeys == null) deletedKeys = new HashSet[_]
//            deletedKeys.add(property.key)
//            property = property.next
//          }
//          else break //todo: break is not supported
//        }
//        else throw new IllegalStateException("Unrecognized property type " + property.getClass.getName)
//      }
//      return property
//    }
//
//    def hasNext: Boolean = {
//      return ((nextCalled && nextProperty != null) || (!nextCalled && property != null))
//    }
//
//    def isNumeric: Boolean = {
//      return (property.isInstanceOf[PropertyList.NumericProperty])
//    }
//
//    def getNumericValue: Double = {
//      return (property.asInstanceOf[PropertyList.NumericProperty]).value
//    }
//
//    def getObjectValue: AnyRef = {
//      return (property.asInstanceOf[PropertyList.ObjectProperty]).value
//    }
//
//    def getKey: String = {
//      return property.key
//    }
//
//    def nextProperty: PropertyList = {
//      if (nextCalled) {
//        property = nextProperty
//        nextProperty = findReturnablePropertyAtOrAfter(property.next)
//      }
//      else nextCalled = true
//      return property
//    }
//
//    def next: AnyRef = {
//      return nextProperty
//    }
//
//    def remove {
//      throw new UnsupportedOperationException
//    }
//
//    private def writeObject(out: ObjectOutputStream) {
//      out.writeInt(CURRENT_SERIAL_VERSION)
//      out.writeObject(property)
//      out.writeObject(nextProperty)
//      out.writeObject(deletedKeys)
//      out.writeBoolean(nextCalled)
//      out.writeBoolean(returnNumeric)
//      out.writeBoolean(returnObject)
//    }
//
//    private def readObject(in: ObjectInputStream) {
//      val version: Int = in.readInt
//      property = in.readObject.asInstanceOf[PropertyList]
//      nextProperty = in.readObject.asInstanceOf[PropertyList]
//      deletedKeys = in.readObject.asInstanceOf[HashSet[_]]
//      nextCalled = in.readBoolean
//      returnNumeric = in.readBoolean
//      returnObject = in.readBoolean
//    }
//
//    private[util] var property: PropertyList = null
//    private[util] var nextProperty: PropertyList = null
//    private[util] var deletedKeys: HashSet[_] = null
//    private[util] var nextCalled: Boolean = false
//    private[util] var returnNumeric: Boolean = true
//    private[util] var returnObject: Boolean = true
//  }
//
//  class NumericIterator extends Iterator with Serializable {
//    def this(pl: PropertyList) {
//      this()
//      `super`(pl)
//      this.returnObject = false
//    }
//  }
//
//  class ObjectIterator extends Iterator with Serializable {
//    def this(pl: PropertyList) {
//      this()
//      `super`(pl)
//      this.returnNumeric = false
//    }
//  }
//
//}
