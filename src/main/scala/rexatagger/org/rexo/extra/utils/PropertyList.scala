package org.rexo.extra.utils

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util

//import java.util.{HashSet, HashMap}
//import java.io.Serializable
//import scala.Serializable

/**
 * Created by klimzaporojets on 9/26/14.
 */
object PropertyList {
    def iterator(arg:PropertyList): PropertyList#Iterador = {
      //return  new PropertyList#Iterador(arg)
      return new arg.Iterador(arg)
      //PropertyList.
    }

    def numericIterator(arg:PropertyList): PropertyList#Iterador = {
      //return new PropertyList#NumericIterator(arg)
      return new arg.NumericIterator(arg)
    }

    def objectIterator(arg:PropertyList): PropertyList#Iterador = {
      //return new PropertyList#ObjectIterator(arg)
      return new arg.ObjectIterator(arg)
    }

  def add(key: String, value: Any, rest: PropertyList): PropertyList = {
    assert((key != null))
    return new PropertyList.ObjectProperty(key, value, rest)
  }

  def add(key: String, value: String, rest: PropertyList): PropertyList = {
    assert((key != null))
    return new PropertyList.ObjectProperty(key, value, rest)
  }

  def add(key: String, value: Double, rest: PropertyList): PropertyList = {
    assert((key != null))
    return new PropertyList.NumericProperty(key, value, rest)
  }

  def remove(key: String, rest: PropertyList): PropertyList = {
    assert((key != null))
    return new PropertyList.ObjectProperty(key, null, rest)
  }

  def sumDuplicateKeyValues(pl: PropertyList): PropertyList = {
    if (!(pl.isInstanceOf[PropertyList.NumericProperty])) throw new IllegalArgumentException("PropertyList must be Numeric to sum values")
    val key2value: collection.mutable.Map[Any, Any] = collection.mutable.Map() //HashMap[_, _] = new HashMap[_, _]
    val iter: PropertyList#Iterador = PropertyList.numericIterator(pl) // pl.numericIterator
    while (iter.hasNext) {
      iter.nextProperty
      val key: String = iter.getKey
      val vall: Double = iter.getNumericValue
      val storedValue: Double = key2value.get(key).asInstanceOf[Double]
      if (storedValue == null) key2value.put(key, vall.toDouble /*new Double(vall)*/)
      else key2value.put(key, vall.toDouble /*new Double(storedValue.doubleValue + vall)*/)
    }
    var ret: PropertyList = null
    val hashIter: Iterator[_] = key2value.keySet.iterator
    while (hashIter.hasNext) {
      val key: String = hashIter.next.asInstanceOf[String]
      val vall: Double = (key2value.get(key).asInstanceOf[Double]).doubleValue
      ret = PropertyList.add(key, vall, ret)
    }
    return ret
  }

  private final val serialVersionUID: Long = 1
  private final val CURRENT_SERIAL_VERSION: Int = 0

  private object NumericProperty {
    private final val serialVersionUID: Long = 1
    private final val CURRENT_SERIAL_VERSION: Int = 0
  }

  class NumericProperty(override val key:String, var value:Double, override val next:PropertyList) extends PropertyList(key, next) with Serializable {

//is it necessary?
//    override var key = keyi
//    override var next = keyi

//    def this(key: String, value: Double, rest: PropertyList) {
//      this()
//      super (key, rest)
//      this.value = value
//    }

    private def writeObject(out: ObjectOutputStream) {
      out.writeInt(CURRENT_SERIAL_VERSION)
      out.writeDouble(value)
    }

    private def readObject(in: ObjectInputStream) {
      val version: Int = in.readInt
      value = in.readDouble
    }

//    var value: Double = .0
  }

  private object ObjectProperty {
    private final val serialVersionUID: Long = 1
    private final val CURRENT_SERIAL_VERSION: Int = 0
  }

  private class ObjectProperty(override val key:String, var value:Any, override val next:PropertyList) extends PropertyList(key, next) {
//    def this(key: String, value: AnyRef, rest: PropertyList) {
//      this()
//      `super`(key, rest)
//      this.value = value
//    }

    private def writeObject(out: ObjectOutputStream) {
      out.writeInt(CURRENT_SERIAL_VERSION)
      out.writeObject(value)
    }

    private def readObject(in: ObjectInputStream) {
      val version: Int = in.readInt
      value = in.readObject.asInstanceOf[AnyRef]
    }

//    var value: AnyRef = null
  }

}

class PropertyList(val key: String, val next: PropertyList) extends Serializable {
//  var key:String = keyi
//  var next:PropertyList = nexti


  def lookupObject(key: String): Any = {
    if (this.key == key) {
      if (this.isInstanceOf[PropertyList.ObjectProperty])
      {
        val oprop = (this.asInstanceOf[PropertyList.ObjectProperty])
        return oprop.value
      }
      else if (this.isInstanceOf[PropertyList.NumericProperty]) return (this.asInstanceOf[PropertyList.NumericProperty]).value //new Double((this.asInstanceOf[PropertyList.NumericProperty]).value)
      else throw new IllegalStateException("Unrecognitized PropertyList entry.")
    }
    else if (this.next == null) {
      return null
    }
    else {
      return next.lookupObject(key)
    }
  }

  def lookupNumber(key: String): Double = {
    if (this.key == key) {
      if (this.isInstanceOf[PropertyList.NumericProperty]) return (this.asInstanceOf[PropertyList.NumericProperty]).value
      else if (this.isInstanceOf[PropertyList.ObjectProperty]) {
        val obj: Any = (this.asInstanceOf[PropertyList.ObjectProperty]).value
        if (obj == null) return 0
        if (obj.isInstanceOf[Double]) return (obj.asInstanceOf[Double]).doubleValue
        if (obj.isInstanceOf[Integer]) return (obj.asInstanceOf[Double]).intValue
        if (obj.isInstanceOf[Float]) return (obj.asInstanceOf[Double]).floatValue
        if (obj.isInstanceOf[Short]) return (obj.asInstanceOf[Double]).shortValue
        if (obj.isInstanceOf[Long]) return (obj.asInstanceOf[Double]).longValue
        return 0
      }
      else throw new IllegalStateException("Unrecognitized PropertyList entry.")
    }
    else if (this.next == null) {
      return 0
    }
    else {
      return next.lookupNumber(key)
    }
  }

  def hasProperty(key: String): Boolean = {
    if (this.key == key) {
      if (this.isInstanceOf[PropertyList.ObjectProperty] && (this.asInstanceOf[PropertyList.ObjectProperty]).value == null) return false
      else return true
    }
    else if (this.next == null) {
      return false
    }
    else {
      return next.hasProperty(key)
    }
  }

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

//  protected def this() {
//    this()
//    throw new IllegalArgumentException("Zero args constructor not allowed.")
//  }

//  protected def this(key: String, rest: PropertyList) {
//    this()
//    this.key = key
//    this.next = rest
//  }

  def print {
    if (this.isInstanceOf[PropertyList.NumericProperty]) System.out.println(this.key.toString + "=" + (this.asInstanceOf[PropertyList.NumericProperty]).value)
    else if (this.isInstanceOf[PropertyList.ObjectProperty]) System.out.println(this.key.toString + "=" + (this.asInstanceOf[PropertyList.ObjectProperty]).value)
    else throw new IllegalArgumentException("Unrecognized PropertyList type")
    if (this.next != null) this.next.print
  }

  private def writeObject(out: ObjectOutputStream) {
    out.writeInt(PropertyList.CURRENT_SERIAL_VERSION)
    out.writeObject(next)
    out.writeObject(key)
  }

  private def readObject(in: ObjectInputStream) {
    val version: Int = in.readInt
//todo: see if it is necessary , because if not, will have to declare next and key as vars, complicating with overriding in
    //subclasses
//    next = in.readObject.asInstanceOf[PropertyList]
//    key = in.readObject.asInstanceOf[String]
  }

//  protected var next: PropertyList = null
//  protected var key: String = null

  object Iterador {
    private final val serialVersionUID: Long = 1
    private final val CURRENT_SERIAL_VERSION: Int = 0
//    def apply(pl:PropertyList):PropertyList#Iterador =
//    {
//      val ni:PropertyList#Iterador = new PropertyList#Iterador(pl)
//      return ni
//    }
  }

  class Iterador(pl:PropertyList) extends java.util.Iterator[Any] with Serializable {
//    this()
//    def this(pl: PropertyList) {
//      this()
      property = findReturnablePropertyAtOrAfter(pl)
      if (property == null) nextProperty = null
      else nextProperty = findReturnablePropertyAtOrAfter(property.next)
//    }

    private def findReturnablePropertyAtOrAfter(property: PropertyList): PropertyList = {
      var retProperty:PropertyList = null
      while (property != null) {
        if (property.isInstanceOf[PropertyList.NumericProperty] /*&& returnNumeric*/) {
          if ((property.asInstanceOf[PropertyList.NumericProperty]).value == 0.0) {
            if (deletedKeys == null) deletedKeys = new util.HashSet[Any]()
            deletedKeys.add(property.key)
            retProperty = property.next
          }
          else return retProperty //break //todo: break is not supported
        }
        else if (property.isInstanceOf[PropertyList.ObjectProperty] && returnObject) {
          if ((property.asInstanceOf[PropertyList.ObjectProperty]).value == null) {
            if (deletedKeys == null) deletedKeys = new util.HashSet[Any]() //collection.mutable.Set() //new HashSet[_]
            deletedKeys.add(property.key)
            retProperty = property.next
          }
          else return retProperty //break //todo: break is not supported
        }
        else throw new IllegalStateException("Unrecognized property type " + property.getClass.getName)
      }
      //return property
      return retProperty
    }

    def hasNext: Boolean = {
      return ((nextCalled && nextProperty != null) || (!nextCalled && property != null))
    }

    def isNumeric: Boolean = {
      return (property.isInstanceOf[PropertyList.NumericProperty])
    }

    def getNumericValue: Double = {
      return (property.asInstanceOf[PropertyList.NumericProperty]).value
    }

    def getObjectValue: Any = {
      return (property.asInstanceOf[PropertyList.ObjectProperty]).value
    }

    def getKey: String = {
      return property.key
    }

    def getNextProperty(): PropertyList = {
      if (nextCalled) {
        property = nextProperty
        nextProperty = findReturnablePropertyAtOrAfter(property.next)
      }
      else nextCalled = true
      return property
    }

    def next: AnyRef = {
      return nextProperty
    }

    def remove {
      throw new UnsupportedOperationException
    }

    private def writeObject(out: ObjectOutputStream) {
      out.writeInt(PropertyList.CURRENT_SERIAL_VERSION)
      out.writeObject(property)
      out.writeObject(nextProperty)
      out.writeObject(deletedKeys)
      out.writeBoolean(nextCalled)
      out.writeBoolean(returnNumeric)
      out.writeBoolean(returnObject)
    }

    private def readObject(in: ObjectInputStream) {
      val version: Int = in.readInt
      property = in.readObject.asInstanceOf[PropertyList]
      nextProperty = in.readObject.asInstanceOf[PropertyList]
      deletedKeys = in.readObject.asInstanceOf[util.HashSet[Any]]
      nextCalled = in.readBoolean
      returnNumeric = in.readBoolean
      returnObject = in.readBoolean
    }

    private[utils] var property: PropertyList = null
    private[utils] var nextProperty: PropertyList = null
    private[utils] var deletedKeys: util.HashSet[Any] = null //collection.mutable.Set[Any] // HashSet[_] = null
    private[utils] var nextCalled: Boolean = false
    private[utils] var returnNumeric: Boolean = true
    private[utils] var returnObject: Boolean = true
  }

  class NumericIterator(pl:PropertyList) extends Iterador(pl) with Serializable {
    //TODO: is if toString works better
    this.returnObject = true
//    this.returnObject = false
//    def this(pl: PropertyList) {
//      this()
//      `super`(pl)
//      this.returnObject = false
//    }
  }

  class ObjectIterator(pl:PropertyList) extends Iterador(pl) with Serializable {
    this.returnNumeric = true
//    this.returnNumeric = true
//    def this(pl: PropertyList) {
//      super(pl)
//      this.returnNumeric = false
//    }
  }

}