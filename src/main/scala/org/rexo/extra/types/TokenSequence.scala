package org.rexo.extra.types

import org.rexo.extra.pipe.PipeOutputAccumulator
import java.io.{ObjectOutputStream, ObjectInputStream}
import org.rexo.extra.utils.PropertyList

/**
 * Created by klimzaporojets on 9/26/14.
 */
object TokenSequence {
  private final val serialVersionUID: Long = 1
  private final val CURRENT_SERIAL_VERSION: Int = 0

  def apply(tokens:collection.mutable.ArrayBuffer[_]):TokenSequence =
  {
    val tkns:TokenSequence = new TokenSequence
    tkns.tokens = tokens.asInstanceOf[collection.mutable.ArrayBuffer[Token]]
    return tkns
  }
  def apply():TokenSequence =
  {
    val tkns:TokenSequence = new TokenSequence
    tkns.tokens = collection.mutable.ArrayBuffer [Token]()
    return tkns
  }

  def apply(capacity:Int):TokenSequence =
  {
    val tkns:TokenSequence = new TokenSequence
    tkns.tokens = collection.mutable.ArrayBuffer [Token]()
    return tkns
  }

  def apply(tokens:Array[Token]):TokenSequence =
  {
    var i: Int = 0
    val tkns:TokenSequence = new TokenSequence
    tkns.tokens = collection.mutable.ArrayBuffer [Token]()
    while (i < tokens.length) {
      tkns.add(tokens(i))
      i += 1;
    }
    return tkns
  }

  def apply(tokens:Array[AnyRef]):TokenSequence =
  {
    val tkns:TokenSequence = new TokenSequence
    tkns.tokens = collection.mutable.ArrayBuffer[Token]()
    var i: Int = 0
    while (i < tokens.length) {
      tkns.add(new Token(tokens(i).toString))
      i += 1; i - 1
    }
    return tkns
  }
}

class TokenSequence extends PipeOutputAccumulator with Sequence with Serializable {


  var tokens: collection.mutable.ArrayBuffer[Token] = collection.mutable.ArrayBuffer[Token]()
  private var properties: PropertyList = null

  def size: Int = {
    return this.tokens.size
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    sb.append("TokenSequence " + super.toString + "\n")

      var i: Int = 0
      while (i < tokens.size) {
        {
          val tt: String = getToken(i).toString
          sb.append("Token#" + i + ":")
          sb.append(tt)
          if (!tt.endsWith("\n")) sb.append("\n")
        }
        i += 1;
      }

    return sb.toString
  }

  def getToken(i: Int): Token = {
    return tokens(i)
  }

  def get(i: Int): AnyRef = {
    return tokens(i)
  }

  def add(o: AnyRef) {
    if (o.isInstanceOf[Token]) add(o.asInstanceOf[Token])
    else if (o.isInstanceOf[TokenSequence]) add(o.asInstanceOf[TokenSequence])
    else add(new Token(o.toString))
  }

  def add(t: Token) {
    tokens.+=(t)
  }

  def remove(index: Int): AnyRef = {
    //todo: see if it works
    tokens = (tokens.take(index) ++ tokens.drop(index+1))
    return tokens
  }

  def removeLastToken: AnyRef = {
    if (tokens.size > 0) {
      //return tokens.remove(tokens.size - 1)
      tokens = tokens.take(tokens.size-1)
      return tokens
    }
    else return null
  }

  def addAll(ts: TokenSequence) {
    {
      var i: Int = 0
      while (i < ts.size) {
        add(ts.getToken(i))
        i += 1;
      }
    }
  }

  def addAll(tokens: Array[Token]) {
    {
      var i: Int = 0
      while (i < tokens.length) {
        add(tokens(i))
        i += 1;
      }
    }
  }

  def addAll(tokens: Array[AnyRef]) {
    {
      var i: Int = 0
      while (i < tokens.length) {
        {
          if (tokens(i).isInstanceOf[Token]) add(tokens(i).asInstanceOf[Token])
          else add(new Token(tokens(i).toString))
        }
          i += 1;
      }
    }
  }

  def iterator: Iterator[_] = {
    return tokens.iterator
  }

  def clonePipeOutputAccumulator: PipeOutputAccumulator = {
    val ret: TokenSequence = TokenSequence.apply(tokens)
    ret.properties = this.properties
    return ret
  }

  def setNumericProperty(key: String, value: Double) {
    properties = PropertyList.add(key, value, properties)
  }

  def setProperty(key: String, value: AnyRef) {
    properties = PropertyList.add(key, value, properties)
  }

  def getNumericProperty(key: String): Double = {
    return properties.lookupNumber(key)
  }

  def getProperty(key: String): Any = {
    return properties.lookupObject(key)
  }

  def hasProperty(key: String): Boolean = {
    return properties.hasProperty(key)
  }

  private def writeObject(out: ObjectOutputStream) {
    out.writeInt(TokenSequence.CURRENT_SERIAL_VERSION)
    out.defaultWriteObject
  }

  private def readObject(in: ObjectInputStream) {
    val version: Int = in.readInt
    in.defaultReadObject
  }

}
