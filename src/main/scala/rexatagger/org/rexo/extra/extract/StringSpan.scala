package org.rexo.extra.extract

import org.rexo.extra.types.Token
import java.io.{ObjectInputStream, ObjectOutputStream}


/**
 * Created by klimzaporojets on 9/26/14.
 */
/**
 * Adapted from java
 * TODO: translate to Scala
 */
object StringSpan {
  private def constructTokenText(doc: CharSequence, start: Int, end: Int): String = {
    val subseq: CharSequence = doc.subSequence(start, end)
    return subseq.toString.intern
  }

  private final val serialVersionUID: Long = 1
  private final val CURRENT_SERIAL_VERSION: Int = 1
}

class StringSpan(document:CharSequence, start:Int, end:Int)
          extends Token(StringSpan.constructTokenText(document,start,end)) with Span {


  def intersection(r: Span): Span = {
    val other: StringSpan = r.asInstanceOf[StringSpan]
    val newStart: Int = Math.max(start, other.getStartIdx) //.start )
    val newEnd: Int = Math.min(end, other.getEndIdx) //other.end)
    return new StringSpan(document, newStart, newEnd)
  }

  def getDocument: AnyRef = {
    return document
  }

  def intersects(r: Span): Boolean = {
    if (!(r.isInstanceOf[StringSpan])) return false
    val sr: StringSpan = r.asInstanceOf[StringSpan]
    return ((sr.getDocument /*document*/ eq this.document) && !(sr.getEndIdx < this.start || sr.getStartIdx > this.end))
  }

  def isSubspan(r: Span): Boolean = {
    return ((r.getDocument eq this.document) && (this.start <= r.getStartIdx) && (r.getEndIdx <= this.end))
  }

  def getStartIdx: Int = {
    return start
  }

  def getEndIdx: Int = {
    return end
  }

  override def toString: String = {
    //TODO: improve the toString by calling super, so it will also show the respective text associated with the span
    //this is mainly for debugging purposes..
    return /*super.toString +*/ "  span[" + start + ".." + end + "]"
  }

  private def writeObject(out: ObjectOutputStream) {
    out.defaultWriteObject
    out.writeInt(StringSpan.CURRENT_SERIAL_VERSION)
  }

  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject
    val version: Int = in.readInt
  }

}