package org.rexo.referencetagging

/**
 * Created by klimzaporojets on 9/25/14.
 */
//class SegmentationException extends Exception {
//  def this(message: String) {
//    this()
//    super(message)
//  }
//}

class SegmentationException (ex: Exception) extends Exception(ex) {
  def this(message:String) = this(new Exception(message))
  def this(message:String, throwable: Throwable) = this(new Exception(message, throwable))
}

object SegmentationException {
  def apply(message:String) = new Exception(message)
  def apply(message:String, throwable: Throwable) = new Exception(message, throwable)
}