package org.rexo.referencetagging

/**
 * Created by klimzaporojets on 9/25/14.
 */
class ReferencesNotFoundException private(ex: SegmentationException) extends SegmentationException(ex) {
  def this(message:String) = this(new SegmentationException(message))
  def this(message:String, throwable: Throwable) = this(new SegmentationException(message, throwable))
}

object ReferencesNotFoundException {
  def apply(message:String) = new ReferencesNotFoundException(message)
  def apply(message:String, throwable: Throwable) = new ReferencesNotFoundException(message, throwable)
}