package org.rexo.referencetagging

import org.rexo.referencetagging.SegmentationException

/**
 * Created by klimzaporojets on 9/25/14.
 */
class HeaderNotFoundException private(ex: SegmentationException) extends SegmentationException(ex) {
  def this(message:String) = this(new SegmentationException(message))
  def this(message:String, throwable: Throwable) = this(new SegmentationException(message, throwable))
}

object HeaderNotFoundException {
  def apply(message:String) = new HeaderNotFoundException(message)
  def apply(message:String, throwable: Throwable) = new HeaderNotFoundException(message, throwable)
}
