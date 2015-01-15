package org.rexo.extra.extract

/**
 * Created by klimzaporojets on 9/17/14.
 */
/** A sub-section of a document, either linear or two-dimensional.
  * Spans are immutable. */
abstract trait Span {
  /** Returns a textual representatio of the span, suitable for XML output, e.g. */
  def getText: String

  /** Returns a new span that is the intersection of this span and another. */
  def intersection(r: Span): Span

  def getDocument: AnyRef

  def intersects(r: Span): Boolean

  def isSubspan(r: Span): Boolean

  /**
   * Returns an integer index identifying the start of this span.
   * Beware that in some cases (e.g., for images), this may not
   * correspond directly to a sequence index.
   */
  def getStartIdx: Int

  /**
   * Returns an integer index identifying the end of this span.
   * Beware that in some cases (e.g., for images), this may not
   * correspond directly to a sequence index.
   */
  def getEndIdx: Int
}

