package org.rexo.base.extract

import org.rexo.extra.extract.Span
import org.rexo.extra.types.Sequence

/**
 * Created by klimzaporojets on 10/2/14.
 */
abstract trait Tokenization extends Sequence {

  def getDocument: AnyRef

  def getSpan(i: Int): Span

  def subspan(start: Int, end: Int): Span
}
