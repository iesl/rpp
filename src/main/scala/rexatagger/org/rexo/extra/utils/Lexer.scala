package org.rexo.extra.utils

/**
 * Created by klimzaporojets on 9/26/14.
 */
abstract trait Lexer extends Iterator[Any] {
  def getStartOffset: Int

  def getEndOffset: Int

  def getTokenString: String

  def hasNext: Boolean

  def next: AnyRef

  def remove
}

