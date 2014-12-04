package org.rexo.extra.types

/**
 * Created by klimzaporojets on 9/26/14.
 */
abstract trait Sequence {
  def size: Int

  def get(index: Int): AnyRef
}


