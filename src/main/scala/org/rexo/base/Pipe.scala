package org.rexo.base


/**
 * Created by klimzaporojets on 10/2/14.
 * adapted from mallet
 */
abstract class Pipe {
  var targetProcessing = true
  /**
   * Process an Instance.  This method takes an input Instance,
   * destructively modifies it in some way, and returns it.
   * This is the method by which all pipes are eventually run.
   * <p>
   * One can create a new concrete subclass of Pipe simply by
   * implementing this method.
   * @param carrier Instance to be processed.
   */
  def pipe(carrier: Instance): Instance

  /** Return true iff this pipe expects and processes information in
			the <tt>target</tt> slot. */
  def isTargetProcessing: Boolean = {
    return targetProcessing
  }

  def setTargetProcessing(targetProcessing:Boolean):Unit =
  {
    this.targetProcessing = targetProcessing
  }
}
