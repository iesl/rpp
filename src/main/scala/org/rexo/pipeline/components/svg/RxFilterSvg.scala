package org.rexo.pipeline.components.svg

/**
 * Created by klimzaporojets on 9/25/14.
 */
/**
 * @author asaunders
 */
object RxFilterSvd {

  object ReturnCode {
    final val OK: Int = 0
    final val WARNING: Int = 1
    final val ABORT_PAPER: Int = 2
    final val ABORT_SESSION: Int = 3
    final val ERROR: Int = 4
  }

}

abstract trait RxFilterSvd {
  def accept(rdoc: RxDocumentSvg): Int

  def init(pipeline: RxPipelineSvg): Int

}


