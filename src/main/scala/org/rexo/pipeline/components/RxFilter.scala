package org.rexo.pipeline.components

import org.rexo.pipeline.components.svg.RxDocumentSvg

/**
 * Created by klimzaporojets on 9/25/14.
 */
/**
 * @author asaunders
 */
object RxFilter {

  object ReturnCode {
    final val OK: Int = 0
    final val WARNING: Int = 1
    final val ABORT_PAPER: Int = 2
    final val ABORT_SESSION: Int = 3
    final val ERROR: Int = 4
  }

}

abstract trait RxFilter {
  def accept(rdoc: RxDocument): Int

//  def acceptSvd(rdo: RxDocumentSvd): Int

  def init(pipeline: RxPipeline): Int


}


