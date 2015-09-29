package edu.umass.cs.rexo.ghuang.segmentation.svg

import java.util.logging.{Logger,Level}
import org.rexo.pipeline.components.svg.{RxPipelineSvg, RxDocumentSvg, RxFilterSvd}
import org.rexo.pipeline.components.svg.RxFilterSvd.ReturnCode
import org.rexo.referencetagging.SegmentationException

class SegmentationFilterSvg extends RxFilterSvd {

  val logger = Logger.getLogger("SegmentationFilterSvg")


  def accept(rdoc: RxDocumentSvg): Int = {
    val segmentationFinder: LayoutSegmentFinderSvg = new LayoutSegmentFinderSvg()
    try {
      val results: collection.mutable.Map[String, Any] = segmentationFinder.markup(rdoc.getTokenization)
      rdoc.getScope("document").put("segmentation", results)
      return ReturnCode.OK
    }
    catch {
      case exception: SegmentationException => {
        logger.log(Level.SEVERE, exception.getMessage())
//        exception.printStackTrace()
        rdoc.docErrorString(exception.getMessage)
        return ReturnCode.ABORT_PAPER
      }
    }
  }

  def init(pipeline: RxPipelineSvg): Int = {
    return ReturnCode.OK
  }
}
