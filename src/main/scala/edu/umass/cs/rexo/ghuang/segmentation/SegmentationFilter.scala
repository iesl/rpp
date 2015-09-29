package edu.umass.cs.rexo.ghuang.segmentation

import org.rexo.pipeline.components.{RxPipeline, RxDocument, RxFilter}
import org.rexo.referencetagging.SegmentationException
import org.rexo.extraction.CRFOutputFormatter

import org.rexo.pipeline.components.RxFilter.ReturnCode

class SegmentationFilter extends RxFilter {

  def accept(rdoc: RxDocument): Int = {
    val segmentationFinder: LayoutSegmentFinder = new LayoutSegmentFinder()
    try {
      val results: collection.mutable.Map[String, Any ] = segmentationFinder.markup(rdoc.getTokenization)
      rdoc.getScope("document").put("segmentation", results)

      //now it also tags it , remove this part if don't want to tag segmentations
      val formatter:CRFOutputFormatter = new CRFOutputFormatter
      formatter.updateSegmentation(results)
      //end of tagging

      return ReturnCode.OK
    }
    catch {
      case exception: SegmentationException => {
        exception.printStackTrace()
        rdoc.docErrorString(exception.getMessage)
        return ReturnCode.ABORT_PAPER
      }
    }
  }

  def init(pipeline: RxPipeline): Int = {
    return ReturnCode.OK
  }
}
