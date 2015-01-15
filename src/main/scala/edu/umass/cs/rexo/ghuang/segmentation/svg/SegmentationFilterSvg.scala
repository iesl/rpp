package edu.umass.cs.rexo.ghuang.segmentation.svg

//import org.apache.log4j.Logger
import org.rexo.pipeline.components.{RxPipeline, RxDocument, RxFilter}
import org.rexo.extraction.{CRFOutputFormatter, NewHtmlTokenization}
import edu.umass.cs.rexo.ghuang.segmentation.LayoutSegmentFinder
import org.rexo.pipeline.components.svg.{RxPipelineSvg, RxFilterSvd, RxDocumentSvg}
import org.rexo.pipeline.components.svg.RxFilterSvd.ReturnCode
import org.rexo.referencetagging.SegmentationException


/**
 * Created by klimzaporojets on 9/25/14.
 */
object SegmentationFilterSvg {
}

class SegmentationFilterSvg extends RxFilterSvd {

  def accept(rdoc: RxDocumentSvg): Int = {
    val segmentationFinder: LayoutSegmentFinderSvg = new LayoutSegmentFinderSvg()
    try {
      val results: collection.mutable.Map[String, Any] = segmentationFinder.markup(rdoc.getTokenization)
      rdoc.getScope("document").put("segmentation", results)


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

  def init(pipeline: RxPipelineSvg): Int = {
    return ReturnCode.OK
  }
}
