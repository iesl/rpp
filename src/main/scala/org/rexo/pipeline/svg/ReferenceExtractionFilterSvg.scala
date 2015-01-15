package org.rexo.pipeline.svg

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import org.rexo.extraction.{NewHtmlTokenizationSvg, CRFOutputFormatter}
import org.jdom2.Element
import org.rexo.pipeline.components.svg.RxDocumentSvg
import org.rexo.extra.types.Sequence

import org.rexo.pipeline.components.RxFilter.ReturnCode

/**
* Created by klimzaporojets on 9/26/14.
*/
/**
  */
object ReferenceExtractionFilterSvg {
//  private var log: Logger = Logger.getLogger(classOf[ReferenceExtractionFilter])
}

class ReferenceExtractionFilterSvg extends AbstractFilterSvg {
  private var log = Logger(LoggerFactory.getLogger("ScalaTagger"))

  //TODO: check what type this should be ?? Look into Factorie
  private var _referencesExtractor: Any = null
  private var _headersExtractor: Any = null


  private def initCrfs(referenceCrfFile:String, headerCrfFile:String) {
    _referencesExtractor = null
    _headersExtractor = null
  }

  private def loadPartiallyTrainedModel(crfFile: String): Any = {
    //todo: see how to implement with factorie
  }

  private def loadCrfExtor(crfFile: String): Any = {
    //todo: see how to implement with factorie
  }

  def accept(rdoc: RxDocumentSvg): Int = {
    var errorCode: Int = ReturnCode.OK
    try {
      errorCode = if (doExtraction(rdoc)) ReturnCode.OK else ReturnCode.ABORT_PAPER
    }
    catch {
      case e: Exception => {
        errorCode = ReturnCode.ABORT_PAPER
        log.info("(crf) " + e.getClass.getName + ": " + e.getMessage)
        e.printStackTrace()
      }
    }
    return errorCode
  }

  /**
   * @param rdoc
   */
  private def doExtraction(rdoc: RxDocumentSvg): Boolean = {
    val tokenization: NewHtmlTokenizationSvg = rdoc.getTokenization
    val referenceElements: collection.mutable.MutableList[Element] = collection.mutable.MutableList() //ArrayList[_] = new ArrayList[_]
    val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").get.asInstanceOf[collection.mutable.Map[Any, Any]]
    if (tokenization == null) {
      getLogger(rdoc).error("Partitioner found nothing to partition...")
      rdoc.docErrorString("Partitioner found nothing to partition")
      return false
    }

      val refList: collection.mutable.MutableList[Any] = segmentations.get("referenceList").get.asInstanceOf[collection.mutable.MutableList[Any]]
      if (refList == null) {
        getLogger(rdoc).error("no biblio to extract")
        rdoc.docErrorString("no biblio to extract")
        return false
      }
      val referenceIterator: Iterator[_] = refList.iterator
      var refNum: Int = 1
      while (referenceIterator.hasNext) {

        val reference: NewHtmlTokenizationSvg = referenceIterator.next.asInstanceOf[NewHtmlTokenizationSvg];

        val crfOutputFormatter: CRFOutputFormatter = new CRFOutputFormatter

        val predictedLabels: Sequence = null;

        crfOutputFormatter.updateHtmlTagsSvd(reference, predictedLabels, "reference")
        //TODO: here Kate's extraction code instead of previous mallet CRF
      }
    segmentations.put("referenceElements", referenceElements)
    return true
  }

}
