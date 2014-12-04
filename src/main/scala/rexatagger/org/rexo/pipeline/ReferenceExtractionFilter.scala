package org.rexo.pipeline.components

//import org.apache.log4j.Logger
//import org.rexo.pipeline.AbstractFilter
//import edu.umass.cs.mallet.base.pipe.Pipe
//import edu.umass.cs.mallet.base.extract.CRFExtractor
//import edu.umass.cs.mallet.base.fst.CRF4
import java.io.{FileInputStream, BufferedInputStream, ObjectInputStream}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import org.rexo.pipeline.AbstractFilter
import org.rexo.extraction.{CRFOutputFormatter, NewHtmlTokenization}
import org.rexo.extra.types.Sequence

//import old.base.types.PropertyHolder

//import edu.umass.cs.mallet.base.pipe.SerialPipes
//import edu.umass.cs.mallet.base.pipe.Noop
import java.util.zip.GZIPInputStream
import org.rexo.pipeline.components.RxFilter.ReturnCode
//import org.rexo.extraction.{CRFOutputFormatter, NewHtmlTokenization}
//import java.util.{Iterator, List, Map, ArrayList}
import scala.Predef.Map
//import edu.umass.cs.mallet.base.extract.Extraction
//import edu.umass.cs.mallet.base.types.Sequence
import org.jdom2.Element
//import edu.umass.cs.mallet.base.types.PropertyHolder
import scala.List

/**
* Created by klimzaporojets on 9/26/14.
*/
/**
  */
object ReferenceExtractionFilter {
//  private var log: Logger = Logger.getLogger(classOf[ReferenceExtractionFilter])
}

class ReferenceExtractionFilter extends AbstractFilter {
//  def this(referenceCrfFile: File, headerCrfFile: File) {
//    this()
//    initCrfs(referenceCrfFile, headerCrfFile)
//  }
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

  def accept(rdoc: RxDocument): Int = {
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
  private def doExtraction(rdoc: RxDocument): Boolean = {
    val tokenization: NewHtmlTokenization = rdoc.getTokenization
    val referenceElements: collection.mutable.MutableList[Element] = collection.mutable.MutableList() //ArrayList[_] = new ArrayList[_]
    val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").get.asInstanceOf[collection.mutable.Map[Any, Any]]
    if (tokenization == null) {
      getLogger(rdoc).error("Partitioner found nothing to partition...")
      rdoc.docErrorString("Partitioner found nothing to partition")
      return false
    }
//    if (_headersExtractor != null) {
//      val header: NewHtmlTokenization = segmentations.get("headerTokenization").asInstanceOf[NewHtmlTokenization]
//      if (header != null) {
//        log.info("running crf on header")
//        if (header.clearTokenFeatures) {
//          log.warn("header tokens had features set before crf extraction")
//        }
////        val extraction: Extraction = _headersExtractor.extract(header)
////        val predictedLabels: Sequence = extraction.getDocumentExtraction(0).getPredictedLabels
//        val crfOutputFormatter: CRFOutputFormatter = new CRFOutputFormatter
//
//        val element: Element = crfOutputFormatter.toXmlElement(header, null /*predictedLabels*/, "headers")
//        val firstHeaderToken: PropertyHolder = header.getToken(0)
//        val llx: Double = firstHeaderToken.getNumericProperty("llx")
//        val lly: Double = firstHeaderToken.getNumericProperty("lly")
//        val pageNum: Int = firstHeaderToken.getNumericProperty("pageNum").asInstanceOf[Int]
//        val persistentMentionID: String = "p" + pageNum + "x" + llx + "y" + lly
//        element.setAttribute("headerID", persistentMentionID)
//        segmentations.put("headerElement", element)
//      }
//    }
//    if (_referencesExtractor != null) {
      val refList: collection.mutable.MutableList[Any] = segmentations.get("referenceList").get.asInstanceOf[collection.mutable.MutableList[Any]]
      if (refList == null) {
        getLogger(rdoc).error("no biblio to extract")
        rdoc.docErrorString("no biblio to extract")
        return false
      }
      val referenceIterator: Iterator[_] = refList.iterator
      var refNum: Int = 1
      while (referenceIterator.hasNext) {

        val reference: NewHtmlTokenization = referenceIterator.next.asInstanceOf[NewHtmlTokenization];
//        System.out.println (/*reference.toString*/ "hello");

        val crfOutputFormatter: CRFOutputFormatter = new CRFOutputFormatter

        //should it be sequence??
        val predictedLabels: Sequence = null;

        crfOutputFormatter.updateHtmlTags(reference, predictedLabels, "reference")
        //TODO: here Kate's extraction code instead of previous mallet CRF
//        if (reference.clearTokenFeatures) {
//          log.warn("reference tokens had features set before crf extraction")
//        }
//        log.info("running crf on reference " + refNum + " of " + refList.size)
//        val extraction: Extraction = _referencesExtractor.extract(reference)
//        val predictedLabels: Sequence = extraction.getDocumentExtraction(0).getPredictedLabels
//        val warning: String = checkReference(reference, predictedLabels)
//        if (!(warning == "")) {
//          log.error("Suspicous reference (" + refNum + "):" + warning)
//        }
//        val crfOutputFormatter: CRFOutputFormatter = new CRFOutputFormatter
//        val element: Element = crfOutputFormatter.toXmlElement(reference, predictedLabels, "reference")
//        val firstRefToken: PropertyHolder = reference.getToken(0).asInstanceOf[PropertyHolder]
//        val llx: Double = firstRefToken.getNumericProperty("llx")
//        val lly: Double = firstRefToken.getNumericProperty("lly")
//        val pageNum: Int = firstRefToken.getNumericProperty("pageNum").asInstanceOf[Int]
//        val persistentMentionID: String = "p" + pageNum + "x" + llx + "y" + lly
//        element.setAttribute("refID", persistentMentionID)
//        referenceElements.add(element)
//        refNum += 1
      }
//    }
    segmentations.put("referenceElements", referenceElements)
    return true
  }

//  private def checkReference(tokens: NewHtmlTokenization, predictedTags: Sequence): String = {
//    assert(tokens.size == predictedTags.size)
//    var seenMarker: Boolean = false
//    var seenAuthors: Boolean = false
//    var seenTitle: Boolean = false
//    var warning: String = ""
//    var previousTag: String = ""
//    {
//      var i: Int = 0
//      while (i < predictedTags.size) {
//        {
//          val tag: String = predictedTags.get(i).toString
//          var truncateHere: Boolean = false
//          if (previousTag.startsWith("ref-marker") && !tag.startsWith("ref-marker")) {
//            seenMarker = true
//          }
//          if (previousTag.startsWith("author") && !tag.startsWith("author")) {
//            seenAuthors = true
//          }
//          if (previousTag.startsWith("title") && !tag.startsWith("title")) {
//            seenTitle = true
//          }
//          val newMarker: Boolean = (tag.startsWith("ref-marker") && !previousTag.startsWith("ref-marker"))
//          if ((seenMarker || seenAuthors || seenTitle) && newMarker) {
//            truncateHere = true
//            warning = warning + "duplicate ref-marker;"
//          }
//          val newAuthor: Boolean = (tag.startsWith("author") && !previousTag.startsWith("author"))
//          if (seenAuthors && newAuthor) {
//            truncateHere = true
//            warning = warning + "duplicate authors;"
//          }
//          val newTitle: Boolean = (tag.startsWith("title") && !previousTag.startsWith("title"))
//          if (seenTitle && newTitle) {
//            truncateHere = true
//            warning = warning + "duplicate title;"
//          }
//          previousTag = tag
//        }
//        ({
//          i += 1; i - 1
//        })
//      }
//    }
//    return warning
//  }

}
