package org.rexo.pipeline

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import edu.umass.cs.rexo.ghuang.segmentation.Token2BodyFeatureSequence
import org.rexo.pipeline.components.RxDocument
import org.rexo.pipeline.components.RxFilter.ReturnCode
import org.rexo.extraction.{CRFOutputFormatter, NewHtmlTokenization}
import scala.collection.mutable
import org.rexo.extra.types.{PropertyHolder, Sequence}
import org.rexo.pipeline.extractors.{BodyRulesTransducer, RulesExtractor}
import org.rexo.base.pipe.SerialPipes
import org.rexo.base.{Instance, Pipe}


/**
 * @author klimzaporojets
 *         Analogous to ReferenceExtractionFilter, but for extracting the sections from body.
 *
 */
object BodyExtractionFilter {
//  private var log: Logger = Logger.getLogger(classOf[BodyExtractionFilter])
}

class BodyExtractionFilter extends AbstractFilter {
  private var _bodyExtractor: RulesExtractor = null

  val log = Logger(LoggerFactory.getLogger("BodyExtractionFilter"))
//  def this() {
//    this()
    initExtractor
//  }

  private def initExtractor {
    val pipes: mutable.MutableList[Pipe] = mutable.MutableList[Pipe]() //new ArrayList[_]
    //todo: see if NewHtmlTokenization2TokenSequence is required
//    pipes.add(new NewHtmlTokenization2TokenSequence)
//    pipes.add(new Token2BodyFeatureSequence)

    pipes.+=(new Token2BodyFeatureSequence)
    val sp: SerialPipes = new SerialPipes(pipes)
    _bodyExtractor = new RulesExtractor(sp)
//    _bodyExtractor.setTokenizationPipe(null)
  }

//  private def loadPartiallyTrainedModel(crfFile: File): CRFExtractor = {
//    val crf: CRF4 = new ObjectInputStream(new BufferedInputStream(new FileInputStream(crfFile))).readObject.asInstanceOf[CRF4]
//    val tokPipe: Pipe = new SerialPipes(Array[Pipe](new Noop))
//    tokPipe.setTargetProcessing(false)
//    return new CRFExtractor(crf, tokPipe)
//  }

//  private def loadCrfExtor(crfFile: File): CRFExtractor = {
//    val ois: ObjectInputStream = new ObjectInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(crfFile))))
//    return ois.readObject.asInstanceOf[CRFExtractor]
//  }

  def accept(rdoc: RxDocument): Int = {
    var errorCode: Int = ReturnCode.OK
    try {
      errorCode = if (doExtraction(rdoc)) ReturnCode.OK else ReturnCode.ABORT_PAPER
    }
    catch {
      case e: Exception => {
        errorCode = ReturnCode.ABORT_PAPER
        e.printStackTrace
        log.info("(crf) " + e.getClass.getName + ": " + e.getMessage)
      }
    }
    return errorCode
  }

  /**
   * @param rdoc
   */
  private def doExtraction(rdoc: RxDocument): Boolean = {
    val tokenization: NewHtmlTokenization = rdoc.getTokenization
//    val referenceElements: ArrayList[_] = new ArrayList[_]
    val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").get.asInstanceOf[collection.mutable.Map[Any, Any]]
    if (tokenization == null) {
      getLogger(rdoc).error("Partitioner found nothing to partition...")
      rdoc.docErrorString("Partitioner found nothing to partition")
      return false
    }
    if (_bodyExtractor != null) {
      val body: NewHtmlTokenization = segmentations.get("bodyTokenization").get.asInstanceOf[NewHtmlTokenization]
      if (body != null) {
        log.info("running crf on body")
        if (body.clearTokenFeatures) {
          log.warn("body tokens had features set before crf extraction")
        }
        _bodyExtractor.extract(body)
        val carrier: Instance = _bodyExtractor.getCarrier
        val bodyRulesTransducer: BodyRulesTransducer = new BodyRulesTransducer
        val predictedLabels: Sequence = bodyRulesTransducer.transduce(carrier.getData.asInstanceOf[NewHtmlTokenization])
        val crfOutputFormatter: CRFOutputFormatter = new CRFOutputFormatter
//        val element: Element = crfOutputFormatter.toXmlElement(body, predictedLabels, "body")
//        val element: Element = crfOutputFormatter.toHtmlElement(body, predictedLabels, "body")
        crfOutputFormatter.updateHtmlTags(body,predictedLabels,"body")

        val firstHeaderToken: PropertyHolder = body.getToken(0)
        val llx: Double = firstHeaderToken.getNumericProperty("llx")
        val lly: Double = firstHeaderToken.getNumericProperty("lly")
        val pageNum: Int = firstHeaderToken.getNumericProperty("pageNum").asInstanceOf[Int]
        val persistentMentionID: String = "p" + pageNum + "x" + llx + "y" + lly

//        element.setAttribute("bodyID", persistentMentionID)
//        segmentations.put("bodyElement", element)
      }
    }
    return true
  }

  private def checkReference(tokens: NewHtmlTokenization, predictedTags: Sequence): String = {
    assert(tokens.size == predictedTags.size)
    var seenMarker: Boolean = false
    var seenAuthors: Boolean = false
    var seenTitle: Boolean = false
    var warning: String = ""
    var previousTag: String = ""
      var i: Int = 0
      while (i < predictedTags.size) {
        {
          val tag: String = predictedTags.get(i).toString
          var truncateHere: Boolean = false
          if (previousTag.startsWith("ref-marker") && !tag.startsWith("ref-marker")) {
            seenMarker = true
          }
          if (previousTag.startsWith("author") && !tag.startsWith("author")) {
            seenAuthors = true
          }
          if (previousTag.startsWith("title") && !tag.startsWith("title")) {
            seenTitle = true
          }
          val newMarker: Boolean = (tag.startsWith("ref-marker") && !previousTag.startsWith("ref-marker"))
          if ((seenMarker || seenAuthors || seenTitle) && newMarker) {
            truncateHere = true
            warning = warning + "duplicate ref-marker;"
          }
          val newAuthor: Boolean = (tag.startsWith("author") && !previousTag.startsWith("author"))
          if (seenAuthors && newAuthor) {
            truncateHere = true
            warning = warning + "duplicate authors;"
          }
          val newTitle: Boolean = (tag.startsWith("title") && !previousTag.startsWith("title"))
          if (seenTitle && newTitle) {
            truncateHere = true
            warning = warning + "duplicate title;"
          }
          previousTag = tag
        }
        ({
          i += 1; i - 1
        })
      }
    warning
  }

}

