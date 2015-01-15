//package edu.umass.cs.rexo.ghuang.segmentation
//
//import java.util.{ArrayList, LinkedList}
//import edu.umass.cs.mallet.base.extract.StringSpan
//import edu.umass.cs.rexo.ghuang.segmentation.BibliographyStats
//import org.apache.log4j.Logger
//import edu.umass.cs.mallet.base.pipe.Pipe
//import edu.umass.cs.mallet.base.types.Instance
//import edu.umass.cs.mallet.base.types.Sequence
//import edu.umass.cs.mallet.base.fst.CRF4
//
///**
// * Created by klimzaporojets on 9/25/14.
// */
///**
// * Use a trained CRF to segment references.
// *
// * @author ghuang
// *
// */
//object CRFBibliographySegmentor {
//  private def checkAndSegmentReferences(lines: List[_], predictedLabels: Sequence, result: CRFBibliographySegmentor.ReferenceData): String = {
//    var warning: String = ""
//    var seenPrologue: Boolean = false
//    var seenRef: Boolean = false
//    var seenEpilogue: Boolean = false
//    var seenJunk: Boolean = false
//    var reference: LinkedList[_] = new LinkedList[_]
//    var lineIdx: Int = 0
//    var labIdx: Int = 0
//    while (labIdx < predictedLabels.size && lineIdx < lines.size) {
//      val tag: String = predictedLabels.get(labIdx).toString
//      if (tag == "biblioPrologue") seenPrologue = true
//      else if (tag == "post") seenEpilogue = true
//      else if (tag.startsWith("biblio-")) seenRef = true
//      else if (tag == "junk") seenJunk = true
//      if (seenEpilogue && !seenPrologue) warning += "epilogue section before prologue section; "
//      if (seenEpilogue && !seenRef) warning += "epilogue section before references; "
//      if (!seenRef && seenJunk) warning += "junk line before the first reference; "
//      val lineTok: AnyRef = lines.get(lineIdx)
//      if ((lineTok.isInstanceOf[StringSpan]) && (lineTok.asInstanceOf[StringSpan]).getNumericProperty("isHeaderFooterLine") > 0) {
//        lineIdx += 1
//        continue //todo: continue is not supported
//      }
//      else if (tag == "biblioPrologue") {
//        result.prologueList.add(lines.get(lineIdx))
//      }
//      else if (tag == "post") {
//        result.epilogueList.add(lines.get(lineIdx))
//      }
//      else if (tag == "biblio-B") {
//        result.numReferences += 1
//        if (reference.size > 0) result.referenceLineList.add(reference)
//        reference = new LinkedList[_]
//        reference.add(lines.get(lineIdx))
//      }
//      else if (tag == "biblio-I") {
//        if (reference.size == 0) warning += "biblio-I not after biblio-B, line ignored; "
//        else {
//          reference.add(lines.get(lineIdx))
//        }
//      }
//      labIdx += 1
//      lineIdx += 1
//    }
//    if (reference.size > 0) {
//      result.referenceLineList.add(reference)
//      val stats: BibliographyStats = BibliographyStats.getStats(result.referenceLineList, lines, predictedLabels)
//      if (stats.hasSuspiciousReferences) {
//      }
//    }
//    return warning
//  }
//
//  private var log: Logger = Logger.getLogger(classOf[CRFBibliographySegmentor])
//
//  class ReferenceData {
//    private[segmentation] var prologueList: LinkedList[_] = new LinkedList[_]
//    private[segmentation] var referenceLineList: LinkedList[_] = new LinkedList[_]
//    private[segmentation] var epilogueList: LinkedList[_] = new LinkedList[_]
//    private[segmentation] var numReferences: Int = 0
//  }
//
//}
//
//class CRFBibliographySegmentor {
//  def this(crf: CRF4) {
//    this()
//    m_crf = crf
//  }
//
//  def getInputPipe: Pipe = {
//    return m_crf.getInputPipe
//  }
//
//  def segmentReferences(htmlTokenization: NewHtmlTokenization): CRFBibliographySegmentor.ReferenceData = {
//    val inst: Instance = new Instance(htmlTokenization, null, null, null, m_crf.getInputPipe)
//    val predictedLabels: Sequence = m_crf.transduce(inst.getData.asInstanceOf[Sequence])
//    val ret: CRFBibliographySegmentor.ReferenceData = new CRFBibliographySegmentor.ReferenceData
//    val lineSpans: ArrayList[_] = new ArrayList[_]
//    lineSpans.addAll(htmlTokenization.getLineSpans)
//    assert((lineSpans.size >= predictedLabels.size))
//    val warning: String = checkAndSegmentReferences(lineSpans, predictedLabels, ret)
//    if (!(warning == "")) {
//      log.error(warning)
//    }
//    return ret
//  }
//
//  private var m_crf: CRF4 = null
//}
