package edu.umass.cs.rexo.ghuang.segmentation.svg

import org.rexo.extra.types.{TokenSequence, Token, Sequence}
import org.rexo.extra.extract.{Span, StringSpan}
import org.rexo.base.{Instance, Pipe}
import org.rexo.base.pipe.SerialPipes
import org.rexo.extraction.NewHtmlTokenizationSvg
import edu.umass.cs.rexo.ghuang.segmentation.{NewHtmlTokenization2LineInfoSvg, LineInfo2TokenSequenceV2}

/**
 * Use rules to segment references.
 *
 * @author kzaporojets: based on CRF segmentor (CRFBibliographySegmentor)
 *
 */
object RulesBibliographySegmenterSvg {
  private def checkAndSegmentReferences(lines: IndexedSeq[Span], predictedLabels: Sequence, result: RulesBibliographySegmenterSvg.ReferenceData): String = {
    var warning: String = ""
    var seenPrologue: Boolean = false
    var seenRef: Boolean = false
    var seenEpilogue: Boolean = false
    var seenJunk: Boolean = false
    var reference: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //LinkedList[_] = new LinkedList[_]
    var lineIdx: Int = 0
    var labIdx: Int = 0
    while (labIdx < predictedLabels.size && lineIdx < lines.size) {
      val tag: String = predictedLabels.get(labIdx).toString
      if (tag == "biblioPrologue") seenPrologue = true
      else if (tag == "post") seenEpilogue = true
      else if (tag.startsWith("biblio-")) seenRef = true
      else if (tag == "junk") seenJunk = true
      if (seenEpilogue && !seenPrologue) warning += "epilogue section before prologue section; "
      if (seenEpilogue && !seenRef) warning += "epilogue section before references; "
      if (!seenRef && seenJunk) warning += "junk line before the first reference; "
      val lineTok = lines(lineIdx)
      if (!(lineTok.isInstanceOf[StringSpan] && lineTok.asInstanceOf[StringSpan].getNumericProperty("isHeaderFooterLine") > 0)) {
        if (tag == "biblioPrologue") {
          result.prologueList += lines(lineIdx)
        }
        else if (tag == "post") {
          result.epilogueList += lines(lineIdx)
        }
        else if (tag == "biblio-B") {
          result.numReferences += 1
          if (reference.size > 0) result.referenceLineList += reference
          reference = collection.mutable.MutableList[Any]()
          reference += lines(lineIdx)
        }
        else if (tag == "biblio-I") {
          if (reference.size == 0) warning += "biblio-I not after biblio-B, line ignored; "
          else {
            reference += lines(lineIdx)
          }
        }
//        println(s"p3 (${predictedLabels.get(labIdx).toString}) :${lines(lineIdx).getText}")
        labIdx += 1
        lineIdx += 1
      }
      else{
//        println(s"p3 (${predictedLabels.get(labIdx).toString}) :${lines(lineIdx).getText}")
        lineIdx += 1
      }
    }
    if (reference.size > 0) {
      result.referenceLineList += reference
    }
    warning
  }

  class ReferenceData {
    private[segmentation] var prologueList = collection.mutable.MutableList[Span]()
    private[segmentation] var referenceLineList = collection.mutable.MutableList[Any]()
    private[segmentation] var epilogueList = collection.mutable.MutableList[Any]()
    private[segmentation] var predictedLabels: Sequence = null
    private[segmentation] var numReferences = 0
  }

}

class RulesBibliographySegmenterSvg {

  private def transformFromCrfToRulesPipe(): Pipe = {
    val pipes = collection.mutable.MutableList[Pipe]()
    pipes += new NewHtmlTokenization2LineInfoSvg
    val pli = new LineInfo2TokenSequenceV2
    pli.setTargetProcessing(false)
    pipes += pli
    val serialPipes = new SerialPipes(pipes)
    serialPipes
  }

  private def transduce(data: TokenSequence): Sequence = {
    val transducedData = new TokenSequence
    var label: String = null
      var i: Int = 0
      while (i < data.size) {
        val tkn: Token = data.get(i)
//          if (tkn.getText.toUpperCase.trim == "REFERENCES") {
        if (LayoutSegmentFinderSvg.BIBLIOGRAPHY_PATTERN.findFirstIn(tkn.getText).isDefined) {
          label = "biblioPrologue"
        }
        else if (tkn.getFeatures.hasProperty("possibleInit")) {
          label = "biblio-B"
        }
        else if (tkn.getFeatures.hasProperty("ignore")) {
//          println(s"adding label JUNK: ${tkn.getText}")
          label = "junk"
        }
        else {
          label = "biblio-I"
        }
        transducedData.add(label)
        i += 1
    }
    transducedData
  }

  def segmentReferences(htmlTokenization: NewHtmlTokenizationSvg): RulesBibliographySegmenterSvg.ReferenceData = {
    val rulesInputPipe: Pipe = transformFromCrfToRulesPipe()
    val inst: Instance = new Instance(htmlTokenization, null, null, null, rulesInputPipe)
    val predictedLabels = transduce(inst.getData().asInstanceOf[TokenSequence])
    htmlTokenization.setProperty("referenceFeatures", inst)
    val ret = new RulesBibliographySegmenterSvg.ReferenceData
    ret.predictedLabels = predictedLabels
    val warning: String = RulesBibliographySegmenterSvg.checkAndSegmentReferences(htmlTokenization.getLineSpans, predictedLabels, ret)
    println(warning)
    ret
  }
}
