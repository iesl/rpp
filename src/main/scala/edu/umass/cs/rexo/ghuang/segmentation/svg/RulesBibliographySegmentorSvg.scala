package edu.umass.cs.rexo.ghuang.segmentation.svg

import org.rexo.extra.types.{TokenSequence, Token, Sequence}
import org.rexo.extra.extract.{Span, StringSpan}
import org.rexo.base.{Instance, Pipe}
import org.rexo.base.pipe.SerialPipes
import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
import edu.umass.cs.rexo.ghuang.segmentation.{NewHtmlTokenization2LineInfoSvg, NewHtmlTokenization2LineInfo, LineInfo2TokenSequenceV2}
import scala.collection.mutable

/**
 * Created by klimzaporojets on 10/3/14.
 */
/**
 * Use rules to segment references.
 *
 * @author kzaporojets: based on CRF segmentor (CRFBibliographySegmentor)
 *
 */
object RulesBibliographySegmentorSvg {
  private def checkAndSegmentReferences(lines: mutable.MutableList[Span], predictedLabels: Sequence, result: RulesBibliographySegmentorSvg.ReferenceData): String = {
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
      val lineTok: Any = lines(lineIdx)
      if (!((lineTok.isInstanceOf[StringSpan]) && (lineTok.asInstanceOf[StringSpan]).getNumericProperty("isHeaderFooterLine") > 0)) {
        if (tag == "biblioPrologue") {
          result.prologueList.+=(lines(lineIdx))
        }
        else if (tag == "post") {
          result.epilogueList.+=(lines(lineIdx))
        }
        else if (tag == "biblio-B") {
          result.numReferences += 1
          if (reference.size > 0) result.referenceLineList.+=(reference)
          reference = collection.mutable.MutableList[Any]()
          reference.+=(lines(lineIdx))
        }
        else if (tag == "biblio-I") {
          if (reference.size == 0) warning += "biblio-I not after biblio-B, line ignored; "
          else {
            reference.+=(lines(lineIdx))
          }
        }
        labIdx += 1
        lineIdx += 1
      }
      else
      {
        //what goes before continue
        lineIdx += 1
      }
    }
    if (reference.size > 0) {
      result.referenceLineList.+=(reference)
    }
    return warning
  }

  class ReferenceData {
    private[segmentation] var prologueList: collection.mutable.MutableList[Span] = collection.mutable.MutableList[Span]() //LinkedList[_] = new LinkedList[_]
    private[segmentation] var referenceLineList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //LinkedList[_] = new LinkedList[_]
    private[segmentation] var epilogueList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //LinkedList[_] = new LinkedList[_]
    private[segmentation] var predictedLabels:Sequence = null
    private[segmentation] var numReferences: Int = 0
  }

}

class RulesBibliographySegmentorSvg {

  private def transformFromCrfToRulesPipe(): Pipe = {
    val pipes: collection.mutable.MutableList[Pipe] = collection.mutable.MutableList[Pipe]()
    pipes.+=(new NewHtmlTokenization2LineInfoSvg)
    val pli: Pipe = new LineInfo2TokenSequenceV2
    pli.setTargetProcessing(false)
    pipes.+=(pli)
    val serialPipes: SerialPipes = new SerialPipes(pipes)
    return serialPipes
  }

  private def transduce(data: Sequence): Sequence = {
    val transducedData: Sequence = new TokenSequence
    val iter: Iterator[_] = (data.asInstanceOf[TokenSequence]).iterator
    var label: String = null
      var i: Int = 0
      while (i < (data.asInstanceOf[TokenSequence]).size) {
        {
          val tkn: Token = ((data.asInstanceOf[TokenSequence]).get(i)).asInstanceOf[Token]
          if (tkn.getText.toUpperCase.trim == "REFERENCES") {
            label = "biblioPrologue"
          }
          else if (tkn.getFeatures.hasProperty("possibleInit")) {
            label = "biblio-B"
          }
          else if (tkn.getFeatures.hasProperty("ignore")) {
            label = "junk"
          }
          else {
            label = "biblio-I"
          }
          (transducedData.asInstanceOf[TokenSequence]).add(label)
        }
        ({
          i += 1; i - 1
        })
//      }
    }
    return transducedData
  }

  def segmentReferences(htmlTokenization: NewHtmlTokenizationSvg): RulesBibliographySegmentorSvg.ReferenceData = {
    val rulesInputPipe: Pipe = transformFromCrfToRulesPipe()
    val inst: Instance = new Instance(htmlTokenization, null, null, null, rulesInputPipe)
    val predictedLabels: Sequence = transduce(inst.getData.asInstanceOf[Sequence])
    val ret: RulesBibliographySegmentorSvg.ReferenceData = new RulesBibliographySegmentorSvg.ReferenceData
    val lineSpans: collection.mutable.MutableList[Span] = collection.mutable.MutableList[Span]()
    lineSpans.++=(htmlTokenization.getLineSpans)
    assert((lineSpans.size >= predictedLabels.size))
    ret.predictedLabels = predictedLabels
    val warning: String = RulesBibliographySegmentorSvg.checkAndSegmentReferences(lineSpans, predictedLabels, ret)
    return ret
  }
}
