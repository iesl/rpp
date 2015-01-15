package edu.umass.cs.rexo.ghuang.segmentation

import scala.List
import scala.collection.mutable
import org.rexo.extra.extract.Span

/**
 * Created by klimzaporojets on 9/25/14.
 */
object JournalSegmenter {
  private[segmentation] def getSegmenter(lineSpans: List[_]): JournalSegmenter = {
    import scala.collection.JavaConversions._
    for (curr <- journalSegmenters.entrySet) {
      if (curr.getKey.findAllIn(lineSpans.mkString).hasNext) {
        return curr.getValue
      }
    }
    return null
  }

  private[segmentation] var journalSegmenters: Map[scala.util.matching.Regex, JournalSegmenter] =
            Map[scala.util.matching.Regex, JournalSegmenter]()
}

abstract class JournalSegmenter {
  private[segmentation] def getAbstract(lineSpans: mutable.MutableList[_]): mutable.MutableList[Span]

  private[segmentation] def getBody(lineSpans: mutable.MutableList[_]): mutable.MutableList[_]

  private[segmentation] def getReferences(lineSpans: mutable.MutableList[_]): mutable.MutableList[_]
}


