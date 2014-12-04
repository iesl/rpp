package org.rexo.pipeline.extractors

import org.rexo.base.{Pipe, Instance}
import org.rexo.base.extract.{Extraction, Tokenization}

/**
 * Created by klimzaporojets on 10/2/14.
 */
class RulesExtractor /*extends Extractor*/ {
  private var featurePipe: Pipe = null
  private[extractors] var carrier: Instance = null

  def this(featurePipe: Pipe) {
    this()
    this.featurePipe = featurePipe
  }


  def getFeaturePipe: Pipe = {
    return featurePipe
  }




  def extract(spans: Tokenization): Extraction = {
    carrier = new Instance(spans, null, null, null, featurePipe)
    return null
  }

  def getCarrier: Instance = {
    return carrier
  }

}
