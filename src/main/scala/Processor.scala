package rpp 

import annotator.Annotator 

trait Processor {
  def process(annotator: Annotator): Annotator
}
