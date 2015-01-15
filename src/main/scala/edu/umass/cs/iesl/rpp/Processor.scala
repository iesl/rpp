package edu.umass.cs.iesl.rpp

import edu.umass.cs.iesl.xml_annotator.Annotator

trait Processor {
  def process(annotator: Annotator): Annotator
}
