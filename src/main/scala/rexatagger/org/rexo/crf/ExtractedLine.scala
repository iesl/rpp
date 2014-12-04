package org.rexo.crf

/**
 * Created by klimzaporojets on 11/9/14.
 */
case class ExtractedLine(content: String, labelString: String) {
  val featureSeq: collection.mutable.ArrayBuffer[String] = collection.mutable.ArrayBuffer[String]()
  var coordinates: Any = null

}
