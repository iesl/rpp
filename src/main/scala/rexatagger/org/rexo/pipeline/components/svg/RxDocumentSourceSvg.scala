package org.rexo.pipeline.components.svg

//import java.util.Iterator
import scala.Iterator
import org.rexo.pipeline.components.svg.RxDocumentSvg

/**
 * Created by klimzaporojets on 9/25/14.
 */
/**
 * @author adingle
 */
abstract trait RxDocumentSourceSvg {
  /**
   * Sets a limit on the number of documents for the iterator to return.
   */
  def setMaxDocuments(max: Long)

  /**
   * Returns an iterator of initialized RxDocuments.
   */
  def iterator: Iterator[_]

  /**
   * Performs any necessary extra processing to close the input associated with this RxDocument.
   */
  def closeDocument(rdoc: RxDocumentSvg)

  /**
   * Performs any necessary processing to close down this document source prior exiting the program (such as clearing any
   * locks held in the DB).
   *
   * @param lastDoc The last document processed.
   */
  def closeSource(lastDoc: RxDocumentSvg)

  /**
   * Performs any necessary processing to close down this document source prior exiting the program (such as clearing any
   * locks held in the DB).
   *
   * @param lastDoc The last document processed.
   */
  def closeSourceSvd(lastDoc: RxDocumentSvg)

}

