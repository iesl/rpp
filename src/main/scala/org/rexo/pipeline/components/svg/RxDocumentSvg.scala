package org.rexo.pipeline.components.svg

import org.jdom2.input.SAXBuilder
import org.jdom2.{JDOMException, Document, Element}
import scala.collection.mutable

import org.rexo.extraction.NewHtmlTokenizationSvg
import java.io.{IOException, StringReader}


/**
 * @author asaunders
 */
object RxDocumentSvg {

  def apply():RxDocumentSvg = new RxDocumentSvg()

  def apply(document:Document):RxDocumentSvg = {
    val rxD = new RxDocumentSvg
    rxD.getScope("document").put("document", document)
    return rxD
  }
  /**
   * @param stringReader
   * @return
   */
  def create(stringReader: StringReader): RxDocumentSvg = {
    val builder: SAXBuilder = new SAXBuilder
    var doc: Document = null
    try {
      doc = builder.build(stringReader)
    }
    catch {
      case e: JDOMException => {
        e.printStackTrace
      }
      case e: IOException => {
        e.printStackTrace
      }
    }
    return apply(doc) //new RxDocument(doc)
  }
}

class RxDocumentSvg {
  private val _scopeMap = scala.collection.mutable.Map[String,collection.mutable.Map[_,_]]()

  createScopes

  def getSource: Element = {
    throw new UnsupportedOperationException
  }

  def getDocument: Element = {
    val document: Document = getScope("document").get("document").asInstanceOf[Document]
    document.getRootElement
  }

  protected def createScopes {
    setScope("document", scala.collection.mutable.Map() /*new HashMap[_, _]*/)
    setScope("session", scala.collection.mutable.Map() /*new HashMap[_, _]*/)
    setScope("global", scala.collection.mutable.Map() /*new HashMap[_, _]*/)
    getScope("document").put("collated.document", null)
    getScope("document").put("error.code.string", "none")
  }

  /**
   * @param scope
   * @return
   */
  def getScope(scope: String): scala.collection.mutable.Map[Any,Any] = {
    return _scopeMap.get(scope).get.asInstanceOf[scala.collection.mutable.Map[Any,Any]]
  }


  /**
   * @param scope
   * @param scopeMap
   */
  def setScope(scope: String, scopeMap: collection.mutable.Map[_, _]) {
    _scopeMap.remove(scope)
    _scopeMap.put(scope, scopeMap)
  }

  /**
   * Adds a document error string, which may be processed later by an error filter.
   */
  def docErrorString(error: String) {
    var errorList = getScope("document").get("error.list").getOrElse(null).asInstanceOf[mutable.MutableList[String]]
    if (errorList == null) {
      errorList = mutable.MutableList[String]()
      getScope("document").put("error.list", errorList)
    }
    errorList.+=(error)

  }

  /**
   * Adds a document info string, which may be processed later by another filter.
   */
  def docInfoString(info: String) {
    var infoList = getScope("document").get("info.list").get.asInstanceOf[mutable.MutableList[String]]
    if (infoList == null) {
      infoList = mutable.MutableList()
      getScope("document").put("info.list", infoList)
    }
    infoList.+=(info)
  }

  /**
   * Returns a String which names this document.  This will be the SHA1 hash, if available, or the name of the document from the filesystem.
   */
  def getId: String = {
    var docFname: String = getScope("document").get("corpus.fileSHA1").get.asInstanceOf[String]
    if (docFname == null) {
      docFname = getScope("document").get("file.basename").get.asInstanceOf[String]
    }
    if (docFname == null) {
      docFname = ""
    }
    return docFname
  }

  def getTokenization: NewHtmlTokenizationSvg = {
    val tokenization: NewHtmlTokenizationSvg = getScope("document").get("tokenization").get.asInstanceOf[NewHtmlTokenizationSvg]
    return tokenization
  }

  def setTokenization(tokenization: NewHtmlTokenizationSvg) {
    getScope("document").put("tokenization", tokenization)
  }

  def getTokenization(whichTokenization: String): NewHtmlTokenizationSvg = {
    val tokenization: NewHtmlTokenizationSvg = getScope("document").get(whichTokenization + ".tokenization").asInstanceOf[NewHtmlTokenizationSvg]
    return tokenization
  }

  def setTokenization(whichTokenization: String, tokenization: NewHtmlTokenizationSvg) {
    getScope("document").put(whichTokenization + ".tokenization", tokenization)
  }
}
