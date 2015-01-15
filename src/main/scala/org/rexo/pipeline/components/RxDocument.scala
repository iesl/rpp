package org.rexo.pipeline.components

import org.jdom2.input.SAXBuilder
import org.jdom2.{JDOMException, Document, Element}
import java.util.{LinkedList, HashMap}
import scala.collection.mutable

//import scala.Predef.Map
import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
import java.io.{IOException, StringReader}

/**
 * Created by klimzaporojets on 9/25/14.
 */
/**
 * @author asaunders
 */
object RxDocument {
//  def create: RxDocument = {
//    return new RxDocument()
//  }

  def apply():RxDocument = new RxDocument()

  def apply(document:Document):RxDocument = {
    val rxD = new RxDocument
    rxD.getScope("document").put("document", document)
    return rxD
  }
  /**
   * @param stringReader
   * @return
   */
  def create(stringReader: StringReader): RxDocument = {
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

class RxDocument {
//  this
  private var _scopeMap: scala.collection.mutable.Map[String, collection.mutable.Map[_,_]] =
    scala.collection.mutable.Map[String,collection.mutable.Map[_,_]]() //new HashMap[_, _]

  createScopes



  def getSource: Element = {
    throw new UnsupportedOperationException
  }

  def getDocument: Element = {
    val document: Document = getScope("document").get("document").asInstanceOf[Document]
    return document.getRootElement.asInstanceOf[Element]
  }

//  def this(document: Document) {
//    this()
//    createScopes
//    getScope("document").put("document", document)
//  }
//
//  def this()  {
//    this()
//    createScopes
//  }

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


//  def getTest():scala.collection.mutable.Map[Any, Any] =
//    { return null}
//
//  def consumeTest():Unit =
//  {
//    getTest().put("hello", "world")
//  }
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
    var errorList: collection.mutable.MutableList[String] = null
    errorList = getScope("document").get("error.list").getOrElse(null).asInstanceOf[mutable.MutableList[String]]
    if (errorList == null) {
      errorList = mutable.MutableList[String]() //new LinkedList[_]
      getScope("document").put("error.list", errorList)
    }
    errorList.+=(error)
//    var errorList: LinkedList[_] = null
//    errorList = getScope("document").get("error.list").asInstanceOf[LinkedList[_]]
//    if (errorList == null) {
//      errorList = new LinkedList[_]
//      getScope("document").put("error.list", errorList)
//    }
//    errorList.add(error)
  }

  /**
   * Adds a document info string, which may be processed later by another filter.
   */
  def docInfoString(info: String) {
    var infoList: collection.mutable.MutableList[String] = null //LinkedList[_] = null
    infoList = getScope("document").get("info.list").get.asInstanceOf[mutable.MutableList[String]] //[LinkedList[_]]
    if (infoList == null) {
      infoList = mutable.MutableList() //new LinkedList[_]
      getScope("document").put("info.list", infoList)
    }
    infoList.+=(info) //.add(info)
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

  def getTokenization: NewHtmlTokenization = {
    val tokenization: NewHtmlTokenization = getScope("document").get("tokenization").get.asInstanceOf[NewHtmlTokenization]
    return tokenization
  }

  def setTokenization(tokenization: NewHtmlTokenization) {
    getScope("document").put("tokenization", tokenization)
  }

//  def setTokenizationSvd(tokenization: NewHtmlTokenizationSvd) {
//    getScope("documentSvd").put("tokenization", tokenization)
//  }

  def getTokenization(whichTokenization: String): NewHtmlTokenization = {
    val tokenization: NewHtmlTokenization = getScope("document").get(whichTokenization + ".tokenization").asInstanceOf[NewHtmlTokenization]
    return tokenization
  }

  def setTokenization(whichTokenization: String, tokenization: NewHtmlTokenization) {
    getScope("document").put(whichTokenization + ".tokenization", tokenization)
  }

}
