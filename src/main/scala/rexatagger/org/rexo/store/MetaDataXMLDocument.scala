package org.rexo.store

//import java.util.{Iterator, ArrayList, List}
//import scala.List
import org.rexo.extraction.NewHtmlTokenization

//import org.rexo.extraction.{HSpans, NewHtmlTokenization}
import org.jdom2.{Namespace, Element, Document, Text}
//import edu.umass.cs.mallet.base.util.PropertyList
import scala.Iterator
import org.jdom2.output.XMLOutputter
//import org.apache.log4j.Logger
//import org.rexo.exceptions.ForwardedException
//import org.apache.commons.collections.ListUtils
import java.util.regex.Pattern
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import java.io.{OutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import org.jdom2.input.SAXBuilder
//import org.rexo.exceptions.InitializationException
import org.jdom2.output.Format

/**
 * Created by klimzaporojets on 9/25/14.
 */
object MetaDataXMLDocument {
  def createFromTokenization(/*fileSHA1: SHA1Hash,*/ segmentations: Map[Any, Any]): MetaDataXMLDocument = {
    val referenceElements: List[Element] = segmentations.get("referenceElements").getOrElse(null).asInstanceOf[List[Element]]
    val bodyTokenization: NewHtmlTokenization = segmentations.get("bodyTokenization").getOrElse(null).asInstanceOf[NewHtmlTokenization]
    val referencesTokenization: NewHtmlTokenization = segmentations.get("referencesTokenization").getOrElse(null).asInstanceOf[NewHtmlTokenization]
    val grantsList: List[Element] = segmentations.get("grantList").getOrElse(null).asInstanceOf[List[Element]]
    val citationList: List[Element] = segmentations.get("citationList").getOrElse(null).asInstanceOf[List[Element]]
    val contentElement: Element = new Element("content")
//    val headerElement: Element = segmentations.get("headerElement").getOrElse(null).asInstanceOf[Element]
    val headerTokenization: NewHtmlTokenization = segmentations.get("headerTokenization").getOrElse(null).asInstanceOf[NewHtmlTokenization]
//    val newHdr: Element = headerElement.clone.asInstanceOf[Element]
//    contentElement.addContent(newHdr)
    //var bodyElement: Element = segmentations.get("bodyElement").getOrElse(null).asInstanceOf[Element]
//    if (bodyElement != null) {
//      val newBdy: Element = bodyElement.clone.asInstanceOf[Element]
//      contentElement.addContent(newBdy)
//    }
//    else {

    val headerElement:Element = new Element("header")
    val headerText: String = headerTokenization.getFormattedText
    headerElement.setText(headerText)
    contentElement.addContent(headerElement)

    val bodyElement = new Element("body")
    val bodyText: String = bodyTokenization.getFormattedText
    bodyElement.setText(bodyText)
    contentElement.addContent(bodyElement)



    val referencesElement:Element = new Element("references")
    val referencesText:String = referencesTokenization.getFormattedText
    referencesElement.setText(referencesText)
    contentElement.addContent(referencesElement)


//    val headerElement:Element = new Element("header")
//    val headerText: String = headerTokenization.getFormattedText
//    headerElement.setText(headerText)
//    contentElement.addContent(headerElement)

//    }
//    val biblioElement: Element = new Element("biblio")
////    {
//      var i: Int = 0
//      while (i < referenceElements.size) {
//        {
//          val referenceElement: Element = referenceElements(i) //referenceElements.get(i).asInstanceOf[Element]
//          biblioElement.addContent(referenceElement.clone)
////          biblioElement.addContent(new Element("test", Namespace.getNamespace("str", "uri")))
//
//        }
//        ({
//          i += 1; i - 1
//        })
//      }
////    }
//    contentElement.addContent(biblioElement)
//    val grantElement: Element = new Element("grants")
//    if (grantsList != null) {
//      {
//        var i: Int = 0
//        while (i < grantsList.size) {
//          {
//            val grantContent: Element = grantsList(i) //grantsList.get(i).asInstanceOf[Element]
//            grantElement.addContent(grantContent.clone.asInstanceOf[Element])
//          }
//          ({
//            i += 1; i - 1
//          })
//        }
//      }
//    }
//    val citationsElement: Element = new Element("CitationContexts")
//    if (citationList != null) {
//      {
//        var i: Int = 0
//        while (i < citationList.size) {
//          {
//            val cicElement: Element = citationList(i) //citationList.get(i).asInstanceOf[Element]
//            citationsElement.addContent(cicElement.clone.asInstanceOf[Element])
//          }
//          ({
//            i += 1; i - 1
//          })
//        }
//      }
//    }
    val rootElement: Element = new Element("document")
    rootElement.addContent(contentElement)
//    rootElement.addContent(citationsElement)
//    rootElement.addContent(grantElement)
    val document: Document = new Document(rootElement)
    val newMetaDocument: MetaDataXMLDocument = new MetaDataXMLDocument(document)
    return newMetaDocument
  }

//  private def createJdomFromHSpan(span: HSpans): Element = {
//    val spanChildren: ArrayList[_] = span.getChildren
//    val spanName: String = span.getName
//    val spanElement: Element = new Element(spanName)
//    val properties: PropertyList = span.getProperties
//    if (properties != null) {
//      val propIter: PropertyList#Iterator = properties.iterator
//      while (propIter.hasNext) {
//        propIter.next
//        val key: String = propIter.getKey
//        val oValue: AnyRef = propIter.getObjectValue
//        if (oValue != null) {
//          spanElement.setAttribute(key, oValue.toString)
//        }
//      }
//    }
//    if (spanChildren.isEmpty) {
//      val spanText: String = span.getText
//      span.getMultiSpan
//      spanElement.setText(spanText)
//    }
//    else {
//      val iterator: Iterator[_] = spanChildren.iterator
//      while (iterator.hasNext) {
//        val childSpan: HSpans = iterator.next.asInstanceOf[HSpans]
//        spanElement.addContent(createJdomFromHSpan(childSpan))
//      }
//    }
//    return spanElement
//  }

//  private def dumpDocument(metaDataXMLDocument: MetaDataXMLDocument) {
//    try {
//      val headersElement: Element = metaDataXMLDocument.getHeadersElement
//      printElement(headersElement)
//      val biblioElement: Element = metaDataXMLDocument.getBiblioElement
//      printElement(biblioElement)
//      val bodyElement: Element = metaDataXMLDocument.getBodyElement
//      printElement(bodyElement)
//      val bodyText: String = metaDataXMLDocument.getBodyText
//      System.out.println("bodyText: ========================")
//      System.out.println(bodyText)
//      System.out.println("++++++++++++++++++++++++++")
//    }
//    catch {
//      case e: Exception => {
//        e.printStackTrace
//      }
//    }
//  }
//
//  private def printElement(element: Element) {
//    val xmlOutputter: XMLOutputter = new XMLOutputter(org.jdom.output.Format.getCompactFormat)
//    xmlOutputter.output(element, System.out)
//  }
//
////  private var log: Logger = Logger.getLogger(classOf[MetaDataXMLDocument])
//
//  /**
//    */
//  class IncorrectXMLStructureException extends ForwardedException {
//    def this(msg: String) {
//      this()
//      `super`(msg)
//    }
//
//    def this(cause: Throwable) {
//      this()
//      `super`(cause)
//    }
//
//    def this(msg: String, cause: Throwable) {
//      this()
//      `super`(msg, cause)
//    }
//  }
//
//  class NoSuchElementException extends Exception {
//    def this(msg: String) {
//      this()
//      `super`(msg)
//    }
//
//    def this(cause: Throwable) {
//      this()
//      `super`(cause)
//    }
//
//    def this(msg: String, cause: Throwable) {
//      this()
//      `super`(msg, cause)
//    }
//  }
//
}
//
class MetaDataXMLDocument {
//  def getHeadersElement: Element = {
//    try {
//      return _document.getRootElement.getChild("content").getChild("headers")
//    }
//    catch {
//      case e: NullPointerException => {
//        throw new MetaDataXMLDocument.NoSuchElementException("headers")
//      }
//    }
//  }
//
//  def getBiblioElement: Element = {
//    try {
//      return _document.getRootElement.getChild("content").getChild("biblio")
//    }
//    catch {
//      case e: NullPointerException => {
//        throw new MetaDataXMLDocument.NoSuchElementException("biblio")
//      }
//    }
//  }
//
//  def getBodyElement: Element = {
//    try {
//      return _document.getRootElement.getChild("content").getChild("body")
//    }
//    catch {
//      case e: NullPointerException => {
//        throw new MetaDataXMLDocument.NoSuchElementException("body")
//      }
//    }
//  }
//
//  private def getGrantNumbers: List[_] = {
//    return getGrantNumbersCheesyMethod
//  }
//
//  private def getGrantNumbersCheesyMethod: List[_] = {
//    try {
//      val descendants: Iterator[_] = _document.getRootElement.getChild("grants").getDescendants(new Filter {
//        def matches(o: AnyRef): Boolean = {
//          if (o.isInstanceOf[Element]) {
//            val subElement: Element = o.asInstanceOf[Element]
//            if (subElement.getName == "grant-number") {
//              return true
//            }
//          }
//          return false
//        }
//      })
//      val nsfGrantNumbers: ArrayList[_] = new ArrayList[_]
//      while (descendants.hasNext) {
//        val grantNumberElement: Element = descendants.next.asInstanceOf[Element]
//        val text: String = grantNumberElement.getText
//        val grantNumber: String = cleanNSFGrantNumber(text)
//        nsfGrantNumbers.add(grantNumber)
//      }
//      return nsfGrantNumbers
//    }
//    catch {
//      case e: NullPointerException => {
//        return ListUtils.EMPTY_LIST
//      }
//    }
//  }
//
//  private def getGrantNumbersRobustMethod: List[_] = {
//    try {
//      val descendants: Iterator[_] = _document.getRootElement.getChild("grants").getDescendants(new Filter {
//        def matches(o: AnyRef): Boolean = {
//          if (o.isInstanceOf[Element]) {
//            val subElement: Element = o.asInstanceOf[Element]
//            if (subElement.getName == "grant-number") {
//              return true
//            }
//          }
//          return false
//        }
//      })
//      var foundNSF: Boolean = false
//      val nsfGrantNumbers: ArrayList[_] = new ArrayList[_]
//      while (descendants.hasNext) {
//        val grantRecElement: Element = descendants.next.asInstanceOf[Element]
//        val text: String = grantRecElement.getText
//        if (grantRecElement.getName == "grant-institution") {
//          if (isNSFInstitution(text)) {
//            foundNSF = true
//          }
//        }
//        else {
//          if (foundNSF) {
//            val grantNumber: String = cleanNSFGrantNumber(text)
//            if (isNSFGrantNumber(grantNumber)) {
//              nsfGrantNumbers.add(grantNumber)
//            }
//          }
//        }
//      }
//      return nsfGrantNumbers
//    }
//    catch {
//      case e: NullPointerException => {
//        return ListUtils.EMPTY_LIST
//      }
//    }
//  }
//
//  private def isNSFGrantNumber(text: String): Boolean = {
//    val pattern: Pattern = Pattern.compile("\\d{7}")
//    return pattern.matcher(text).find
//  }
//
//  private def cleanNSFGrantNumber(text: String): String = {
//    val cleaned: String = text.replaceAll("[^\\d]", "")
//    return cleaned
//  }
//
//  private def isNSFInstitution(text: String): Boolean = {
//    {
//      var i: Int = 0
//      while (i < NSF_INST_PATTERNS.length) {
//        {
//          val pattern: Pattern = NSF_INST_PATTERNS(i)
//          if (pattern.matcher(text).find) {
//            return true
//          }
//        }
//        ({
//          i += 1; i - 1
//        })
//      }
//    }
//    return false
//  }
//
//  /**
//    */
//  def getBodyText: String = {
//    try {
//      val bodyElement: Element = getBodyElement
//      return getNormalizedChildText(bodyElement)
//    }
//    catch {
//      case e: MetaDataXMLDocument.NoSuchElementException => {
//        throw new MetaDataXMLDocument.IncorrectXMLStructureException(e)
//      }
//    }
//  }
//
  def this(document: Document) {
    this()
    _document = document
  }
//
//  /**
//   * @param gzipCompressedBytes
//   */
//  def this(gzipCompressedBytes: Array[Byte]) {
//    this()
//    try {
//      var inputStream: GZIPInputStream = null
//      inputStream = new GZIPInputStream(new ByteArrayInputStream(gzipCompressedBytes))
//      val saxBuilder: SAXBuilder = new SAXBuilder
//      _document = saxBuilder.build(inputStream)
//    }
//    catch {
//      case e: IOException => {
//        throw new InitializationException(e)
//      }
//      case e: JDOMException => {
//        throw new InitializationException(e)
//      }
//    }
//  }
//
//  /**
//   * @param inputStream
//   */
//  def this(inputStream: InputStream) {
//    this()
//    try {
//      val saxBuilder: SAXBuilder = new SAXBuilder
//      _document = saxBuilder.build(inputStream)
//    }
//    catch {
//      case e: IOException => {
//        throw new InitializationException(e)
//      }
//      case e: JDOMException => {
//        throw new InitializationException(e)
//      }
//    }
//  }
//
  def getDocument: Document = {
    return _document
  }
//
//  /**
//    */
//  def toGZippedXML: Array[Byte] = {
//    val output: XMLOutputter = new XMLOutputter(Format.getRawFormat)
//    val bos: ByteArrayOutputStream = new ByteArrayOutputStream
//    val xmlOutputStream: OutputStream = new GZIPOutputStream(bos)
//    try {
//      output.output(_document, xmlOutputStream)
//    }
//    catch {
//      case e: IOException => {
//      }
//    }
//    return bos.toByteArray
//  }
//
//  private def getNormalizedChildText(parent: Element): String = {
//    val stringBuffer: StringBuffer = new StringBuffer
//    val iterator: Iterator[_] = parent.getDescendants(new Filter {
//      def matches(o: AnyRef): Boolean = {
//        return o.isInstanceOf[Text]
//      }
//    })
//    while (iterator.hasNext) {
//      val text: Text = iterator.next.asInstanceOf[Text]
//      stringBuffer.append(text.getTextNormalize).append(" ")
//    }
//    return stringBuffer.toString
//  }
//
  private var _document: Document = null
//  private var NSF_INST_PATTERNS: Array[Pattern] = Array[Pattern](Pattern.compile("NSF"), Pattern.compile("National\\s+Science\\s+Foundation", Pattern.CASE_INSENSITIVE))
}
