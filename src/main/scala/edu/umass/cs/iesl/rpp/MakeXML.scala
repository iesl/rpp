package edu.umass.cs.iesl.rpp


import edu.umass.cs.iesl.rpp.ReferencePartProcessor._
import edu.umass.cs.iesl.xml_annotator.Annotator
import org.jdom2.output.{Format, XMLOutputter}
import org.jdom2.{Document => JDOMDocument, Element}

/**
 * Methods for serializing RPP output to XML. 
 * Based on previous implementation in BatchMain.mkXML 
 */
object MakeXML {

  final val DOCUMENT_TAG = "document"
  final val HEADER_TAG = "header"
  final val AUTHORS_TAG = "authors"
  final val PERSON_TAG = "person"
  final val PERSON_FIRST_TAG = "person-first"
  final val PERSON_LAST_TAG = "person-last"
  final val REFERENCE_TAG = "reference"
  final val REFERENCES_TAG = "references"
  final val DATE_TAG = "date"
  final val VENUE_TAG = "venue"
  final val PARAGRAPHS_TAG = "paragraphs"
  final val PARA_TAG = "p"
  /**
   * Convert the annotator's markup to an XML string
   * @param annotator
   * @return
   */
  def mkXML(annotator: Annotator): String = {
    val docElement = new Element(DOCUMENT_TAG)
    docElement.addContent(mkHeaderXML(annotator))
    docElement.addContent(mkParagraphXML(annotator))
    docElement.addContent(mkReferenceXML(annotator))
    val doc = new JDOMDocument(docElement)
    val xmlOutput = new XMLOutputter()
    xmlOutput.setFormat(Format.getPrettyFormat)
    xmlOutput.outputString(doc)
  }

  /**
   * Convert the header information into XML
   * @param annotator
   * @return
   */
  def mkHeaderXML(implicit annotator: Annotator): Element = {
    import edu.umass.cs.iesl.rpp.HeaderPartProcessor._
    val headerElement = new Element(HEADER_TAG)
    annotator.getRangeSet("header").foreach(headerRange => {
      List(headerTitle, headerAffiliation, headerAddress, headerEmail, headerDate, headerAbstract).foreach(annoType => {
        val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(headerRange)
        val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
        annos.foreach(t => headerElement.addContent(new Element(normalTagHeader(annoType)).addContent(t.trim)))
      })
      val authorsElement = new Element(AUTHORS_TAG)
      val authorBIndexSet = annotator.getBIndexSetWithinRange(headerAuthor)(headerRange)
      authorBIndexSet.foreach(ai => {
        val personElement = new Element(PERSON_TAG)
        val tokens = annotator.getRange(headerAuthor)(ai).toList.flatMap(authorRange => {
          val tokenBIndexSet = annotator.getFilteredBIndexSetWithinRange(headerAuthor, headerToken)(authorRange)
          tokenBIndexSet.toList.flatMap(tokenIndex => annotator.getTextOption(headerToken)(tokenIndex).map(lineBreak))
        })
        val name = tokens.mkString(" ")
        val partsOption = cc.factorie.util.namejuggler.PersonNameParser.parseFullNameSafe(name)
        if (partsOption.isDefined) {
          val parts = partsOption.get
          personElement.addContent(new Element(PERSON_FIRST_TAG).addContent(parts.givenNames.mkString(" ")))
          personElement.addContent(new Element(PERSON_LAST_TAG).addContent(parts.surNames.mkString(" ")))
          authorsElement.addContent(personElement)
        }
      })
      headerElement.addContent(authorsElement)
    })
    headerElement
  }

  /**
   * Convert the reference information into XML
   * @param annotator
   * @return
   */
  def mkReferenceXML(implicit annotator: Annotator): Element = {
    val referencesElement = new Element(REFERENCES_TAG)
    annotator.getRangeSet("reference").foreach(refsRange => {
      val refBIndexSet = annotator.getBIndexSetWithinRange("biblio-marker")(refsRange)
      refBIndexSet.foreach(refIndex => {
        val referenceElement = new Element(REFERENCE_TAG)
        annotator.getRange("biblio-marker")(refIndex).foreach(refRange => {
          List(refTitleString, refMarkerString, referenceIdString).foreach(annoType => {
            val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(refRange)
            val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
            annos.foreach(t => referenceElement.addContent(new Element(normalTagReference(annoType)).addContent(t.trim)))
          })

          val dateBIndexSet = annotator.getBIndexSetWithinRange(refDateString)(refRange)
          dateBIndexSet.foreach { asi =>
            annotator.getRange(refDateString)(asi).foreach { dateRange =>
              val dateElement = new Element(DATE_TAG)
              List(refYearString, refMonthString).foreach { annoType =>
                val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(dateRange)
                val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
                annos.foreach(t => dateElement.addContent(new Element(normalTagReference(annoType)).addContent(t.trim)))
              }
              referenceElement.addContent(dateElement)
            }
          }

          val authorsBIndexSet = annotator.getBIndexSetWithinRange(refAuthorsString)(refRange)
          authorsBIndexSet.foreach(asi => {
            annotator.getRange(refAuthorsString)(asi).foreach(authorsRange => {
              val authorsElement = new Element(AUTHORS_TAG)
              val personBIndexSet = annotator.getBIndexSetWithinRange(refPersonString)(refRange)
              personBIndexSet.foreach(pi => {
                annotator.getRange(refPersonString)(pi).foreach(personRange => {
                  val personElement = new Element(PERSON_TAG)
                  List(refFirstString, refMiddleString, refLastString).foreach(annoType => {
                    val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(personRange)
                    val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
                    annos.foreach(t => personElement.addContent(new Element(normalTagReference(annoType)).addContent(t.trim)))
                  })
                  authorsElement.addContent(personElement)
                })
              })
              referenceElement.addContent(authorsElement)
            })
          })

          val venueBIndexSet = annotator.getBIndexSetWithinRange(refVenueString)(refRange)
          venueBIndexSet.foreach { asi =>
            annotator.getRange(refVenueString)(asi).foreach { venueRange =>
              val venueElement = new Element(VENUE_TAG)
              List(refJournalString, refBooktitleString, refOrganizationString, refAddressString, refVolumeString, refPagesString).foreach { annoType =>
                val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(venueRange)
                val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
                annos.foreach(t => venueElement.addContent(new Element(normalTagReference(annoType)).addContent(t.trim)))
              }
              referenceElement.addContent(venueElement)
            }
          }
        })
        referencesElement.addContent(referenceElement)
      })
    })
    referencesElement
  }


  /**
   * Convert the paragraph information into XML
   * @param annotator
   * @return
   */
  def mkParagraphXML(implicit annotator: Annotator): Element = {
    val parasElement = new Element(PARAGRAPHS_TAG)
    val annoType = "paragraph"
    annotator.getRangeSet(annoType).foreach(paraRange => {
      val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(paraRange)
      val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak)).take(1)
      val pElement = new Element(PARA_TAG)
      annos.foreach(t => pElement.addContent(t.trim))
      parasElement.addContent(pElement)
    })
    parasElement
  }
  
  
  private  def lineBreak(pair: (Int, String))(implicit annotator: Annotator) = {
    val (offset, text) = pair
    val lineBIndexSet = annotator.getBIndexSetByAnnotationType("line")
    Annotator.mkTextWithBreaks(text, lineBIndexSet.map(_ - offset), ' ')
  }
  private def normalTagHeader(s: String): String = if (s == "abstract") s else s.split("-").last
  
  private def normalTagReference(s: String): String = {
    val annoMap = Map(
      "ref-first" -> "person-first",
      "ref-last" -> "person-last"
    )
    if (annoMap.contains(s)) annoMap(s)
    else if (s.startsWith("ref-")) s.split("-").last
    else s
  }

}
