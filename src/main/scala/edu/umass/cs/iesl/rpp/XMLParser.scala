package edu.umass.cs.iesl.rpp

/**
 * @author Kate Silverstein 
 *         created on 4/1/15
 */
import cc.factorie.app.nlp._
import cc.factorie.util.namejuggler
import edu.umass.cs.iesl.bibie._
import edu.umass.cs.iesl.paperheader.crf.HeaderTagSpanBuffer
import scala.collection.mutable.{ArrayBuffer, Stack, HashMap}

class ReferenceField(val tag: String, val subfields: Set[String], val isTerminal: Boolean = false) {
  //  val isTerminal: Boolean = false
  val strings = new ArrayBuffer[String]()
  val subfieldMap: Map[String, ArrayBuffer[ReferenceField]] = subfields.map(fieldname => (fieldname, new ArrayBuffer[ReferenceField]())).toMap
  def +=(s: String): Unit = if (isTerminal) strings += s else throw new Exception("non-terminals dont have strings, yet you are trying to add one. why?")
  def field(fieldname: String): Option[ArrayBuffer[ReferenceField]] = subfieldMap.get(fieldname)
  override def toString: String = {
    if (isTerminal) s"<$tag>${strings.mkString(" ")}</$tag>"
    else
      s"<$tag>" +
        subfieldMap
          .filter { case (_, buff) => buff.nonEmpty }
          .map { case (_, buff) => buff.map(_.toString).mkString("\n") }
          .mkString(" ") +
        s"</$tag>"
  }
}
class Title extends ReferenceField("title", Set(), isTerminal=true)
class Authors extends ReferenceField("authors", Set("person"))
class Person extends ReferenceField("person", Set("person-first", "person-last"))
class PersonX(subtag: String) extends ReferenceField("person-"+subtag, Set(), isTerminal=true)
class Venue extends ReferenceField("venue", Set("address", "booktitle", "date"))
class Date extends ReferenceField("date", Set("year"))
class Year extends ReferenceField("year", Set(), isTerminal=true)
class Booktitle extends ReferenceField("booktitle", Set(), isTerminal=true)
class Reference extends ReferenceField("reference", Set("title", "authors", "venue", "ref-marker", "date"))

object XMLParser {
  /**
   * Converts a headerDoc (a document with an attr[paper_header.HeaderTagSpanBuffer]) and a sequence of "reference" documents (where each token
   * has an attr[bibie.CitationLabel]) to a string of proper XML. See BatchMain for usage.
   * @param headerDoc - a FACTORIE document that has been processed using paper-header
   * @param refs - FACTORIE documents that have been processed using bibie
   * @return
   */
  def docsToXML(headerDoc: Document, refs: Seq[Document]): String = {
    val stuff = new ArrayBuffer[String]()
    stuff += "<document>"
    if (headerDoc.attr.contains(classOf[HeaderTagSpanBuffer])) {
      val headerAuthors = new ArrayBuffer[String]()
      val hb = headerDoc.attr[HeaderTagSpanBuffer]
      stuff += "<header>"
      for (span <- hb) {
        val label = span.label.categoryValue
        if (label == "author") {
          // TODO replace this namejuggler stuff with the CRF!
          val name = span.tokens.map(_.string).mkString(" ")
          val thing: namejuggler.PersonName = namejuggler.PersonNameParser.parseFullName(name)
          val lastName = thing.surNames.mkString(" ")
          val firstName = thing.givenNames.mkString(" ")
          headerAuthors += s"<person><person-first>$firstName</person-first><person-last>$lastName</person-last></person>"
        } else {
          val contents = span.tokens.map(_.string).mkString(" ")
          stuff += s"<$label>$contents</$label>"
        }
      }
      if (headerAuthors.nonEmpty) {
        stuff += "<authors>"
        stuff ++= headerAuthors
        stuff += "</authors>"
      }
      stuff += "</header>"
    }
    val refxml: Seq[String] = for (ref <- refs) yield {
      var str: String = ""
      try {
        str = XMLParser.fromBibieReferenceDocument(ref)
      } catch {
        case e: Exception => e.printStackTrace()
      }
      str
    }
    val refxmlFilt = refxml.filter(s => s.length > 0)
    if (refxmlFilt.nonEmpty) {
      stuff += "<references>"
      stuff ++= refxmlFilt
      stuff += "</references>"
    }
    stuff += "</document>"
    stuff.mkString("\n")
  }

  // TODO this probably shouldn't live here -- it should live in the bibie codebase
  // TODO add in the rest of the possible annotations from bibie (e.g. "journal")
  def fromBibieReferenceDocument(doc: Document): String = {
    val ref = new Reference
    val tokens = doc.tokens.toIndexedSeq

    // pointers to current chunks (there's probably a better way)
    var title: Title = null
    var author: Authors = null
    var person: Person = null
    var personX: PersonX = null
    var venue: Venue = null
    var date: Date = null
    var year: Year = null
    var booktitle: Booktitle = null

    for (token <- tokens) {
//      println(s"${token.string}\t${token.attr[CitationLabel].categoryValue}")
      val tags = token.attr[CitationLabel].categoryValue.split(":")
      for (tag <- tags) {
        val prefix = tag.take(1)
        val base = tag.drop(2)
        base match {
          case "title" =>
            prefix match {
              case "B" =>
                ref.field(base) match {
                  case Some(buff) =>
                    buff += new Title
                    buff.last += token.string
                    title = buff.last.asInstanceOf[Title]
                  case None =>
                }
              case "I" => title += token.string
            }
          case "authors" =>
            prefix match {
              case "B" =>
                ref.field(base) match {
                  case Some(buff) =>
                    buff += new Authors
                    author = buff.last.asInstanceOf[Authors]
                  case None =>
                }
              case _ =>
            }
          case "person" =>
            prefix match {
              case "B" =>
                author.field(base) match {
                  case Some(buff) =>
                    buff += new Person
                    person = buff.last.asInstanceOf[Person]
                  case None =>
                }
              case _ =>
            }
          case "person-last" | "person-first" =>
            prefix match {
              case "B" =>
                person.field(base) match {
                  case Some(buff) =>
                    val t = base.split("-")(1)
                    buff += new PersonX(t)
                    buff.last += token.string
                    personX = buff.last.asInstanceOf[PersonX]
                  case _ =>
                }
              case "I" => personX += token.string
            }
          case "venue" =>
            prefix match {
              case "B" =>
                ref.field(base) match {
                  case Some(buff) =>
                    buff += new Venue
                    venue = buff.last.asInstanceOf[Venue]
                  case None =>
                }
              case _ =>
            }
          case "date" =>
            prefix match {
              case "B" =>
                ref.field(base) match { // FIXME I think "date" should always be within a "venue"? need to doublecheck, if so this should be an error
                  case Some(buff) =>
                    buff += new Date
                    date = buff.last.asInstanceOf[Date]
                  case None => if (venue != null) venue.field(base) match {
                    case Some(buff) =>
                      buff += new Date
                      date = buff.last.asInstanceOf[Date]
                    case None =>
                  }
                }
              case _ =>
            }
          case "year" =>
            prefix match {
              case "B" =>
                date.field(base) match {
                  case Some(buff) =>
                    buff += new Year
                    buff.last += token.string
                    year = buff.last.asInstanceOf[Year]
                  case None =>
                }
              case "I" => year += token.string
            }
          case "booktitle" =>
            prefix match {
              case "B" =>
                venue.field(base) match {
                  case Some(buff) =>
                    buff += new Booktitle
                    buff.last += token.string
                    booktitle = buff.last.asInstanceOf[Booktitle]
                  case None =>
                }
              case "I" => booktitle += token.string
            }
          case _ => //println("not yet supported: " + token.attr[CitationLabel].categoryValue)
        }
      }
    }
    ref.toString
  }

  def main(args: Array[String]): Unit = {
    val docString =
      """
        |Ann	B-authors:B-person:B-person-last
        |Taylor	I-authors:I-person:I-person-last
        |,	I-authors:I-person:I-person-last
        |Mitchel	I-authors:B-person:B-person-first
        |Marcus	I-authors:I-person:B-person-last
        |,	I-authors:I-person:I-person-last
        |and	I-authors
        |Beatrice	I-authors:B-person:B-person-first
        |Santorini	I-authors:I-person:B-person-last
        |.	I-authors:I-person:I-person-last
        |2003	B-date:B-year
        |.	I-date:I-year
        |The	B-title
        |Penn	I-title
        |treebank	I-title
        |:	I-title
        |an	I-title
        |overview	I-title
        |.	I-title
        |In	B-venue:B-booktitle
        |Abeill	I-venue:I-booktitle
        |´	I-venue:I-booktitle
        |e	I-venue:I-booktitle
        |(	I-venue:I-booktitle
        |2003	I-venue:I-booktitle
        |)	I-venue:I-booktitle
        |,	I-venue:I-booktitle
        |chapter	I-venue:B-chapter
        |1	I-venue:B-volume
        |,	I-venue:I-volume
        |pages	I-venue:B-pages
        |5	I-venue:I-pages
        |–	I-venue:I-pages
        |22	I-venue:I-pages
        |.	I-venue:I-pages
        |
      """.stripMargin
    val doc = new Document("")
    docString.split("\n").foreach { line =>
      val parts = line.split("\t")
      if (parts.length == 2) {
        val token = new Token(doc, parts(0))
        token.attr += new CitationLabel(parts(1), token)
      }
    }
    val xml = fromBibieReferenceDocument(doc)
    println(xml)
  }

}


//class Field(v: Any) {
//  val value: Any = v
//}
//class Authors extends Field {
//  val person = new ArrayBuffer[Person]()
//  override val value = "authors"
//  override def toString = "<authors>" + person.map(_.toString).mkString("\n") + "</authors>"
//}
//class Person extends Field {
//  var personFirst: PersonFirst = null
//  var personLast: PersonLast = null
//  override val value = "person"
//  override def toString: String = {
//    var s = "<person>"
//    if (personLast != null) s += personLast.toString + "\n"
//    if (personFirst != null) s += personFirst.toString + "\n"
//    s += "</person>"
//    s
//  }
//}
//class PersonFirst extends Field {
//  val strings = new ArrayBuffer[String]()
//  override val value = "person-first"
//  override def toString = "<person-first>" + strings.mkString(" ") + "</person-first>"
//}
//class PersonLast extends Field {
//  val strings = new ArrayBuffer[String]()
//  override val value = "person-last"
//  override def toString = "<person-last>" + strings.mkString(" ") + "</person-last>"
//}
//class RefMarker extends Field {
//  val strings = new ArrayBuffer[String]()
//  override val value = "refmarker"
//  override def toString = "<ref-marker>" + strings.mkString(" ") + "</ref-marker>"
//}
//class Title extends Field {
//  val strings = new ArrayBuffer[String]
//  override def toString = "<title>" + strings.mkString(" ") + "</title>"
//}
//class Venue extends Field {
//  var booktitle: Booktitle = null
//  var pages = new ArrayBuffer[Pages]()
//  var date: Date = null
//  var journal = new ArrayBuffer[Journal]()
//  var address: Address = null
//  var volume: Volume = null
//  val organization = new ArrayBuffer[Organization]()
//  val chapter = new ArrayBuffer[Chapter]()
//  val number = new ArrayBuffer[Number]()
//  val publisher = new ArrayBuffer[Publisher]()
//  override def toString: String = {
//    var s = "<venue>"
//    if (booktitle != null) s += booktitle.toString + "\n"
//    if (pages.nonEmpty) s += pages.map(_.toString).mkString("\n") + "\n"
//    if (date != null) s += date.toString + "\n"
//    if (journal.nonEmpty) s += journal.map(_.toString).mkString("\n") + "\n"
//    if (volume != null) s += volume.toString + "\n"
//    if (address != null) s += address.toString + "\n"
//    if (organization.nonEmpty) s += organization.map(_.toString).mkString("\n") + "\n"
//    if (chapter.nonEmpty) s += chapter.map(_.toString).mkString("\n") + "\n"
//    if (number.nonEmpty) s += number.map(_.toString).mkString("\n") + "\n"
//    if (publisher.nonEmpty) s += publisher.map(_.toString).mkString("\n") + "\n"
//    s += "</venue>"
//    s
//  }
//}
////Association	I-venue:B-organization
//class Organization extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<organization>" + strings.mkString(" ") + "</organization>"
//}
//
////the	I-venue:I-chapter
//class Chapter extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<chapter>" + strings.mkString(" ") + "</chapter>"
//}
//
////2	I-venue:B-number
//class Number extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<number>" + strings.mkString(" ") + "</number>"
//}
//
////Tsinghua	B-venue:B-publisher
//class Publisher extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<publisher>" + strings.mkString(" ") + "</publisher>"
//}
//
//class Address extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<address>" + strings.mkString(" ") + "</address>"
//}
//class Booktitle extends Field {
//  val strings = new ArrayBuffer[String]
//  override def toString = "<booktitle>" + strings.mkString(" ") + "</booktitle>"
//}
//class Journal extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<journal>" + strings.mkString(" ") + "</journal>"
//}
//class Volume extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<volume>" + strings.mkString(" ") + "</volume>"
//}
//class Pages extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<pages>" + strings.mkString(" ") + "</pages>"
//}
//class Date extends Field {
//  var year: Year = null
//  override def toString: String = {
//    var s = "<date>\n"
//    if (year != null) s += year.toString + "\n"
//    s += "</date>"
//    s
//  }
//}
//class Year extends Field {
//  val strings = new ArrayBuffer[String]()
//  override def toString = "<year>" + strings.mkString(" ") + "</year>"
//}
//
//class Reference {
//  val authors = new ArrayBuffer[Authors]()
//  var refMarker: RefMarker = null
//  var title: Title = null
//  var venues = new ArrayBuffer[Venue]()
//  override def toString: String = {
//    var s = "<reference>"
//    if (refMarker != null) s += refMarker.toString + "\n"
//    if (authors.nonEmpty) s += authors.map(_.toString).mkString("\n") + "\n"
//    if (title != null) s += title.toString + "\n"
//    if (venues.nonEmpty) s += venues.map(_.toString).mkString("\n") + "\n"
//    s += "</reference>"
//    s
//  }
//}
//
//object XMLParser {
//  def docsToXML(headerDoc: Document, refs: Seq[Document]): String = {
//    val stuff = new ArrayBuffer[String]()
//    stuff += "<document>"
//    if (headerDoc.attr.contains(classOf[HeaderTagSpanBuffer])) {
//      val hb = headerDoc.attr[HeaderTagSpanBuffer]
//      stuff += "<header>"
//      for (span <- hb) {
//        val label = span.label.categoryValue
//        val contents = span.tokens.map(_.string).mkString(" ")
//        stuff += s"<$label>$contents</$label>"
//      }
//      stuff += "</header>"
//    }
//    stuff += "<references>"
//    for (ref <- refs) stuff += XMLParser.fromBibieReferenceDocument(ref)
//    stuff += "</references>"
//    stuff += "</document>"
//    stuff.mkString("\n")
//  }
//  def fromBibieReferenceDocument(doc: Document): String = {
//    val ref = new Reference
//    val tokens = doc.tokens.toIndexedSeq
//
//    var author: Authors = null
//    var person: Person = null
//    var refmarker: RefMarker = null
//    var title: Title = null
//    var venue: Venue = null
//    var booktitle: Booktitle = null
//    var pages: Pages = null
//    var date: Date = null
//    var year: Year = null
//    var journal: Journal = null
//    var organization: Organization = null
//    var chapter: Chapter = null
//    var number: Number = null
//    var volume: Volume = null
//    var address: Address = null
//    var publisher: Publisher = null
//
//    for (token <- tokens) {
//      val tags = token.attr[CitationLabel].categoryValue.split(":")
//      for (tag <- tags) {
//        val prefix = tag.take(1)
//        val base = tag.drop(2)
//        base match {
//          case "authors" =>
//            prefix match {
//              case "B" =>
//                ref.authors += new Authors
//                author = ref.authors.last
//              case "I" =>
//                assert(author != null)
//                author = ref.authors.last
//            }
//          case "person" =>
//            prefix match {
//              case "B" =>
//                if (author == null) {
//                  if (ref.authors.isEmpty) ref.authors += new Authors
//                  author = ref.authors.last
//                }
//                author.person += new Person
//                person = author.person.last
//              case "I" =>
//                assert(person != null)
//                person = author.person.last
//            }
//
//          case "person-first" =>
//            prefix match {
//              case "B" =>
//                assert(person != null)
//                person.personFirst = new PersonFirst
//                person.personFirst.strings += token.string
//              case "I" =>
//                assert(person != null)
//                person.personFirst.strings += token.string
//            }
//          case "person-last" =>
//            prefix match {
//              case "B" =>
//                assert(person != null)
//                person.personLast = new PersonLast
//                person.personLast.strings += token.string
//              case "I" =>
//                assert(person != null)
//                person.personLast.strings += token.string
//            }
//
//          case "ref-marker" =>
//            prefix match {
//              case "B" =>
//                assert(refmarker == null, "refmarker not null")
//                ref.refMarker = new RefMarker
//                refmarker = ref.refMarker
//                refmarker.strings += token.string
//              case "I" =>
//                refmarker.strings += token.string
//            }
//
//          case "title" =>
//            prefix match {
//              case "B" =>
//                if (title == null) {
//                  ref.title = new Title
//                }
//                title = ref.title
//                title.strings += token.string
//              case "I" => title.strings += token.string
//            }
//
//          case "venue" =>
//            prefix match {
//              case "B" =>
//                ref.venues += new Venue
//                venue = ref.venues.last
//              case "I" =>
//                venue = ref.venues.last
//            }
//          case "booktitle" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.booktitle = new Booktitle
//                booktitle = venue.booktitle
//                booktitle.strings += token.string
//              case "I" =>
//                assert(booktitle != null)
//                booktitle.strings += token.string
//            }
//          case "journal" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.journal += new Journal
//                journal = venue.journal.last
//                journal.strings += token.string
//              case "I" =>
//                assert(journal != null)
//                journal.strings += token.string
//            }
//          case "organization" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.organization += new Organization
//                organization = venue.organization.last
//                organization.strings += token.string
//              case "I" =>
//                assert(organization != null)
//                organization.strings += token.string
//            }
//          case "chapter" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.chapter += new Chapter
//                chapter = venue.chapter.last
//                chapter.strings += token.string
//              case "I" =>
//                assert(chapter != null)
//                chapter.strings += token.string
//            }
//          case "number" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.number += new Number
//                number = venue.number.last
//                number.strings += token.string
//              case "I" =>
//                assert(number != null)
//                number.strings += token.string
//            }
//          case "publisher" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.publisher += new Publisher
//                publisher = venue.publisher.last
//                publisher.strings += token.string
//              case "I" =>
//                assert(publisher != null)
//                publisher.strings += token.string
//            }
//          case "volume" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                assert(venue.volume == null)
//                venue.volume = new Volume
//                volume = venue.volume
//                volume.strings += token.string
//              case "I" =>
//                assert(volume != null)
//                volume.strings += token.string
//            }
//          case "address" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                assert(venue.address == null)
//                venue.address = new Address
//                address = venue.address
//                address.strings += token.string
//              case "I" =>
//                assert(address != null)
//                address.strings += token.string
//            }
//          case "pages" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null)
//                venue.pages += new Pages
//                pages = venue.pages.last
//                pages.strings += token.string
//              case "I" =>
//                assert(pages != null)
//                pages.strings += token.string
//            }
//          case "date" =>
//            prefix match {
//              case "B" =>
//                if (ref.venues.isEmpty) ref.venues += new Venue
//                venue = ref.venues.last
//                venue.date = new Date
//                date = venue.date
//              case "I" => assert(venue != null && date != null)
//            }
//          case "year" =>
//            prefix match {
//              case "B" =>
//                assert(venue != null, "venue is null")
//                assert(date != null, "date is null")
//                assert(date.year == null, "date.year is not null")
//                date.year = new Year
//                year = date.year
//                year.strings += token.string
//              case "I" =>
//                assert(year != null)
//                year.strings += token.string
//            }
//          case _ => println("not yet supported: " + tag)
//        }
//      }
//    }
//    ref.toString
//  }
//
//}





