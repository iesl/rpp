package edu.umass.cs.iesl.rpp

/**
 * @author Kate Silverstein 
 *         created on 4/1/15
 */
import cc.factorie.app.nlp._
import cc.factorie.util.namejuggler
import edu.umass.cs.iesl.bibie._
import edu.umass.cs.iesl.paperheader.tagger.HeaderTagSpanBuffer
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






