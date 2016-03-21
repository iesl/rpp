package edu.umass.cs.iesl.rpp

//import cc.factorie.app.nlp._

import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.util._

import edu.umass.cs.iesl.xml_annotator.Annotator
import edu.umass.cs.iesl.xml_annotator.Annotator._
import edu.umass.cs.iesl.paperheader.model._
import edu.umass.cs.iesl.bibie.model.DefaultCitationTagger

import scala.io.StdIn
import scala.collection.mutable

import java.util.logging.Logger
import java.io.File

import org.jdom2.input.SAXBuilder
import org.jdom2.Document


object Cli {

  private val logger = Logger.getLogger(getClass.getName)

  class CliOpts extends DefaultCmdOptions {
    val inputDir = new CmdOption[String]("input-dir", "", "STRING", "input directory")
    val htModel = new CmdOption[String]("ht-model", "", "STRING", "header tagger model")
    val citeModel = new CmdOption[String]("cite-model", "", "STRING", "bibie model")
  }

  val opts = new CliOpts
  val docs = new mutable.ArrayBuffer[Document]()
  val anns = new mutable.ArrayBuffer[Annotator]()
  var idx = 0

  def main(args: Array[String]): Unit = {
    opts.parse(args)
    print("> ")
    var text = StdIn.readLine()
    while (text != null) {
      logger.info(text)
      text match {
        case "load" => loadDocs()
        case "process" => processDocs()
        case "curr" =>
          val curr = docs(idx)
          println(curr)
        case "next" =>
          idx += 1
          if (idx < docs.length) println(docs(idx))
        case "prev" =>
          idx -= 1
          if (idx >= 0) println(docs(idx))
        case "line" =>
          val dom = docs(idx)
          val ann = List(LineProcessor).foldLeft(Annotator(dom)) {
            case (annoAcc, pro) => pro.process(annoAcc)
          }
          printAnnots(ann)
        case "all" =>
          val dom = docs(idx)
          val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
          val ht = new DefaultHeaderTagger(None, lexicon, new java.net.URL(opts.htModel.value))
          val lexiconPrefix = getClass.getResource("/lexicons").toString
          val citeModel = opts.citeModel.value
          val ct = new DefaultCitationTagger(lexiconPrefix, citeModel)
          val ann = List(
            LineProcessor,
            StructureProcessor,
            HeaderPartProcessor(ht),
            CitationProcessor,
            CitationReferenceLinkProcessor
          ).foldLeft(Annotator(dom)) {
            case (annoAcc, pro) =>
              val a = pro.process(annoAcc)
              printAnnots(a)
              println("")
              a
          }
        case "cite" =>
          val dom = docs(idx)
          val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
          val ht = new DefaultHeaderTagger(None, lexicon, new java.net.URL(opts.htModel.value))
          val lexiconPrefix = getClass.getResource("/lexicons").toString
          val citeModel = opts.citeModel.value
          val ct = new DefaultCitationTagger(lexiconPrefix, citeModel)
          val ann = List(
            LineProcessor,
            StructureProcessor,
            HeaderPartProcessor(ht)
          ).foldLeft(Annotator(dom)) { case (annoAcc, pro) => pro.process(annoAcc) }
          val ann2 = List(CitationProcessor).foldLeft(Annotator(dom)) { case (annoAcc, pro) =>
            val a = pro.process(annoAcc)
            printAnnots(a)
            a
          }

        case "xml" =>
          val ann = anns(idx)
          val xml = MakeXML.mkXML(ann)
          println(xml)
      }

      print("> ")
      text = StdIn.readLine()
    }
  }

  def loadDocs(): Unit = {
    val filenames: Seq[String] = new File(opts.inputDir.value).listFiles().map(_.getAbsolutePath)
    val builder = new SAXBuilder()
    val doms: Seq[Document] = filenames.map { fname => builder.build(new File(fname)) }
    docs ++= doms
    logger.info(s"loaded ${docs.length} docs")
  }

  def processDocs(): Unit = {
    val lexicon = new StaticLexicons()(LexiconsProvider.classpath())
    val ht = new DefaultHeaderTagger(None, lexicon, new java.net.URL(opts.htModel.value))
    val lexiconPrefix = getClass.getResource("/lexicons").toString
    val citeModel = opts.citeModel.value
    val ct = new DefaultCitationTagger(lexiconPrefix, citeModel)
    anns ++= docs.map { dom =>
      val ann = List(
        LineProcessor,
        StructureProcessor,
        HeaderPartProcessor(ht),
        ReferencePartProcessor(ct),
        CitationProcessor,
        CitationReferenceLinkProcessor
      ).foldLeft(Annotator(dom)) {
        case (annoAcc, pro) => pro.process(annoAcc)
      }
      ann
    }
  }

  def printAnnots(ann: Annotator): Unit = {
    val anns: Seq[(String, String)] = Main.getAllAnnotations(ann)
    anns.foreach { thing =>
      val (k, v) = thing
      println(s"$k\t$v")
    }
  }



}
