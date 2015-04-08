package edu.umass.cs.iesl.rpp

import edu.umass.cs.iesl.bibie.TestCitationModel
import edu.umass.cs.iesl.paperheader.crf._
import edu.umass.cs.iesl.xml_annotator._
import scala.collection.mutable.{ArrayBuffer, Stack}
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.{Document, Sentence, Token}
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import edu.umass.cs.iesl.bibie.CitationLabel


/**
 * Created by kate on 3/24/15.
 */



object BatchMain {
  def main(args: Array[String]): Unit = {
    import java.io.{File, PrintWriter}

    // check to make sure the args are valid
    //    args.foreach(arg => assert(new File(arg).exists, s"filename $arg does not exist."))

    val referenceModelUri = args(0)
    val headerTaggerModelFile = args(1)
    val inputDir = new File(args(2))
    val outputDir = args(3)

    val inputFiles = inputDir.listFiles.map(_.getPath)
    println(s"about to process ${inputFiles.length} input files")
    val outputFiles = inputDir.listFiles.map(_.getName).map(n => outputDir + "/" + n + ".tagged")
    val lexiconUrlPrefix = "file://" + getClass.getResource("/lexicons").getPath()
    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)
    val headerTagger = new HeaderTagger
    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))
    var failCount = 0
    val totalCount = inputFiles.length
    val errAnalysis = new scala.collection.mutable.HashMap[String, Int]()

    def updateErrs[E<:Exception](e: E): Unit = {
      val key = e.getClass.getCanonicalName
      if (errAnalysis.contains(key)) errAnalysis(key) += 1
      else errAnalysis(key) = 1
    }

    // FIXME need to get around this whole re-processing thing -- but how ...
    // TODO also add body text?
    inputFiles.zip(outputFiles).foreach { case (input, output) =>
      println(s"processing: $input")
      var annotator: Annotator = null
      try {
        annotator = Main.process(trainer, headerTagger, input)
      } catch {
        case e: Exception => updateErrs(e)
      }
      if (annotator != null) {
        var fail = 0
        val headerTxt = Main.getHeaderLines(annotator).mkString("\n")
        val headerDoc = new Document(headerTxt)
        DeterministicTokenizer.process(headerDoc)
        new Sentence(headerDoc.asSection, 0, headerDoc.tokens.size)
        try {
          headerTagger.process(headerDoc)
        } catch {
          case e: Exception =>
            updateErrs(e)
            fail = 1
        }
        val refs = Main.getReferencesWithBreaks(annotator)
        val refDocs = refs.map(ref => {
          val doc = new Document(ref)
          DeterministicTokenizer.process(doc)
          new Sentence(doc.asSection, 0, doc.tokens.size)
          doc.tokens.foreach(t => t.attr += new CitationLabel("", t))
          doc
        })
        try {
          TestCitationModel.process(refDocs, trainer, print=false)
        } catch {
          case e: Exception =>
            updateErrs(e)
            fail = 1
        }
        val xml = XMLParser.docsToXML(headerDoc, refDocs)
        try {
          scala.xml.XML.loadString(xml)
        } catch {
          case e: Exception =>
            updateErrs(e)
            fail = 1
        }
        if (fail == 0) {
//          println("")
//          println(xml)
//          println("")
          val pw = new PrintWriter(new File(output))
          pw.write(xml)
          pw.close()
        } else {
          failCount += 1
        }
      } else {
        failCount += 1
      }
      //      try {
      //        println(s"processing: $input")
      //        val annotator = Main.process(trainer, headerTagger, input)
      //        val headerTxt = Main.getHeaderLines(annotator).mkString("\n")
      //        val headerDoc = new Document(headerTxt)
      //        DeterministicTokenizer.process(headerDoc)
      //        new Sentence(headerDoc.asSection, 0, headerDoc.tokens.size)
      //        headerTagger.process(headerDoc)
      //
      //        val refs = Main.getReferencesWithBreaks(annotator)
      //        val refDocs = refs.map(ref => {
      //          val doc = new Document(ref)
      //          DeterministicTokenizer.process(doc)
      //          new Sentence(doc.asSection, 0, doc.tokens.size)
      //          doc.tokens.foreach(t => t.attr += new CitationLabel("", t))
      //          doc
      //        })
      //        TestCitationModel.process(refDocs, trainer, print=false)
      //
      //        val xml = XMLParser.docsToXML(headerDoc, refDocs)
      //        scala.xml.XML.loadString(xml)
      //        val pw = new PrintWriter(new File(output))
      //        pw.write(xml)
      //        pw.close()
      //
      //      } catch {
      //        case e: Exception =>
      //          println(s"failed to process file: $input ; exception message = ${e.toString}")
      ////          e.printStackTrace()
      //          failCount += 1
      //      }
    }

    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
    println("exceptions:")
    errAnalysis.foreach {
      case (name, ct) => println(s"$name $ct")
    }
  }
}

/*
annotation types:

reference
body
header-institution
ref-month
header-note
header-token
line
ref-address
ref-first
reference-token
section-marker
abstract
header-title
ref-middle
ref-date
ref-booktitle
citation
reference_id
header-email
ref-marker
ref-year
paragraph
ref-organization
ref-volume
header-author
header-date
ref-pages
header
ref-authors
ref-title
ref-last
biblio-marker
ref-journal
header-address
ref-person
header-tech
figure-marker
table-marker
ref-venue
 */