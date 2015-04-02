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
    val referenceModelUri = args(0)
    val headerTaggerModelFile = args(1)
    val inputDir = new File(args(2))
    val outputDir = args(3)
    val inputFiles = inputDir.listFiles.map(_.getPath)
    val outputFiles = inputDir.listFiles.map(_.getName).map(n => outputDir + "/" + n + ".tagged")
    val lexiconUrlPrefix = "file://" + getClass.getResource("/lexicons").getPath()
    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)
    val headerTagger = new HeaderTagger
    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))
    var failCount = 0
    var totalCount = 0

    // FIXME need to get around this whole re-processing thing -- but how ...
    // TODO also add body text?
    inputFiles.zip(outputFiles).take(5).foreach({ case (input, output) =>
      try {
        println(s"processing: $input")
        val annotator = Main.process(trainer, headerTagger, input)
        val headerTxt = Main.getHeaderLines(annotator).mkString("\n")
        val headerDoc = new Document(headerTxt)
        DeterministicTokenizer.process(headerDoc)
        new Sentence(headerDoc.asSection, 0, headerDoc.tokens.size)
        headerTagger.process(headerDoc)

        val refs = Main.getReferencesWithBreaks(annotator)
        val refDocs = refs.map(ref => {
          val doc = new Document(ref)
          DeterministicTokenizer.process(doc)
          new Sentence(doc.asSection, 0, doc.tokens.size)
          doc.tokens.foreach(t => t.attr += new CitationLabel("", t))
          doc
        })
        TestCitationModel.process(refDocs, trainer)

        val xml = XMLParser.docsToXML(headerDoc, refDocs)
        println(xml)
        scala.xml.XML.loadString(xml)

        val pw = new PrintWriter(new File(output))
        pw.write(xml)
        pw.close()

      } catch {
        case e: Exception =>
          println(s"failed to process file: $input")
          e.printStackTrace()
          failCount += 1
      }
      totalCount += 1
    })

    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
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