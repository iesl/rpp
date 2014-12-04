package org.rexo.ui

import org.rexo.util.EnglishDictionary
import java.io._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import org.rexo.extraction.NewHtmlTokenization
import org.rexo.pipeline.components.{ReferenceExtractionFilter, RxPipeline, RxDocument}
import java.util.zip.GZIPInputStream
import org.rexo.pipeline._
import edu.umass.cs.rexo.ghuang.segmentation.SegmentationFilter
import org.rexo.store.MetaDataXMLDocument
import org.jdom2.Document
import org.jdom2.output.XMLOutputter
import org.jdom2.output.Format

//import com.typesafe.scalalogging
import org.jdom2.Document
import org.jdom2.input.SAXBuilder


/**
 * Created by klimzaporojets on 9/16/14.
 */

object ScalaTagger {

  val logger = Logger(LoggerFactory.getLogger("ScalaTagger"))

  private def buildPipeline(argumentMap: Map[Any, Any]): RxPipeline = {
    val pipeline: RxPipeline = new RxPipeline
    val logp: Boolean = argumentMap.get("enable.log") != null
    pipeline.getScope("session").put("log.boolean", logp )
    pipeline.getScope("session").put("log.directory", new File("./log"))
    pipeline.getScope("session").put("sessionID.integer", new Integer(-1))
    pipeline.addStandardFilters

    pipeline.add(new SegmentationFilter())
    pipeline.add(new ReferenceExtractionFilter)
    pipeline.add(new BodyExtractionFilter)

    return pipeline
  }



  def main (args:Array[String])
  {
    val dataDir:String = null
    val DICT_FILE:String = getClass.getResource("/words.txt").getPath

    val dictionary: EnglishDictionary = EnglishDictionary.createDefault(new File(dataDir, DICT_FILE))

    //todo: for now just without taking into account any of the external parameters , see to add parameters
    val pipeline: RxPipeline = buildPipeline(Map[Any, Any]()/*initProperties*/)


    Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_.nonEmpty).foreach(line =>
    {
      println("inside processing " + line)

      val files: Array[String] = line.split("->")
      val infile: File = new File(files(0).trim)
      val outfile: File = new File(files(1).trim)
      logger.info(infile.getPath + " -> " + outfile.getPath)


      if (infile.exists) {
        val document: Document = readInputDocument(infile)
        val tokenization: NewHtmlTokenization = NewHtmlTokenization.createNewHtmlTokenization(document, dictionary)
        val rdoc: RxDocument = new RxDocument


        rdoc.setTokenization(tokenization)


        try {
          pipeline.execute(rdoc)
          logger.info("writing output file")
          writeOutput(outfile, rdoc)
        }
        catch {
          case e: Exception => {
            e.printStackTrace()
          }
        }
      }
      else {
        print("File not found: " + infile.getPath)
      }
    })





    def readInputDocument(infile: File): Document =
    {
      val saxBuilder: SAXBuilder = new SAXBuilder
      val is: BufferedInputStream = new BufferedInputStream(new FileInputStream(infile))
      try {
        return saxBuilder.build(is)
      }
      catch {
        case e: Exception => {
          e.printStackTrace()
          throw new RuntimeException(e.getClass.getName + ": " + e.getMessage)
        }
      }
      finally {
        is.close
      }
    }



    def writeOutput(outputFile: File, rdoc: RxDocument)
    {
      val tokenization: NewHtmlTokenization = rdoc.getTokenization
      val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").getOrElse(null).asInstanceOf[collection.mutable.Map[Any, Any]]
      if (tokenization == null) {
        return
      }
      var xmlOutputStream: FileOutputStream = null
      try {
        val output: XMLOutputter = new XMLOutputter(Format.getCompactFormat.setOmitDeclaration(true))
        xmlOutputStream = new FileOutputStream(outputFile)

        val format = output.getFormat;
        format.setExpandEmptyElements(true); // if I set it to false, visualization doesn't work
        //sometimes whitespaces are used to make look the text well aligned, the preserve is done only
        //for visualization purposes
        format.setTextMode(Format.TextMode.PRESERVE);
        var outStr = output.outputString(rdoc.getTokenization._parsedDocument)

        outStr = outStr.replaceAll("""<bracket(.*?)></(.*?)>""","<bracket" + "$1" + "/>")

        val pw = new java.io.PrintWriter(new File(outputFile.getAbsolutePath + "new.html"))
        try pw.write(outStr) finally pw.close()

        output.output(rdoc.getTokenization._parsedDocument, xmlOutputStream)

        println ("finished")
      }
      catch {
        case e: IOException => {
          e.printStackTrace()
        }
      }
      finally {
        if (xmlOutputStream != null) {
          try {
            xmlOutputStream.close
          }
          catch {
            case e: IOException => {
              e.printStackTrace()
            }
          }
        }
      }
    }

  }

}
