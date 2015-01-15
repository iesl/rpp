//package org.rexo.crf
//
////import _root_.annotator.Annotator
////import _root_.annotator.LineAnnotator
//import cc.factorie.variable._
//import java.io._
//import edu.umass.cs.rexo.ghuang.segmentation.{SegmentationFilter, LineInfo}
//import org.rexo.util.EnglishDictionary
//import org.rexo.pipeline.components.{ReferenceExtractionFilter, RxDocument, RxPipeline}
//import org.jdom2.Document
//import org.rexo.extraction.{NewHtmlTokenizationSvg, NewHtmlTokenization}
//import org.jdom2.input.SAXBuilder
//import org.jdom2.output.{Format, XMLOutputter}
//import com.typesafe.scalalogging.Logger
//import org.slf4j.LoggerFactory
//import org.rexo.pipeline.BodyExtractionFilter
//import org.rexo.pipeline.components.svg.{RxPipelineSvg, RxDocumentSvg}
//import edu.umass.cs.rexo.ghuang.segmentation.svg.SegmentationFilterSvg
//import org.rexo.pipeline.svg.{BodyExtractionFilterSvg, ReferenceExtractionFilterSvg}
//
////import cc.factorie.app.nlp.segment.SegmentedCorpusLabeling
//
///**
// * Created by klimzaporojets on 11/9/14.
// */
////abstract class ReferenceTag (val line:ChainReferenceSegmenter#Line, initialIndex:Int) extends CategoricalVariable[String](initialIndex)
//
//abstract class ReferenceTag
//  extends CategoricalDomain[String]
//  with SegmentedCorpusLabeling
//
//object ReferenceDomain extends ReferenceTag /*CategoricalDomain[String]*/{
//  this ++= Vector(
//    "biblioPrologue",
//    "B-biblio",
//    "I-biblio",
//    "junk",
//    "post"
//  )
//  freeze()
//
//
//}
//
//trait SegmentedCorpusLabeling {
//
//  val logger = Logger(LoggerFactory.getLogger("ScalaTagger"))
//
//  private def buildPipeline(argumentMap: Map[Any, Any]): RxPipelineSvg = {
//    val pipeline: RxPipelineSvg = new RxPipelineSvg
//    //    val hdrCrf: File = new File(dataDir, HEADER_CRF)
//    //    val refCrf: File = new File(dataDir, REFERENCE_CRF)
//    //    val bibCrf: File = new File(dataDir, BIBLIO_SEG_CRF)
//    val logp: Boolean = argumentMap.get("enable.log") != null
//    pipeline.getScope("session").put("log.boolean", logp /*Boolean.valueOf(logp)*/)
//    pipeline.getScope("session").put("log.directory", new File("./log"))
//    pipeline.getScope("session").put("sessionID.integer", new Integer(-1))
//    //    log.info(if (logp) "Statistics logging enabled" else "Statistics logging disabled (default)")
//    pipeline.addStandardFilters
//    //    log.info("loading biblio-segmentation crf")
//    //    val ois: ObjectInputStream = new ObjectInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(bibCrf))))
//    //    val crf: CRF4 = ois.readObject.asInstanceOf[CRF4]
//    //    ois.close
//
//    //todo: figure out what to do with CRFBibliographySegmentor: it uses some classes from mallet, translate it to factory??
//    //    val crfBibSegmentor: CRFBibliographySegmentor = new CRFBibliographySegmentor(crf)
//    pipeline.add(new SegmentationFilterSvg(/*crfBibSegmentor*/))
//    pipeline.add(new ReferenceExtractionFilterSvg)
//    pipeline.add(new BodyExtractionFilterSvg)
//
//    //    pipeline.add(new GrantExtractionFilter).add(new ReferenceExtractionFilter(refCrf, hdrCrf)).add(new BodyExtractionFilter)
//    //not in the first version
//    //    if (logp) {
//    //      pipeline.addErrorFilters.add(new ErrorLogFilter).addEpilogueFilters.add(new InfoLogFilter).add(new PipelineMetricsFilter)
//    //    }
//    return pipeline
//  }
//
//  def readInputDocument(baseFilePath:String /*infile: File*/): List[Document] =
//  {
//    val builder = new SAXBuilder()
//
//    def getFileList(baseFilePath:String, pageNumber:Int):List[Document] =
//    {
//      val newFile:java.io.File = new File(baseFilePath+pageNumber+".svg")
//      if(newFile.exists /*infile.getAbsolutePath */)
//      {
//        return builder.build(newFile) +: getFileList(baseFilePath, pageNumber+1)
//
//      }
//      return List()
//    }
//
//    return getFileList(baseFilePath,1)
//  }
//
//  def getLabeledLines(corpusPath:String /*corpusPath:File*/):IndexedSeq[(LineInfo)] = {
//
//    //TODO: implement line extraction, corpus will be the name of the file
//
//    val dataDir:String = null
//    val DICT_FILE:String = getClass.getResource("/words.txt").getPath
//
//    val dictionary: EnglishDictionary = EnglishDictionary.createDefault(new File(dataDir, DICT_FILE))
//
//    //todo: for now just without taking into account any of the external parameters , see to add parameters
//    val pipeline: RxPipelineSvg = buildPipeline(Map[Any, Any]()/*initProperties*/)
//
//    //    val logger = new LoggerFactory()
//
////    Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_.nonEmpty).foreach(line =>
////    {
////      println("inside processing " + line)
//
////      val files: Array[String] = line.split("->")
////      val infile: File = corpus
////      new File(files(0).trim)
////      val outfile: File = new File(files(1).trim)
////      logger.info(infile.getPath + " -> " + outfile.getPath)
//
//
////      if (infile.exists) {
//        val document: List[Document] = readInputDocument(corpusPath)
//        val tokenization: NewHtmlTokenizationSvg = NewHtmlTokenizationSvg.createNewHtmlTokenization(document, dictionary)
//        val rdoc: RxDocumentSvg = new RxDocumentSvg
//
//
//        rdoc.setTokenization(tokenization)
//
//
//        try {
//          pipeline.execute(rdoc)
//          logger.info("writing output file")
////          writeOutput(outfile, rdoc)
//        }
//        catch {
//          case e: Exception => {
//            e.printStackTrace()
//
//            //            log.error(e.getClass.getName + ": " + e.getMessage)
//          }
//        }
//
//
//
//
//
//    def writeOutput(outputFile: File, rdoc: RxDocument)
//    {
//      val tokenization: NewHtmlTokenization = rdoc.getTokenization
//      val segmentations: collection.mutable.Map[Any, Any] = rdoc.getScope("document").get("segmentation").getOrElse(null).asInstanceOf[collection.mutable.Map[Any, Any]]
//      if (tokenization == null) {
//        //        log.error("No xml content available for document " + rdoc)
//        return
//      }
//      var xmlOutputStream: FileOutputStream = null
//      try {
//        val output: XMLOutputter = new XMLOutputter(Format.getCompactFormat.setOmitDeclaration(true))
//        xmlOutputStream = new FileOutputStream(outputFile)
//
//        val format = output.getFormat;
//        format.setExpandEmptyElements(true); // if I set it to false, visualization doesn't work
//        //sometimes whitespaces are used to make look the text well aligned, the preserve is done only
//        //for visualization purposes
//        format.setTextMode(Format.TextMode.PRESERVE);
//        var outStr = output.outputString(rdoc.getTokenization._parsedDocument)
//
//        //scala.tools.nsc.io.File("filename").writeAll(outStr)
//
//        //regex for eliminating bracket-begin / bracket-end closing tags and putting the closing mark inside the same tag
//        //for example: <bracket-end id-n="7" type="reference"></bracket-end> into <bracket-end id-n="7" type="reference"/>
//
//        outStr = outStr.replaceAll("""<bracket(.*?)></(.*?)>""","<bracket" + "$1" + "/>")
//
//        val pw = new java.io.PrintWriter(new File(outputFile.getAbsolutePath + "new.html"))
//        try pw.write(outStr) finally pw.close()
//
//        output.output(rdoc.getTokenization._parsedDocument, xmlOutputStream)
//
//        println ("finished")
//      }
//      catch {
//        case e: IOException => {
//          e.printStackTrace()
//          //          log.info("(xml writer) " + e.getClass.getName + ": " + e.getMessage)
//        }
//      }
//      finally {
//        if (xmlOutputStream != null) {
//          try {
//            xmlOutputStream.close
//          }
//          catch {
//            case e: IOException => {
//              e.printStackTrace()
//              //              log.error(e.getMessage)
//            }
//          }
//        }
//      }
//    }
//
//    return null
//  }
//}
