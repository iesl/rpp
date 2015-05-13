package edu.umass.cs.iesl.rpp

import edu.umass.cs.iesl.bibie.TestCitationModel
import edu.umass.cs.iesl.paperheader.crf._
//import edu.umass.cs.iesl.paperheader.tagger._
import edu.umass.cs.iesl.xml_annotator._
import scala.collection.mutable.{ArrayBuffer, Stack}
import cc.factorie.app.nlp.{Document, Sentence, Token}
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.util._
import edu.umass.cs.iesl.bibie.CitationLabel
import java.io.{File, PrintWriter}
import java.nio.file.{Paths, Files}


/**
 * Created by kate on 3/24/15.
 *
 */

class BatchOpts extends DefaultCmdOptions {
  val lexiconsUri = new CmdOption("lexicons-uri", "", "STRING", "URI to lexicons")
  val referenceModelUri = new CmdOption("reference-model-uri", "", "STRING", "reference model URI")
  val headerTaggerModelFile = new CmdOption("header-tagger-model", "", "STRING", "path to serialized header tagger model")
  val outputDir = new CmdOption("output-dir", "", "STRING", "where to store output")
  val inputDir = new CmdOption("input-dir", "", "STRING", "path to dir of input files")
  val dataFilesFile = new CmdOption("data-files-file", "", "STRING", "file containing a list of paths to data files, one per line")
}

class ParallelOpts extends BatchOpts {
  val dir = new CmdOption("dir", "", "STRING", "directory of files to process")
  val output = new CmdOption("output", "", "STRING", "output dir")
  val numJobs = new CmdOption("num-jobs", 8, "INT", "number of jobs to distribute processing over")
  val memPerJob = new CmdOption("mem", 8, "INT", "GB of memory to request per job")
  val numCores = new CmdOption("num-cores", 1, "INT", "number of cores to use")
}

object ParallelInvoker {
  def cut[A](xs: Seq[A], n: Int) = {
    val m = xs.length
    val targets = (0 to n).map { x => math.round((x.toDouble * m) / n).toInt}
    def snip(xs: Seq[A], ns: Seq[Int], got: Seq[Seq[A]]): Seq[Seq[A]] = {
      if (ns.length < 2) got
      else {
        val (i, j) = (ns.head, ns.tail.head)
        snip(xs.drop(j - i), ns.tail, got :+ xs.take(j - i))
      }
    }
    snip(xs, targets, Seq.empty)
  }
  def main(args: Array[String]): Unit = {
    import sys.process._
    implicit val random = new scala.util.Random(0)
    val opts = new ParallelOpts
    opts.parse(args)
    val njobs = opts.numJobs.value
    val ncores = opts.numCores.value
    val mem = opts.memPerJob.value
    val dataDir = opts.dir.value
    val outputDir = opts.output.value
    assert(outputDir != "")
    val files = new File(dataDir).listFiles().map(_.getPath)
    // divide files into njobs sets of equal size
    val dividedDocs = cut(scala.util.Random.shuffle(files), njobs)
    //write filelists
    val prefix = "tmp-filelist-"
    val filenames = (0 until njobs).map(i => Paths.get(s"$prefix$i").toAbsolutePath.toString)
    dividedDocs.zipWithIndex.foreach { case (doclist, idx) =>
      val pw = new PrintWriter(filenames(idx))
      doclist.foreach { fname => pw.println(fname) }
      println(filenames(idx))
      pw.close()
    }

    val cmds = filenames.map { filelist =>
      val args = s"$filelist $outputDir"
      val cmd = System.getenv("RPP_ROOT") + "/bin/process-batch-distributed.sh " + args
      val qsubCmd = s"qsub -pe blake $ncores -sync y -l mem_token=${mem}G -v RPP_ROOT -j y -S /bin/sh $cmd"
      qsubCmd
    }

    cmds.par.foreach { cmd =>
      println(cmd)
      Process(cmd).run()
    }

//     remove created filelists
//    filenames.foreach { fname => Files.delete(Paths.get(fname)) }
    println("done.")
  }
}

object BatchMain {
  def main(args: Array[String]): Unit = {
    val opts = new BatchOpts
    opts.parse(args)
    val referenceModelUri = opts.referenceModelUri.value
    val headerTaggerModelFile = opts.headerTaggerModelFile.value

    // loading inputs
    val inputFiles: Seq[File] = {
      if (opts.dataFilesFile.wasInvoked) scala.io.Source.fromFile(opts.dataFilesFile.value).getLines().map(fname => new File(fname)).toSeq
      else if (opts.inputDir.wasInvoked) new File(opts.inputDir.value).listFiles
      else Seq()
    }
    val outputFilenames: Seq[String] = {
//      if (opts.outputDir.wasInvoked) {
        val outputDir = opts.outputDir.value
        inputFiles.map(_.getName).map(f => outputDir + f + ".tagged")
//      } else inputFiles.map(_.getAbsolutePath + ".tagged")
    }

    // loading models
    val lexiconUrlPrefix = getClass.getResource("/lexicons").toString
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

    // FIXME need to get around this whole re-processing thing; i.e. want FACTORIE doc so we can write XML easily
    // TODO also add body text?
    inputFiles.zip(outputFilenames).foreach { case (input, output) =>
      println(s"processing: ${input.getAbsolutePath} --> $output")
      var annotator: Annotator = null

      /* segmentation */
      try {
        annotator = Main.process(trainer, headerTagger, input.getAbsolutePath)
//        annotator.write(output+"-blah")
      } catch {
        case e: Exception => updateErrs(e)
      }

      if (annotator != null) {
        var fail = 0

        /* annotate with paper-header */
        val headerTxt = Main.getHeaderLines(annotator).mkString("\n")
        println(headerTxt)
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

        /* annotate with bibie */
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

        /* write to XML */
        var xml =  ""
        try {
          xml = XMLParser.docsToXML(headerDoc, refDocs)
          scala.xml.XML.loadString(xml)
        } catch {
          case e: Exception =>
            updateErrs(e)
            fail = 1
        }
        if (fail == 0) {
          val pw = new PrintWriter(new File(output))
          pw.write(xml)
          pw.close()
        } else {
          failCount += 1
        }
      } else {
        failCount += 1
      }
    }

    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
    println("exceptions:")
    errAnalysis.foreach {
      case (name, ct) => println(s"$name $ct")
    }
  }
}
