package edu.umass.cs.iesl.rpp

import java.util.concurrent._

import cc.factorie.app.nlp.lexicon.StaticLexicons
import edu.umass.cs.iesl.bibie.model.DefaultCitationTagger
import edu.umass.cs.iesl.paperheader.model.DefaultHeaderTagger
import edu.umass.cs.iesl.paperheader.model.TokenFeatures
import edu.umass.cs.iesl.xml_annotator._
import cc.factorie.util._
import java.io.{FilenameFilter, File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.net.URL

class BatchOpts extends DefaultCmdOptions with ModelProviderCmdOptions {
  val lexiconsUri = new CmdOption("lexicons-uri", "", "STRING", "URI to lexicons")
  val referenceModelUri = new CmdOption("reference-model-uri", "", "STRING", "reference model URI")
  val headerTaggerModelFile = new CmdOption("header-tagger-model", "", "STRING", "path to serialized header tagger model")
  val outputDir = new CmdOption("output-dir", "", "STRING", "where to store output")
  val inputDir = new CmdOption("input-dir", "", "STRING", "path to dir of input files")
  val logFile = new CmdOption("log-file", "", "STRING", "write logging info to this file")
  val dataFilesFile = new CmdOption("data-files-file", "", "STRING", "file containing a list of paths to data files, one per line")
  val brownClusters = new CmdOption[String]("brown-clusters", "", "STRING", "file containg Brown clusters for header tagger")
  val lexicons = new LexiconsProviderCmdOption("lexicons")
}


object BatchMain extends HyperparameterMain {

  final val codec = "UTF-8" // TODO: cmd option?

  def evaluateParameters(args: Array[String]): Double = {
    println(s"* main(): args: ${args.mkString(", ")}")
    val opts = new BatchOpts
    opts.parse(args)
    val referenceModelUri = opts.referenceModelUri.value
    val headerTaggerModelFile = opts.headerTaggerModelFile.value

    val lexiconUrlPrefix = getClass.getResource("/lexicons").toString
    val citationModelURL = new URL(referenceModelUri)
    val citationTagger = new DefaultCitationTagger(lexiconUrlPrefix, url = citationModelURL)

//    val lexicons = new StaticLexicons()(opts.lexicons.value)
    val lexicons = new StaticLexicons()(opts.lexicons.value)
    val headerTagger = new DefaultHeaderTagger(lexicons, headerTaggerModelFile)

    if (opts.brownClusters.wasInvoked) {
      println(s"Reading brown cluster file: ${opts.brownClusters.value}")
      for (line <- scala.io.Source.fromFile(opts.brownClusters.value).getLines()) {
        val splitLine = line.split("\t")
        TokenFeatures.clusters(splitLine(1)) = splitLine(0)
      }
      println(s"Loaded ${TokenFeatures.clusters.size} clusters for Header Tagger")
    }

    val inputFilenames =
      if (opts.inputDir.wasInvoked) new File(opts.inputDir.value).listFiles(new FilenameFilter() {
        override def accept(parent: File, name: String) = name.toLowerCase.endsWith(".svg")
      }).map(_.getAbsolutePath).toSeq
      else io.Source.fromFile(opts.dataFilesFile.value).getLines().toSeq
    val outputFilenames = inputFilenames.map(fname => opts.outputDir.value + "/" + fname.replaceFirst(".*/(.*)$", "$1.tagged.txt"))
    val badFiles = new scala.collection.mutable.ArrayBuffer[String]()

    inputFilenames.zip(outputFilenames).foreach { case (inputFile, outputFile) =>

      val startTimeMillis: Long = System.currentTimeMillis()
      def deltaSecs(): Long = {
        (System.currentTimeMillis() - startTimeMillis) / 1000
      }

      try {
        println(s"* processing\t$inputFile")

        // start the processing pipeline, using a separate thread to cancel long-running files. NB: this approach
        // is flawed and needs more work. it works for many long-running files, BUT NOT ALL, apparently because called
        // code does not check for the cancel() state. it is mysterious why it *does* work. TODO investigate, say via
        // http://stackoverflow.com/questions/2275443/how-to-timeout-a-thread
        val executor: ExecutorService = Executors.newSingleThreadExecutor()
        val processFuture: Future[Annotator] = executor.submit(new Callable[Annotator]() {
          override def call(): Annotator = {
            Main.process(citationTagger, headerTagger, inputFile)
          }
        })
        try {
          processFuture.get(5, TimeUnit.MINUTES) // TODO instead of hard-coding, pass timeout as a command line option
        } catch {
          case e: TimeoutException =>
            println(s"** TimeoutException\t$inputFile\t${deltaSecs()}")
            processFuture.cancel(true)
        }
        executor.shutdownNow()

        // processing done
        println(s"** writing\t$outputFile\t${deltaSecs()}")
        val pw = new PrintWriter(new File(outputFile), codec)
        val annotator = processFuture.get()

        // for debugging coarse segmentation, use this line instead of xml one to print basic information:
//        val outputStr = Main.coarseOutputStrForAnnotator(annotator, inputFile)
        try {
          val outputStr = MakeXML.mkXML(annotator) // previously mkXML(annotator)
          pw.write(outputStr)
          pw.close()
        } catch {
          case e: Exception =>
            println(s"error writing XML for file $inputFile , skipping")
            println(e)
        }

        println(s"** done\t$inputFile\t${deltaSecs()}")
      } catch {
        case e: Exception =>
          println(s"** failed\t$e\t${deltaSecs()}")
          e.printStackTrace()
          badFiles += inputFile
      }
    }

    println(s"* failed to process ${badFiles.length}/${inputFilenames.length} files.")
    if (opts.logFile.wasInvoked) {
      val pw = new PrintWriter(new File(opts.logFile.value))
      badFiles.foreach(f => pw.write(f + "\n"))
      pw.close()
    }
    badFiles.length
  }


}


class ParallelOpts extends BatchOpts {
  val dir = new CmdOption("dir", "", "STRING", "directory of files to process")
  val numJobs = new CmdOption("num-jobs", 8, "INT", "number of jobs to distribute processing over")
  val memPerJob = new CmdOption("mem", 8, "INT", "GB of memory to request per job")
  val numCores = new CmdOption("num-cores", 1, "INT", "number of cores to use")
}


object ParallelInvoker {

  def cut[A](xs: Seq[A], n: Int) = {
    val m = xs.length
    val targets = (0 to n).map { x => math.round((x.toDouble * m) / n).toInt }
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
    implicit val random = new scala.util.Random(0)
    val opts = new ParallelOpts
    opts.parse(args)
    val njobs = opts.numJobs.value
    val ncores = opts.numCores.value
    val mem = opts.memPerJob.value
    val dataDir = opts.dir.value
    val files = new File(dataDir).listFiles().map(_.getPath)

    // divide files into njobs sets of equal size
    val dividedDocs = cut(scala.util.Random.shuffle(files), njobs)

    val fnamePrefix = "tmp-filelist-"
    val fnames = (0 until njobs).map(i => Paths.get(s"$fnamePrefix$i").toAbsolutePath().toString)
    dividedDocs.zipWithIndex.foreach { case (doclist, idx) => {
      val writer = new PrintWriter(fnames(idx))
      doclist.foreach { fname => writer.println(fname) }
      println(fnames(idx))
      writer.close()
    }
    }

    println(s"Distributed ${files.length} data files into ${njobs} sets of ${dividedDocs.map(_.length).min}-${dividedDocs.map(_.length).max} files")

    val docsParam = DistributorParameter[String](opts.dataFilesFile, fnames)
    val qs = new cc.factorie.util.QSubExecutor(mem, "edu.umass.cs.iesl.rpp.BatchMain", ncores)
    val qsOpts = opts.writeInto(new BatchOpts)
    qsOpts.dataFilesFile.invoke()

    val distributor = new cc.factorie.util.JobDistributor(qsOpts, Seq(docsParam), qs.execute, 60)
    val result = distributor.distribute
    println(s"Finished running $result jobs, ${result.sum}/${files.length} documents failed")

    // remove created filelists
    fnames.foreach { fname => Files.delete(Paths.get(fname)) }

    println("Done")
  }
}
