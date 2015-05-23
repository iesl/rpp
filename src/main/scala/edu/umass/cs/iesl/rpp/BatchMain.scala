package edu.umass.cs.iesl.rpp

import edu.umass.cs.iesl.bibie.TestCitationModel
import edu.umass.cs.iesl.paperheader.tagger._
import edu.umass.cs.iesl.xml_annotator._
import cc.factorie.util._
import java.io.{File, PrintWriter}
import java.nio.file.Paths


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
  val logFile = new CmdOption("log-file", "", "STRING", "write logging info to this file")
  val dataFilesFile = new CmdOption("data-files-file", "", "STRING", "file containing a list of paths to data files, one per line")
}

object BatchMain {
  def main(args: Array[String]): Unit = {
    println("args: " + args.mkString(", "))
    val opts = new BatchOpts
    opts.parse(args)
    val referenceModelUri = opts.referenceModelUri.value
    val headerTaggerModelFile = opts.headerTaggerModelFile.value

    val lexiconUrlPrefix = getClass.getResource("/lexicons").toString
    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)

    val headerTagger = new HeaderTagger
    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))

    val inputFilenames = new File(opts.inputDir.value).listFiles.map(_.getAbsolutePath)
    val outputFilenames = new File(opts.inputDir.value).listFiles.map(_.getName).map(fname => s"${opts.outputDir.value}/$fname.tagged")

    val badFiles = new scala.collection.mutable.ArrayBuffer[String]()

    inputFilenames.zip(outputFilenames).foreach { case (input, output) =>
      try {
        val annotator = Main.process(trainer, headerTagger, input)
        val xml = mkXML(annotator)
        val pw = new PrintWriter(new File(output))
        pw.write(xml)
        pw.close()
      } catch {
        case e: Exception =>
          e.printStackTrace()
          badFiles += input
      }
    }

    println(s"failed to process ${badFiles.length}/${inputFilenames.length} files.")
    if (opts.logFile.wasInvoked) {
      val pw = new PrintWriter(new File(opts.logFile.value))
      badFiles.foreach(f => pw.write(f + "\n"))
      pw.close()
    }
  }

  def mkXML(annotator: Annotator): String = {
    val xml = new StringBuilder()
    xml.appendLine("<document>")
    xml.append(mkHeaderXML(annotator))
    xml.append(mkReferenceXML(annotator))
    xml.appendLine("</document>")
    xml.toString()
  }

  def mkHeaderXML(annotator: Annotator): String = {
    import HeaderPartProcessor._
    def lineBreak(pair: (Int, String)) = {
      val (offset, text) = pair
      val lineBIndexSet = annotator.getBIndexSetByAnnotationType("line")
      Annotator.mkTextWithBreaks(text, lineBIndexSet.map(_ - offset), ' ')
    }
    def normalTag(s: String): String = if (s == "abstract") s else s.split("-").last
    val xml = new StringBuilder()
    implicit var level = 1
    annotator.getRangeSet("header").foreach(headerRange => {
      xml.appendLine("<header>")
      List(headerTitle, headerAffiliation, headerAddress, headerEmail, headerDate, headerAbstract).foreach(annoType => {
        level += 1
        val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(headerRange)
        val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
        annos.foreach(t => xml.appendLine("<" + normalTag(annoType) + ">" + t.trim + "</" + normalTag(annoType) + ">"))
        level -= 1
      })
      level += 1
      xml.appendLine("<authors>")
      val authorBIndexSet = annotator.getBIndexSetWithinRange(headerAuthor)(headerRange)
      authorBIndexSet.foreach(ai => {
        level += 1
        xml.appendLine("<person>")
        val tokens = annotator.getRange(headerAuthor)(ai).toList.flatMap(authorRange => {
          val tokenBIndexSet = annotator.getFilteredBIndexSetWithinRange(headerAuthor, headerToken)(authorRange)
          tokenBIndexSet.toList.flatMap(tokenIndex => annotator.getTextOption(headerToken)(tokenIndex).map(lineBreak _))
        })
        val name = tokens.mkString(" ")
        val parts = cc.factorie.util.namejuggler.PersonNameParser.parseFullName(name)
        level += 1
        xml.appendLine(s"<person-first>${parts.givenNames.mkString(" ")}</person-first>")
        xml.appendLine(s"<person-last>${parts.surNames.mkString(" ")}</person-last>")
        // TODO middle name, prefixes/suffixes
        level -= 1
        xml.appendLine("</person>")
        level -= 1
      })
      xml.appendLine("</authors>")
      level -= 1
      xml.appendLine("</header>")
    })
    xml.toString()
  }

  def mkReferenceXML(annotator: Annotator): String = {
    import ReferencePartProcessor._
    def lineBreak(pair: (Int, String)) = {
      val (offset, text) = pair
      val lineBIndexSet = annotator.getBIndexSetByAnnotationType("line")
      Annotator.mkTextWithBreaks(text, lineBIndexSet.map(_ - offset), ' ')
    }
    def normalTag(s: String): String = {
      val annoMap = Map(
      "ref-first" -> "person-first",
      "ref-last" -> "person-last"
      )
      if (annoMap.contains(s)) annoMap(s)
      else if (s.startsWith("ref-")) s.split("-").last
      else s
    }
    val xml = new StringBuilder()
    implicit var level = 1
    annotator.getRangeSet("reference").foreach(refsRange => {
      xml.appendLine("<references>")
      val refBIndexSet = annotator.getBIndexSetWithinRange("biblio-marker")(refsRange)
      refBIndexSet.foreach(refIndex => {
        level += 1
        xml.appendLine("<reference>")
        annotator.getRange("biblio-marker")(refIndex).foreach(refRange => {

          List(
            refTitleString, refMarkerString, referenceIdString
          ).foreach(annoType => {
            level += 1
            val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(refRange)
            val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
            val xmlTag = normalTag(annoType)
            annos.foreach(t => xml.appendLine("<" + xmlTag + ">" + t.trim + "</" + xmlTag + ">"))
            level -= 1
          })

          val dateBIndexSet = annotator.getBIndexSetWithinRange(refDateString)(refRange)
          dateBIndexSet.foreach { asi =>
            annotator.getRange(refDateString)(asi).foreach { dateRange =>
              level += 1
              xml.appendLine("<date>")
              List(refYearString, refMonthString).foreach { annoType =>
                level += 1
                val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(dateRange)
                val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
                val xmlTag = normalTag(annoType)
                annos.foreach(t => xml.appendLine(s"<$xmlTag>${t.trim}</$xmlTag>"))
                level -= 1
              }
              xml.appendLine("</date>")
              level -= 1
            }
          }

          val authorsBIndexSet = annotator.getBIndexSetWithinRange(refAuthorsString)(refRange)
          authorsBIndexSet.foreach(asi => { annotator.getRange(refAuthorsString)(asi).foreach(authorsRange => {
            level += 1
            xml.appendLine("<authors>")
            val personBIndexSet = annotator.getBIndexSetWithinRange(refPersonString)(refRange)
            personBIndexSet.foreach(pi => { annotator.getRange(refPersonString)(pi).foreach(personRange => {
              level += 1
              xml.appendLine("<person>")
              List(
                refFirstString, refMiddleString, refLastString
              ).foreach(annoType => {
                level += 1
                val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(personRange)
                val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
                val xmlTag = normalTag(annoType)
                annos.foreach(t => xml.appendLine("<" + xmlTag + ">" + t.trim + "</" + xmlTag + ">"))
                level -= 1
              })
              xml.appendLine("</person>")
              level -= 1
            })})
            xml.appendLine("</authors>")
            level -= 1
          })})

          val venueBIndexSet = annotator.getBIndexSetWithinRange(refVenueString)(refRange)
          venueBIndexSet.foreach { asi =>
            annotator.getRange(refVenueString)(asi).foreach { venueRange =>
              level += 1
              xml.appendLine("<venue>")
              List(
                refJournalString, refBooktitleString, refOrganizationString, refAddressString, refVolumeString, refPagesString
              ).foreach { annoType =>
                level += 1
                val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(venueRange)
                val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
                val xmlTag = normalTag(annoType)
                annos.foreach(t => xml.appendLine("<" + xmlTag + ">" + t.trim + "</" + xmlTag + ">"))
                level -= 1
              }
              xml.appendLine("</venue>")
              level -= 1
            }
          }
        })
        xml.appendLine("</reference>")
        level -= 1
      })
      xml.appendLine("</references>")
    })
    xml.toString()
  }
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

//    val inputFiles: Seq[File] = {
//      // for distributed processing
//      if (opts.dataFilesFile.wasInvoked) scala.io.Source.fromFile(opts.dataFilesFile.value).getLines().map(fname => new File(fname)).toSeq
//      // for normal processing
//      else if (opts.inputDir.wasInvoked) new File(opts.inputDir.value).listFiles
//      else Seq()
//    }
//    val outputFilenames: Seq[String] = {
//      val outputDir = opts.outputDir.value
//      inputFiles.map(_.getName).map(f => s"$outputDir/$f.tagged")
//    }
//    val lexiconUrlPrefix = getClass.getResource("/lexicons").toString
//    println("lexiconUrlPrefix: " + lexiconUrlPrefix)
//    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)
//    val headerTagger = new HeaderTagger
//    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))
//    var failCount = 0
//    val totalCount = inputFiles.length
//    val errAnalysis = new scala.collection.mutable.HashMap[String, Int]()
//
//    def updateErrs[E<:Exception](e: E): Unit = {
//      val key = e.getClass.getCanonicalName
//      if (errAnalysis.contains(key)) errAnalysis(key) += 1
//      else errAnalysis(key) = 1
//    }
//
//    // TODO also add body text?
//    inputFiles.zip(outputFilenames).foreach { case (input, output) =>
//      println(s"processing: ${input.getAbsolutePath} --> $output")
//      val annotator = Main.process(trainer, headerTagger, input.getAbsolutePath).write(output)
//    }


//      var annotator: Annotator = null
//
//      /* segmentation */
//      try {
//        annotator = Main.process(trainer, headerTagger, input.getAbsolutePath)
////        annotator.write(output+"-blah")
//      } catch {
//        case e: Exception => updateErrs(e)
//      }
//
//      if (annotator != null) {
//        var fail = 0
//
//        /* annotate with paper-header */
//        val headerTxt = Main.getHeaderLines(annotator).mkString("\n")
//        println(headerTxt)
//        val headerDoc = new Document(headerTxt)
//        DeterministicTokenizer.process(headerDoc)
//        new Sentence(headerDoc.asSection, 0, headerDoc.tokens.size)
//        try {
//          headerTagger.process(headerDoc)
//        } catch {
//          case e: Exception =>
//            updateErrs(e)
//            fail = 1
//        }
//
//        /* annotate with bibie */
//        val refs = Main.getReferencesWithBreaks(annotator)
//        val refDocs = refs.map(ref => {
//          val doc = new Document(ref)
//          DeterministicTokenizer.process(doc)
//          new Sentence(doc.asSection, 0, doc.tokens.size)
//          doc.tokens.foreach(t => t.attr += new CitationLabel("", t))
//          doc
//        })
//
//        try {
//          TestCitationModel.process(refDocs, trainer, print=false)
//        } catch {
//          case e: Exception =>
//            updateErrs(e)
//            fail = 1
//        }
//
//        /* write to XML */
//        var xml =  ""
//        try {
//          xml = XMLParser.docsToXML(headerDoc, refDocs)
//          scala.xml.XML.loadString(xml)
//        } catch {
//          case e: Exception =>
//            updateErrs(e)
//            fail = 1
//        }
//        if (fail == 0) {
//          val pw = new PrintWriter(new File(output))
//          pw.write(xml)
//          pw.close()
//        } else {
//          failCount += 1
//        }
//      } else {
//        failCount += 1
//      }

//    }

//    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
//    println("exceptions:")
//    errAnalysis.foreach {
//      case (name, ct) => println(s"$name $ct")
//    }