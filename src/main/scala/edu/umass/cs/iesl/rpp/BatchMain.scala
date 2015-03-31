package edu.umass.cs.iesl.rpp

import edu.umass.cs.iesl.bibie.TestCitationModel
import edu.umass.cs.iesl.paperheader.crf._
import edu.umass.cs.iesl.xml_annotator._
import scala.collection.mutable.ArrayBuffer

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
    val annotatorsWithOutputFile = new ArrayBuffer[(Annotator, String)]()
    val tags = Set("institution", "address", "title", "author", "tech", "date", "note", "email").map("header-" + _) ++ Set("abstract")

    inputFiles.zip(outputFiles).take(5).foreach({ case (input, output) =>
      try {
        println(s"processing: $input")
        val annotator = Main.process(trainer, headerTagger, input)//.write(output)
        val tup: (Annotator, String) = (annotator, output)
        annotatorsWithOutputFile += tup
      } catch {
        case e: Exception =>
          println(s"failed to process file: $input")
          e.printStackTrace()
          failCount += 1
      }
      totalCount += 1
    })

    annotatorsWithOutputFile.foreach {
      case (annotator, outputFile) =>
        val annots = Main.getAllAnnotationTypes(annotator)
        for (ann <- annots) println(ann)
    }
    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
  }
}
