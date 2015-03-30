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
    import java.io.File
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
    val annotators = new ArrayBuffer[Annotator]()
    val tags = Set("institution", "address", "title", "author", "tech", "date", "note", "email").map("header-" + _) ++ Set("abstract")

    inputFiles.zip(outputFiles).take(5).foreach({ case (input, output) =>
      try {
        val annotator = Main.process(trainer, headerTagger, input)//.write(output)
        val annots = tags.flatMap(t => Main.getAnnotationsByTag(annotator, t))
        annots.foreach(ann => println(ann.mkString(", ")))
//        val annots = Main.getAllAnnotations(annotator)
//        annots.foreach{case (u, v) => println(List(u, v).mkString(" "))}
//        val authorLists = Main.getAuthorNames2(annotator)
//        authorLists.foreach(al => println(al.mkString(" ")))
//        val headerTokenLists = Main.getHeaderTokens(annotator)
//        headerTokenLists.foreach(hl => println(hl.mkString(" ")))
//        annotators += annotator
//        annotator.write(output)
      } catch {
        case e: Exception =>
          println(s"failed to process file: $input")
          e.printStackTrace()
          failCount += 1
      }
      totalCount += 1
    })

//    for (annotator <- annotators; tag <- tags) {
//      import Annotator._
//      val pairSet = annotator.getBIndexPairSet(Single(SegmentCon(tag)))
//      val annotationList: List[List[String]] = pairSet.toList.map(pair => {
//        val (blockIdx, charIdx) = pair
//        val seg = annotator.getSegment(tag)(blockIdx, charIdx)
//        seg.toList.flatMap{ case (bi, labelMap) =>
//          labelMap.map{ case (ci, label) =>
//            annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
//          }
//        }
//      })
//      println("annotations:")
//      for (annoList <- annotationList; anno <- annoList) println(anno)
//    }
//
//    /*
//      val headerInstitution = "header-institution"
//  val headerAddress = "header-address"
//  val headerTitle = "header-title"
//  val headerAuthor = "header-author"
//  val headerTech = "header-tech"
//  val headerDate = "header-date"
//  val headerNote = "header-note"
//  val headerAbstract = "abstract"
//  val headerEmail = "header-email"
//     */
    println(s"processed ${totalCount - failCount} out of $totalCount files ($failCount failures)")
  }
}
