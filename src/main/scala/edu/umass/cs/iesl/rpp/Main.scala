package edu.umass.cs.iesl.rpp

import java.io.File
import org.jdom2.input.SAXBuilder
import edu.umass.cs.iesl.xml_annotator.Annotator
import scala.compat.Platform
import Annotator._
import edu.umass.cs.iesl.bibie._
import edu.umass.cs.iesl.paperheader.tagger.HeaderTagger
import scala.collection.immutable.SortedSet

object Main {

  def main(args: Array[String]): Unit = {
    println("main(): args: " + args.mkString(", "))
    val referenceModelUri = args(0)
    val headerTaggerModelFile = args(1)
    val inFilePath = args(2)
    val outFilePath = args(3)
    val lexiconUrlPrefix = getClass.getResource("/lexicons").toString
    val trainer = TestCitationModel.loadModel(referenceModelUri, lexiconUrlPrefix)

    val headerTagger = new HeaderTagger
    headerTagger.deSerialize(new java.io.FileInputStream(headerTaggerModelFile))

    val annotator = process(trainer, headerTagger, inFilePath).write(outFilePath)
//    Main.printExampleQueries(annotator, inFilePath)
    val outputStr = Main.coarseOutputStrForAnnotator(annotator, inFilePath)
    println(outputStr)
  }


  def process(trainer: CitationCRFTrainer, headerTagger: HeaderTagger, inFilePath: String): Annotator = {
    val builder = new SAXBuilder()
    val dom = builder.build(new File(inFilePath))

    val l = List(
      LineProcessor,
      StructureProcessor //,
      //HeaderPartProcessor(headerTagger),
      //ReferencePartProcessor(trainer),
      //CitationProcessor,
      //CitationReferenceLinkProcessor
    )

    val annotator = l.foldLeft(Annotator(dom)) {
      case (annoAcc, pro) => pro.process(annoAcc)
    }

    annotator
  }

  def coarseOutputStrForAnnotator(annotator: Annotator, inFilePath: String): String = {
    // collect each query function's Seq[String] into a single string
    val outputSB = new StringBuilder()
    outputSB.append(s"\n;; -*- mode: outline -*-\n* ---- example queries: $inFilePath ----")

    outputSB.append("\n** ---- header lines ----")
    getHeaderLines(annotator).foreach(outputSB.append)

    outputSB.append("\n** ---- references with breaks ----")
    getReferencesWithBreaks(annotator).foreach(outputSB.append)

    outputSB.append("\n** ---- done ----")

    outputSB.toString()
  }


  def printExampleQueries(annotator: Annotator, inFilePath: String) = {
    println(s"* ---- example queries: $inFilePath ----")

    println("** ---- header lines ----")
    getHeaderLines(annotator).foreach(println(_))

    println("** ---- references ----")
    getReferences(annotator).foreach(println(_))

    println("** ---- references with breaks ----")
    getReferencesWithBreaks(annotator).foreach(println(_))

    println("** ---- lines of references ----")
    getLinesOfReferences(annotator).foreach(println(_))

    println("** ---- lines ----")
    getLines(annotator).foreach(println(_))

    println("** ---- done ----")
  }


  //
  // top-level queries
  //

  def getLines(annotator: Annotator): Seq[String] = {
    annotator.getTextSet("line").map(_._2).toSeq
  }


  def getReferences(annotator: Annotator): Seq[String] = {
    annotator.getTextSet("biblio-marker").map(_._2).toSeq
  }


  def getReferencesWithBreaks(annotator: Annotator): Seq[String] = {
    val biblioBIndexSet = annotator.getBIndexSetByAnnotationType("biblio-marker")
    val lineBIndexSet = annotator.getFilteredBIndexSet("biblio-marker", "line")
    biblioBIndexSet.toList.flatMap { case (index) =>
      annotator.getTextOption("biblio-marker")(index) map { case (startIndex, str) =>
        Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex))
      }
    }
  }


  def getLinesOfReferences(annotator: Annotator): Seq[String] = {
    //this is possible in such a way because biblio-marker is constrained by line
    annotator.getFilteredTextSet("biblio-marker", "line").map(_._2).toSeq
  }


  def getHeaderLines(annotator: Annotator): Seq[String] = {
    val biblioBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header")))
    val lineBIndexSet = annotator.getBIndexSet(Range("header", SegmentCon("line")))

    biblioBIndexSet.toList.flatMap { case (index) =>
      annotator.getTextOption("header")(index) map { case (startIndex, str) =>
        Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex))
      }
    }
  }


  def getAllAnnotationTypes(annotator: Annotator): Seq[String] = {
    annotator.annotationInfoMap.map { case (annoTypeString, annotationInfo) =>
      annoTypeString
    } toSeq
  }


  //
  // lower-level queries
  //

  def getCitationsAndRefMarkers(annotator: Annotator): Seq[(String, String)] = {
    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").map(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citIndex) = linkMap("cit")
      val (refMarkerString, refIndex) = linkMap("ref")

      val citString = annotator.getTextOption(citationString)(citIndex).map(_._2).getOrElse("")
      val refString = annotator.getTextOption(refMarkerString)(refIndex).map(_._2).getOrElse("")
      (citString, refString)

    }).toSeq
  }


  def getCitationsAndReferences(annotator: Annotator): Seq[(String, String)] = {

    val bibMarkIndexSeq = annotator.getBIndexSet(Single(SegmentCon("biblio-marker"))).toIndexedSeq
    val lineBIndexSet = annotator.getBIndexSet(Range("biblio-marker", SegmentCon("line")))

    annotator.annotationLinkSet.filter(_.name == "citation-reference-link").flatMap(annoLink => {
      val linkMap = annoLink.attrValueMap
      val (citationString, citIndex) = linkMap("cit")
      val (refMarkerString, refIndex) = linkMap("ref")

      val citString = annotator.getTextOption(citationString)(citIndex).map(_._2).getOrElse("")

      val bibMarkerIndex = bibMarkIndexSeq.lastIndexWhere(_ <= refIndex)
      val bibMarkerTotalIndex = bibMarkIndexSeq(bibMarkerIndex)

      annotator.getTextOption("biblio-marker")(bibMarkerTotalIndex).map { case (startIndex, str) =>
        val refString = mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex), '\n')
        (citString, refString)
      }

    }).toSeq
  }


  def getCitationAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexSet = annotator.getBIndexSet(Single(SegmentCon(tag)))
    val tagTokenBIndexSet = annotator.getBIndexSet(Range(tag, SegmentCon("reference-token")))
    tagTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndex) => {
      val tagToken: String = annotator.getTextOption("reference-token")(tokenIndex).map(_._2).getOrElse("")
      if (tagBIndexSet.contains(tokenIndex)) {
        List(tagToken) :: listAcc
      } else {
        (tagToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }


  //  def getAnnotatedReferences(annotator: Annotator): List[List[(String, String)]] = {
  //    val refTags = getAllAnnotationTypes(annotator).filter(t => t.startsWith("ref-"))
  //    val refTokenBIndexSet = annotator.getBIndexSet(Single(SegmentCon("reference-token")))
  //    val allAnnots = new scala.collection.mutable.ArrayBuffer[List[(String, String)]]()
  //    refTokenBIndexSet.toList.foreach {
  //      case (bi, ci) =>
  //        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
  //        val title = annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
  //        val titlePair: (String, String) = ("ref-title", title)
  //        annots += titlePair
  //        allAnnots += annots.toList
  //    }
  ////    refTokenBIndexSet.toList.foreach {
  ////      case (bi, ci) =>
  ////        val annots = new scala.collection.mutable.ArrayBuffer[(String, String)]()
  ////        val titleSeg = annotator.getSegment("ref-title")(bi, ci)
  ////
  //////        val title = titleSeg.toList.flatMap { case (bi, labelMap) =>
  //////            labelMap.map { case (ci, label) =>
  //////                annotator.getTextMap("ref-title")(bi, ci).values.map(_._2).mkString("")
  //////            }
  //////        }
  ////        val titlePair: (String, String) = ("ref-title", title.mkString(" "))
  ////        annots += titlePair
  //////        val titleTm = annotator.getTextMap("ref-title")(bi, ci)
  //////        val title: (String, String) = ("ref-title", titleTm.values.map(_._2).mkString(""))
  //////        annots += title
  ////        allAnnots += annots.toList
  ////    }
  //    allAnnots.toList
  //  }


  //def getHeaderTokens(annotator: Annotator): Seq[Seq[String]] = {
  //  val headerTokenBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header-token")))
  //  headerTokenBIndexSet.toList.map(pair => {
  //    val (blockIdx, charIdx) = pair
  //    val seg = annotator.getSegment("header-token")(blockIdx, charIdx)
  //    seg.toList.flatMap{ case (bi, labelMap) =>
  //      labelMap.map{ case (ci, label) =>
  //        annotator.getTextMap("header-token")(bi, ci).values.map(_._2).mkString("")
  //      }
  //    }
  //  })
  //}


  def getAuthorNames2(annotator: Annotator): Seq[Seq[String]] = {
    val authorBIndexSet = annotator.getBIndexSet(Single(SegmentCon("header-author")))
    val authorTokenBIndexSet = annotator.getBIndexSet(Range("header-author", SegmentCon("header-token")))

    authorTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenIndexPair) => {
      val authorToken: String = annotator.getTextOption("header-token")(tokenIndexPair).map(_._2).getOrElse("")
      if (authorBIndexSet.contains(tokenIndexPair)) {
        List(authorToken) :: listAcc
      } else {
        (authorToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }


  def getAllAnnotations(annotator: Annotator): Seq[(String, String)] = {
    val lineBIndexSet = annotator.getBIndexSet(Single(SegmentCon("line")))
    annotator.annotationInfoMap.keys.flatMap(annoTypeString => {
      val bIndexPairSet = annotator.getBIndexSet(Single(SegmentCon(annoTypeString)))
      bIndexPairSet.toList.flatMap { case index =>
        annotator.getTextOption(annoTypeString)(index) map { case (startIndex, str) =>
          (annoTypeString, Annotator.mkTextWithBreaks(str, lineBIndexSet.map(_ - startIndex), ' ').trim())
        }
      }
    }).toSeq
  }


  def getHeaderAnnotationsByTag(annotator: Annotator, tag: String): List[List[String]] = {
    val tagBIndexSet = annotator.getBIndexSet(Single(SegmentCon(tag)))
    val tagTokenBIndexSet = annotator.getBIndexSet(Range(tag, SegmentCon("header-token")))
    tagTokenBIndexSet.foldLeft(List.empty[List[String]])((listAcc, tokenBIndex) => {
      val tagToken: String = annotator.getTextOption("header-token")(tokenBIndex).map(_._2).getOrElse("")
      if (tagBIndexSet.contains(tokenBIndex)) {
        List(tagToken) :: listAcc
      } else {
        (tagToken :: listAcc.head) :: listAcc.tail
      }
    }).reverse.map(_.reverse)
  }


  def printXML(annotator: Annotator) = {
    import HeaderPartProcessor._
    println {
      annotator.getAnnotationByTypeString(headerAuthor)
    }

    def lineBreak(pair: (Int, String)) = {
      val (offset, text) = pair
      val lineBIndexSet = annotator.getBIndexSetByAnnotationType("line")
      Annotator.mkTextWithBreaks(text, lineBIndexSet.map(_ - offset), ' ')
    }

    annotator.getRangeSet("header").foreach(headerRange => {
      println("<header>")
      List(headerTitle, headerAffiliation, headerAddress, headerEmail, headerDate, headerAbstract).foreach(annoType => {
        val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(headerRange)
        val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
        annos.map(t => println("  <" + annoType + ">" + t.trim + "</" + annoType + ">"))
      })

      println("  <authors>")
      val authorBIndexSet = annotator.getBIndexSetWithinRange(headerAuthor)(headerRange)
      authorBIndexSet.map(ai => {
        println("    <person>")
        val tokens = annotator.getRange(headerAuthor)(ai).toList.flatMap(authorRange => {
          val tokenBIndexSet = annotator.getFilteredBIndexSetWithinRange(headerAuthor, headerToken)(authorRange)
          tokenBIndexSet.toList.flatMap(tokenIndex => annotator.getTextOption(headerToken)(tokenIndex).map(lineBreak _))
        })
        tokens.foreach(t => println("      <person-token>" + t.trim + "</person-token>"))
        println("    </person>")
      })
      println("  </authors>")

      println("</header>")
    })

    annotator.getTextSet(headerEmail).map(l => println(l))

    import ReferencePartProcessor._
    println {
      annotator.getAnnotationByTypeString(refAuthorsString)
    }
    println {
      annotator.getAnnotationByTypeString(refPersonString)
    }
    println {
      annotator.getAnnotationByTypeString(refFirstString)
    }
    println {
      annotator.getAnnotationByTypeString(refLastString)
    }


    annotator.getRangeSet("reference").foreach(refsRange => {
      println("<references>")
      val refBIndexSet = annotator.getBIndexSetWithinRange("biblio-marker")(refsRange)
      refBIndexSet.foreach(refIndex => {
        println("  <reference>")


        annotator.getRange("biblio-marker")(refIndex).map(refRange => {

          List(
            refTitleString
          ).foreach(annoType => {
            val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(refRange)
            val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
            annos.map(t => println("    <" + annoType + ">" + t.trim + "</" + annoType + ">"))
          })

          val authorsBIndexSet = annotator.getBIndexSetWithinRange(refAuthorsString)(refRange)
          authorsBIndexSet.foreach(asi => {
            annotator.getRange(refAuthorsString)(asi).map(authorsRange => {
              println("    <authors>")

              val personBIndexSet = annotator.getBIndexSetWithinRange(refPersonString)(refRange)
              personBIndexSet.foreach(pi => {
                annotator.getRange(refPersonString)(pi).map(personRange => {
                  println("      <person>")
                  List(
                    refFirstString, refLastString
                  ).foreach(annoType => {
                    val bIndexSet = annotator.getBIndexSetWithinRange(annoType)(personRange)
                    val annos = bIndexSet.flatMap(i => annotator.getTextOption(annoType)(i).map(lineBreak _)).take(1)
                    annos.map(t => println("        <" + annoType + ">" + t.trim + "</" + annoType + ">"))
                  })
                  println("      </person>")
                })
              })
              println("    </authors>")
            })
          })
        })
        println("  </reference>")
      })
      println("</references>")
    })
  }


}