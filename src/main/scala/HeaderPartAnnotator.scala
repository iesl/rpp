package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import scala.io.Source
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import edu.umass.cs.iesl.paperheader.process
import edu.umass.cs.iesl.paperheader.crf

import edu.umass.cs.iesl.paperheader.crf.BioHeaderTag
import edu.umass.cs.iesl.paperheader.crf.LoadTSV

import annotator.Annotator 

object HeaderPartAnnotator {
  import Annotator._

  def addAnnotation(annotator: Annotator): Annotator =  {



    case class HeaderLayout(pairIndex: (Int, Int), token: String, x: Double, y: Double, fontSize: Double)

    val rootElement = annotator.getDom().getRootElement()

    //create index from (section idx) -> (bIndex, cIndex)
    val headerBIndexPairSet = annotator.getAnnotatableIndexPairSet(Single(SegmentCon("header")))

    val headerSet = headerBIndexPairSet.map {

      case (blockBIndex, charBIndex) =>
        val textMap = annotator.getTextMap("header")(blockBIndex, charBIndex)
        val elementMap = annotator.getElements("header")(blockBIndex, charBIndex)

        val pairIndexSeq = textMap.toIndexedSeq.flatMap {
          case (_blockIndex, text) =>
            val _charIndex = if (_blockIndex == blockBIndex) charBIndex else 0
            (0 until text.size).filter(i => {
              val currNotWhite = "\\s".r.findFirstIn(text.charAt(i).toString) == None
              currNotWhite && (
                i == 0 || ("\\s".r.findFirstIn(text.charAt(i - 1).toString) != None)
              )
            }).map(i => {
              val e = elementMap(_blockIndex)
              val (xs, _, ys) = Annotator.getTransformedCoords(e, rootElement)
              val x = HeaderLayout(
                _blockIndex -> (_charIndex + i),
                text.substring(i).split("\\s")(0),
                xs(i),
                ys(i),
                Annotator.fontSize(e)
              )
              println("x: " + x)
              x 
            })

        }

    }


    

    annotator
  }

  def run(): Unit =  {

    val str = {
"""
# 
Yale	2406	4923	-1
University	2406	4923	-1
Department	1638	4704	172
of	1638	4704	172
Computer	1638	4704	172
Science	1638	4704	172
P.O. 	2662	4168	133
Box	2662	4168	133
208205	2662	4168	133
NewHaven	2386	4032	133

# 
A	1176	6955	-1
Systematic	1176	6955	-1
Procedure	1176	6955	-1
for	1176	6955	-1
Applying	1176	6955	-1
Fast	1176	6955	-1
Correlation	938	6736	157
Attacks	938	6736	157
to	938	6736	157
Combiners	938	6736	157
with	938	6736	157
Memory	938	6736	157
M.	1446	6447	107
Salmasizadeh	1446	6447	107
J.	1446	6447	107
Golic	1446	6447	107
E.	1446	6447	107
Dawson	1446	6447	107
L.	1446	6447	107
Simpson	1446	6447	107
Information	1446	6447	107
Security	1446	6447	107
Research	1446	6447	107
Centre	1446	6447	107

#
"""
    }

    //"false" means you want to load this data as unlabeled data
    val docs = (new LoadTSV(false)).fromSource(Source.fromString(str))

    // process the documents with the CRF (the annotations are stored in token.attr[BioHeaderTag])
    process.DocProcessor(docs)

    //print some output if you want
    docs.take(5).foreach(doc => {
        val tokens = doc.sections.flatMap(_.tokens)
        tokens.take(5).foreach(token => {
          println(s"${token.string} ${token.attr[BioHeaderTag].categoryValue}")
        })
    })  

  }


}
