package rpp 

import java.io.File
import org.jdom2.Content
import org.jdom2.util.IteratorIterable
import scala.collection.immutable.Queue
import scala.collection.JavaConversions.iterableAsScalaIterable 

import scala.collection.immutable.IntMap
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import annotator.Annotator 

object LineProcessor extends Processor {
  import Annotator._

  override def process(annotator: Annotator): Annotator =  {

    def isSameLine(e1: Element, e2: Element): Boolean = {
      val lineThreshold = 7.0
      val ancestor = Annotator.commonAncestor(e1, e2)
      val PositionGroup(_, _, e1Ys) = Annotator.getTransformedCoords(e1, ancestor)
      val PositionGroup(_, _, e2Ys) = Annotator.getTransformedCoords(e2, ancestor)
      Math.abs(e1Ys(0) - e2Ys(0)) < lineThreshold 
    }

    val lineList = annotator.getElements().foldLeft(Queue[Queue[Element]]())((queueAcc, e) => {
      queueAcc.lastOption match {
        case Some(currentLine) if isSameLine(e, currentLine.last) => 
          queueAcc.init.enqueue {
            queueAcc.last.enqueue(e)
          }
        case _ => 
          queueAcc.enqueue(Queue(e))
      }
    }).map(_.toList).toList

    //Line By Char 

    def firstAndLast(e: Element, ee: Element) = {
      val eeLast = ee.getText().size - 1
      (
        ((1 until e.getText().size).foldLeft(IntMap[Label]())((annoMap, i) => {
          annoMap + (i -> I)
        }) + (0 -> B('l')) ),
        ( (0 until eeLast).foldLeft(IntMap[Label]())((annoMap, i) => {
          annoMap + (i -> I) 
        }) + (eeLast -> L) )
      )
    }

    val labelMapSeq = lineList.toIndexedSeq.flatMap(line => {
      line match {
        case e::Nil => 
          val lastIndex = e.getText().size - 1
          IndexedSeq(
            if (lastIndex == 0) {
              IntMap(0 -> U('l')) 
            } else {
              (1 until lastIndex).foldLeft(IntMap[Label]())((annoMap, i) => {
                annoMap + (i -> I)
              }) + (lastIndex -> L) + (0 -> B('l'))
            }
          )
        case e::ee::Nil => 
          val fl = firstAndLast(e, ee)
          IndexedSeq(fl._1, fl._2)
        case es => 
          val first = es.head
          val tail = es.tail
          val middle = tail.init
          val last = tail.last
          val fl = firstAndLast(first, last) 
          fl._1 +: middle.toIndexedSeq.map(e => {
            (0 until e.getText().size).foldLeft(IntMap[Label]())((annoMap, i) => {
              annoMap + (i -> I)
            })
          }) :+ fl._2
      }
    })

    val table = labelMapSeq.zipWithIndex.flatMap {
      case (labelMap, blockIndex) =>
        labelMap.map {
          case (charIndex, label) =>
            (blockIndex, charIndex) -> label
        }
    } toMap

    annotator.annotate(List("line" -> 'l'), Single(CharCon), table)

  }


}
