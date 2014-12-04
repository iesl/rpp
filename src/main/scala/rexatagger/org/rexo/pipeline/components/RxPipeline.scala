package org.rexo.pipeline.components

//import java.util.{HashMap, LinkedList, Iterator, Map}
//import scala.Predef.Map
import org.rexo.pipeline.components.RxFilter.ReturnCode
import scala.Iterator
import org.rexo.pipeline.components.{RxDocumentSource, RxFilter}
import org.rexo.pipeline.components.svg.RxDocumentSvg

/**
 * Created by klimzaporojets on 9/25/14.
 */
/**
 * @author asaunders
 */
class RxPipeline {
  /**
   *
   */
  private var _scopeMap: collection.mutable.Map[Any, Any] =  collection.mutable.Map[Any, Any]() //new HashMap[_, _]
  private var _standardFilterList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //LinkedList[_] = new LinkedList[_]
  private var _errorFilterList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //LinkedList[_] = new LinkedList[_]
  private var _epilogueFilterList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() //  LinkedList[_] = new LinkedList[_]
  private var _workingFilterList: collection.mutable.MutableList[Any] = collection.mutable.MutableList[Any]() // LinkedList[_] = _standardFilterList

  //  def this() {
//    this()
//    _scopeMap.put("global", collection.mutable.Map[String, AnyRef]() /*HashMap[String, AnyRef]*/)

    setScope("global", collection.mutable.Map[String, AnyRef]() /*HashMap[String, AnyRef]*/)
    setScope("session", collection.mutable.Map[String, AnyRef]()) //new HashMap[String, AnyRef])
    setScope("document", collection.mutable.Map[String, AnyRef]()) //new HashMap[String, AnyRef])
    getScope("session").put("pipeline.progress.iterations.integer", new Integer(0))
    getScope("session").put("pipeline.progress.last.write.integer", new Integer(0))
    getScope("session").put("continuous.execution.boolean", false /*new Boolean(false)*/ )
    getScope("session").put("errorlog.boolean", false /*new Boolean(false)*/)
//  }

  def getScope(scope: String): collection.mutable.Map[String, Any] = {
    return _scopeMap.get(scope).get.asInstanceOf[collection.mutable.Map[String, Any]]
  }

  def setScope(scope: String, scopeMap: collection.mutable.Map[String, AnyRef]) {
    _scopeMap.put(scope, scopeMap)
  }

  /**
   * @param source
   */
  def setInputSource(source: RxDocumentSource) {
    _documentSource = source
  }

  /**
   *
   */
  def execute {
    val continuous: Boolean = getScope("session").get("continuous.execution.boolean").asInstanceOf[Boolean]
    var returnCode: Int = ReturnCode.OK
    var iteration: Int = 1
    while (returnCode != ReturnCode.ABORT_SESSION) {
      getScope("session").put("metric.corpus.iteration.documents.succeeded.integer", new Integer(0))
      val iter: Iterator[_] = _documentSource.iterator
      returnCode = execute(iter)
      if (!continuous.booleanValue) {
        return // is return ok ?? // break //todo: break is not supported
      }
      getScope("session").put("metric.corpus.iteration.integer", new Integer(({
        iteration += 1; iteration
      })))
    }
  }

  def execute(iter: Iterator[_]): Int = {
    var returnCode: Int = ReturnCode.OK
    while (iter.hasNext && returnCode != ReturnCode.ABORT_SESSION) {
      val rdoc: RxDocument = iter.next.asInstanceOf[RxDocument]
      returnCode = execute(rdoc)
    }
    return returnCode
  }


  def execute(rdoc: RxDocument): Int = {
    var returnCode: Int = ReturnCode.OK
    rdoc.getScope("document").++=(getScope("document")) // putAll(getScope("document"))
    rdoc.setScope("session", getScope("session"))
    rdoc.setScope("global", getScope("global"))
//    {
      var haveToBreak = false
      val filterIter: Iterator[_] = _standardFilterList.iterator
      while (!haveToBreak && filterIter.hasNext) {
        val nextFilter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
        returnCode = nextFilter.accept(rdoc)
        if (returnCode == ReturnCode.ABORT_PAPER || returnCode == ReturnCode.ABORT_SESSION) {
          haveToBreak = true //break //todo: break is not supported
        }
      }
//    }
    if (returnCode != ReturnCode.OK && returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter: Iterator[_] = _errorFilterList.iterator
        while (filterIter.hasNext) {
          val errorFilter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
          errorFilter.accept(rdoc)
        }
      }
    }
    if (returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter: Iterator[_] = _epilogueFilterList.iterator
        while (filterIter.hasNext) {
          val element: RxFilter = filterIter.next.asInstanceOf[RxFilter]
          element.accept(rdoc)
        }
      }
    }
    if (returnCode == ReturnCode.ABORT_SESSION) {
      if (_documentSource != null) {
        _documentSource.closeSource(rdoc)
      }
    }
    else {
      if (_documentSource != null) {
        _documentSource.closeDocument(rdoc)
      }
    }
    return returnCode
  }

  def addStandardFilters: RxPipeline = {
    _workingFilterList = _standardFilterList
    return this
  }

  def addErrorFilters: RxPipeline = {
    _workingFilterList = _errorFilterList
    return this
  }

  def addEpilogueFilters: RxPipeline = {
    _workingFilterList = _epilogueFilterList
    return this
  }

  def addPrologueFilters: RxPipeline = {
    _workingFilterList = _epilogueFilterList
    return this
  }

  /**
   * @param f
   * @return
   */
  def add(f: RxFilter): RxPipeline = {
    _workingFilterList.+=(f) //.add(f)
    f.init(this)
    return this
  }

  override def toString: String = {
    val returnStringBuffer: StringBuffer = new StringBuffer
    returnStringBuffer.append("{ standard: ")
    var filterIter = _standardFilterList.iterator
    while (filterIter.hasNext ) {
      val filter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
      returnStringBuffer.append(filter.toString)
      returnStringBuffer.append(" | ")
    }
//    {
//      val filterIter = _standardFilterList.iterator
//      while (filterIter.hasNext ) {
//        val filter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
//        returnStringBuffer.append(filter.toString)
//        returnStringBuffer.append(" | ")
//      }
//    }
    returnStringBuffer.append("} || ")
    returnStringBuffer.append("{ epilogue: ")

      //val filterIter: Iterator[_] = _epilogueFilterList.iterator
    filterIter = _epilogueFilterList.iterator

    while (filterIter.hasNext) {
        val filter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
        returnStringBuffer.append(filter.toString)
        returnStringBuffer.append(" | ")
      }

    returnStringBuffer.append(" } || ")
    returnStringBuffer.append("{ error: ")

//      val filterIter: Iterator[_] = _errorFilterList.iterator
    filterIter = _errorFilterList.iterator
      while (filterIter.hasNext) {
        val filter: RxFilter = filterIter.next.asInstanceOf[RxFilter]
        returnStringBuffer.append(filter.toString)
        returnStringBuffer.append(" | ")
      }

    returnStringBuffer.append("}")
    return returnStringBuffer.toString
  }

  private var _documentSource: RxDocumentSource = null

}
