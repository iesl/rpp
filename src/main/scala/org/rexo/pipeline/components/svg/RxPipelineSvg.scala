package org.rexo.pipeline.components.svg

import org.rexo.pipeline.components.svg.RxFilterSvd.ReturnCode

/**
 * @author asaunders
 */
class RxPipelineSvg {
  /**
   *
   */
  private val _scopeMap = collection.mutable.Map[Any, Any]() //new HashMap[_, _]
  private var _standardFilterList = collection.mutable.MutableList[RxFilterSvd]() //LinkedList[_] = new LinkedList[_]
  private var _errorFilterList = collection.mutable.MutableList[RxFilterSvd]() //LinkedList[_] = new LinkedList[_]
  private var _epilogueFilterList = collection.mutable.MutableList[RxFilterSvd]() //  LinkedList[_] = new LinkedList[_]
  private var _workingFilterList = collection.mutable.MutableList[RxFilterSvd]() // LinkedList[_] = _standardFilterList

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
  def setInputSource(source: RxDocumentSourceSvg) {
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
      getScope("session").put("metric.corpus.iteration.documents.succeeded.integer", 0)
      val iter: Iterator[_] = _documentSource.iterator
      returnCode = execute(iter)
      if (!continuous.booleanValue) {
        return // is return ok ?? // break //todo: break is not supported
      }
      iteration += 1
      getScope("session").put("metric.corpus.iteration.integer", iteration)
    }
  }

  def execute(iter: Iterator[_]): Int = {
    var returnCode: Int = ReturnCode.OK
    while (iter.hasNext && returnCode != ReturnCode.ABORT_SESSION) {
      val rdoc: RxDocumentSvg = iter.next.asInstanceOf[RxDocumentSvg]
      returnCode = execute(rdoc)
    }
    returnCode
  }

  def executeSvd(rdoc: RxDocumentSvg): Int = {
    var returnCode: Int = ReturnCode.OK
    rdoc.getScope("document").++=(getScope("document")) // putAll(getScope("document"))
    rdoc.setScope("session", getScope("session"))
    rdoc.setScope("global", getScope("global"))
    //    {
    var haveToBreak = false
    val filterIter = _standardFilterList.iterator
    while (!haveToBreak && filterIter.hasNext) {
      val nextFilter: RxFilterSvd = filterIter.next()
      returnCode = nextFilter.accept(rdoc)
      if (returnCode == ReturnCode.ABORT_PAPER || returnCode == ReturnCode.ABORT_SESSION) {
        haveToBreak = true //break //todo: break is not supported
      }
    }
    //    }
    if (returnCode != ReturnCode.OK && returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter = _errorFilterList.iterator
        while (filterIter.hasNext) {
          val errorFilter: RxFilterSvd = filterIter.next
          errorFilter.accept(rdoc)
        }
      }
    }
    if (returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter = _epilogueFilterList.iterator
        while (filterIter.hasNext) {
          val element: RxFilterSvd = filterIter.next
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

  def execute(rdoc: RxDocumentSvg): Int = {
    var returnCode: Int = ReturnCode.OK
    rdoc.getScope("document") ++= getScope("document")
    rdoc.setScope("session", getScope("session"))
    rdoc.setScope("global", getScope("global"))
    var haveToBreak = false
    val filterIter = _standardFilterList.iterator
    while (!haveToBreak && filterIter.hasNext) {
      val nextFilter = filterIter.next()
      returnCode = nextFilter.accept(rdoc)
      if (returnCode == ReturnCode.ABORT_PAPER || returnCode == ReturnCode.ABORT_SESSION) {
        haveToBreak = true
      }
    }
    if (returnCode != ReturnCode.OK && returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter = _errorFilterList.iterator
        while (filterIter.hasNext) {
          val errorFilter: RxFilterSvd = filterIter.next()
          errorFilter.accept(rdoc)
        }
      }
    }
    if (returnCode != ReturnCode.ABORT_SESSION) {
      {
        val filterIter = _epilogueFilterList.iterator
        while (filterIter.hasNext) {
          val element: RxFilterSvd = filterIter.next()
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
    returnCode
  }

  def addStandardFilters: RxPipelineSvg = {
    _workingFilterList = _standardFilterList
    this
  }

  def addErrorFilters: RxPipelineSvg = {
    _workingFilterList = _errorFilterList
    this
  }

  def addEpilogueFilters: RxPipelineSvg = {
    _workingFilterList = _epilogueFilterList
    this
  }

  def addPrologueFilters: RxPipelineSvg = {
    _workingFilterList = _epilogueFilterList
    this
  }

  /**
   * @param f
   * @return
   */
  def add(f: RxFilterSvd): RxPipelineSvg = {
    _workingFilterList += f
    f.init(this)
    this
  }

  override def toString: String = {
    val returnStringBuffer: StringBuffer = new StringBuffer
    returnStringBuffer.append("{ standard: ")
    var filterIter = _standardFilterList.iterator
    while (filterIter.hasNext ) {
      val filter: RxFilterSvd = filterIter.next()
      returnStringBuffer.append(filter.toString)
      returnStringBuffer.append(" | ")
    }
//    {
//      val filterIter = _standardFilterList.iterator
//      while (filterIter.hasNext ) {
//        val filter: RxFilterSvd = filterIter.next.asInstanceOf[RxFilterSvd]
//        returnStringBuffer.append(filter.toString)
//        returnStringBuffer.append(" | ")
//      }
//    }
    returnStringBuffer.append("} || ")
    returnStringBuffer.append("{ epilogue: ")

    filterIter = _epilogueFilterList.iterator

    while (filterIter.hasNext) {
        val filter: RxFilterSvd = filterIter.next()
        returnStringBuffer.append(filter.toString)
        returnStringBuffer.append(" | ")
      }

    returnStringBuffer.append(" } || ")
    returnStringBuffer.append("{ error: ")

    filterIter = _errorFilterList.iterator
      while (filterIter.hasNext) {
        val filter: RxFilterSvd = filterIter.next()
        returnStringBuffer.append(filter.toString)
        returnStringBuffer.append(" | ")
      }

    returnStringBuffer.append("}")
    returnStringBuffer.toString
  }

  private var _documentSource: RxDocumentSourceSvg = null

}
