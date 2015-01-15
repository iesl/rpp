package org.rexo.pipeline.svg

import org.rexo.pipeline.components.{RxDocument, RxFilter}
import java.io.{IOException, PrintWriter, FileWriter, File}
import org.rexo.pipeline.components.svg.{RxPipelineSvg, RxDocumentSvg, RxFilterSvd}
import org.rexo.pipeline.components.svg.RxFilterSvd.ReturnCode

//import java.util.Map
//import scala.Predef.Map
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
 * Created by klimzaporojets on 9/26/14.
 */
abstract class AbstractFilterSvg extends RxFilterSvd {


  protected def getLogger(rdoc: RxDocumentSvg): Logger = {
    Logger(LoggerFactory.getLogger("ScalaTagger"))
  }

  private def writeLogFile(rdoc: RxDocumentSvg, logFile: File, text: String, append: Boolean) {
    if (!logFile.getParentFile.exists) {
      if (!logFile.getParentFile.mkdirs) {
        getLogger(rdoc).error("writeLogFile: unable to create directory " + logFile.getParentFile)
        return
      }
    }
    if (!logFile.exists) {
      try {
        getLogger(rdoc).info("writeLogFile: creating file " + logFile)
        if (!logFile.createNewFile) {
          getLogger(rdoc).error("writeLogFile: couldn't create log file " + logFile)
          return
        }
      }
      catch {
        case e: IOException => {
          getLogger(rdoc).error("writeLogFile: " + e.getMessage)
          return
        }
      }
    }
    try {
      val w: PrintWriter = new PrintWriter(new FileWriter(logFile, append))
      if (text != null) {
        w.println(text)
      }
      w.close
    }
    catch {
      case e: IOException => {
        getLogger(rdoc).error(e.getMessage)
      }
    }
  }

  protected def incrementIntAttribute(rdoc: RxDocumentSvg, scope: String, attribute: String) {
    setIntAttribute(rdoc, scope, attribute, getIntAttribute(rdoc, scope, attribute) + 1)
  }

  protected def getIntAttribute(rdoc: RxDocumentSvg, scope: String, attribute: String): Int = {
    val scopeMap: collection.mutable.Map[Any, Any] = rdoc.getScope(scope)
    val value: Integer = scopeMap.get(attribute).asInstanceOf[Integer]
    if (value == null) {
      scopeMap.put(attribute, new Integer(0))
      return 0
    }
    return value.intValue
  }

  protected def setIntAttribute(rdoc: RxDocumentSvg, scope: String, attribute: String, value: Int) {
    val scopeMap: collection.mutable.Map[Any, Any] = rdoc.getScope(scope)
    scopeMap.put(attribute, new Integer(value))
  }

  def init(pipeline: RxPipelineSvg): Int = {
    return ReturnCode.OK
  }
}
