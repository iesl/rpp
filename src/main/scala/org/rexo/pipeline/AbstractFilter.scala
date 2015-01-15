package org.rexo.pipeline

import org.rexo.pipeline.components.{RxPipeline, RxDocument, RxFilter}
import java.io.{IOException, FileWriter, PrintWriter, File}
//import java.util.Map
//import scala.Predef.Map
import org.rexo.pipeline.components.RxFilter.ReturnCode
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
 * Created by klimzaporojets on 9/26/14.
 */
abstract class AbstractFilter extends RxFilter {
//  public def this() {
//    this()
//  }

  protected def getLogger(rdoc: RxDocument): Logger = {
    //return Logger.getLogger(this.getClass)
    Logger(LoggerFactory.getLogger("ScalaTagger"))
  }

//  protected def appendLogFile(rdoc: RxDocument, name: String, extension: String, text: String) {
//    val logFile: File = LogfileFactory.getLogfile(rdoc, name, extension)
//    writeLogFile(rdoc, logFile, text, true)
//  }
//
//  protected def clearLogFile(rdoc: RxDocument, name: String, extension: String) {
//    val logFile: File = LogfileFactory.getLogfile(rdoc, name, extension)
//    writeLogFile(rdoc, logFile, null, false)
//  }

  private def writeLogFile(rdoc: RxDocument, logFile: File, text: String, append: Boolean) {
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

  protected def incrementIntAttribute(rdoc: RxDocument, scope: String, attribute: String) {
    setIntAttribute(rdoc, scope, attribute, getIntAttribute(rdoc, scope, attribute) + 1)
  }

  protected def getIntAttribute(rdoc: RxDocument, scope: String, attribute: String): Int = {
    val scopeMap: collection.mutable.Map[Any, Any] = rdoc.getScope(scope)
    val value: Integer = scopeMap.get(attribute).asInstanceOf[Integer]
    if (value == null) {
      scopeMap.put(attribute, new Integer(0))
      return 0
    }
    return value.intValue
  }

  protected def setIntAttribute(rdoc: RxDocument, scope: String, attribute: String, value: Int) {
    val scopeMap: collection.mutable.Map[Any, Any] = rdoc.getScope(scope)
    scopeMap.put(attribute, new Integer(value))
  }

  def init(pipeline: RxPipeline): Int = {
    return ReturnCode.OK
  }
}
