package org.rexo.extra.utils

import java.util.regex.{Matcher, Pattern}
import java.io.{ObjectOutputStream, ObjectInputStream}

/**
 * Created by klimzaporojets on 9/26/14.
 */
object CharSequenceLexer {


  final val LEX_ALPHA: Pattern = Pattern.compile("\\p{Alpha}+")
  final val LEX_WORDS: Pattern = Pattern.compile("\\w+")
  final val LEX_NONWHITESPACE_TOGETHER: Pattern = Pattern.compile("\\S+")
  final val LEX_WORD_CLASSES: Pattern = Pattern.compile("\\p{Alpha}+|\\p{Digit}+")
  final val LEX_NONWHITESPACE_CLASSES: Pattern = Pattern.compile("\\p{Alpha}+|\\p{Digit}+|\\p{Punct}")
  private final val serialVersionUID: Long = 1
  private final val CURRENT_SERIAL_VERSION: Int = 1

  def apply():CharSequenceLexer =
  {
    val csl:CharSequenceLexer = new CharSequenceLexer(null, LEX_ALPHA/*, null, null, false*/)
    csl.setCharSequence(csl.input)
    csl.matchTextFresh = false
//    csl.matcher =
    return csl;
  }
  def apply(regex:String):CharSequenceLexer =
  {
    val csl:CharSequenceLexer = new CharSequenceLexer(null, Pattern.compile(regex)/*, null, null, false*/)
    csl.setCharSequence(csl.input)
    csl.matchTextFresh = false
    return csl
  }

  def apply(input:CharSequence, regex:String):CharSequenceLexer =
  {
    val csl:CharSequenceLexer = new CharSequenceLexer(input, Pattern.compile(regex)/*, null, null, false*/)
    csl.setCharSequence(csl.input)
    csl.matchTextFresh = false
    return csl
  }

  def apply(input:CharSequence, regex:Pattern):CharSequenceLexer =
  {
    val csl:CharSequenceLexer = new CharSequenceLexer(input, regex/*, null, null, false*/)
    csl.setCharSequence(csl.input)
    csl.matchTextFresh = false
//    println ("the value for matchTextFresh is " + csl.matchTextFresh)
    return csl
  }

}

class CharSequenceLexer(var input:CharSequence, var regex:Pattern/*, var matcher:Matcher,
                         var matchText:String, var matchTextFresh:Boolean*/) extends Lexer with Serializable {
  var matcher: Matcher = null;
  var matchText: String = null;
  var matchTextFresh: Boolean = false;
//  this.matchTextFresh = false
//  setCharSequence(input)
//  this(CharSequenceLexer.LEX_ALPHA)
//
//  def this(regex: Pattern) {
//    this()
//    this.regex = regex
//    setCharSequence(null)
//  }
//
//  def this(regex: String) {
//    this()
//    `this`(Pattern.compile(regex))
//  }
//
//  def this(input: CharSequence, regex: Pattern) {
//    this()
//    `this`(regex)
//    setCharSequence(input)
//  }
//
//  def this(input: CharSequence, regex: String) {
//    this()
//    `this`(input, Pattern.compile(regex))
//  }

  def setCharSequence(input: CharSequence) = {
    this.input = input
    this.matchText = null
    this.matchTextFresh = false
    if (input != null) this.matcher = regex.matcher(input)
  }

  def getCharSequence: CharSequence = {
    return input
  }

  def getPattern: String = {
    return regex.pattern
  }

  def setPattern(reg: String) = {
//    if (!(regex == getPattern)) {
      this.regex = Pattern.compile(reg)
//    }
  }

  def getStartOffset: Int = {
    if (matchText == null) return -1
    return matcher.start
  }

  def getEndOffset: Int = {
    if (matchText == null) return -1
    return matcher.end
  }

  def getTokenString: String = {
    return matchText
  }

  private def updateMatchText ():Unit = {
//    println("about to ubdate")
    if (matcher != null && matcher.find) {
      matchText = matcher.group
      if (matchText.length == 0) {
        updateMatchText
      }
    }
    else
    {
      matchText = null
    }
    matchTextFresh = true
  }

  def hasNext: Boolean = {
    if (!matchTextFresh) {
      updateMatchText
    }
    return (matchText != null)
  }

  def next: AnyRef = {
    if (!matchTextFresh) {
      updateMatchText
    }
    matchTextFresh = false
    return matchText
  }

  def remove = {
    throw new UnsupportedOperationException
  }

  private def writeObject(out: ObjectOutputStream) = {
    out.writeInt(CharSequenceLexer.CURRENT_SERIAL_VERSION)
    if (CharSequenceLexer.CURRENT_SERIAL_VERSION == 0) out.writeObject(regex)
    else if (CharSequenceLexer.CURRENT_SERIAL_VERSION == 1) {
      out.writeObject(regex.pattern)
      out.writeInt(regex.flags)
    }
    out.writeBoolean(matchTextFresh)
  }

  private def readObject(in: ObjectInputStream) = {
    val version: Int = in.readInt
    if (version == 0) regex = in.readObject.asInstanceOf[Pattern]
    else if (version == 1) {
      val p: String = in.readObject.asInstanceOf[String]
      val flags: Int = in.readInt
      regex = Pattern.compile(p, flags)
    }
    matchTextFresh = in.readBoolean
  }

//  private[utils] var regex: Pattern = null
}
