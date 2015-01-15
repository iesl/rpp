package org.rexo.util
import scala.collection.mutable.HashSet
//import java.util.HashSet
import scala.collection.mutable.HashSet
import java.io.{IOException, FileReader, BufferedReader, File}
import scala.io.Source

/**
 * Created by klimzaporojets on 9/16/14.
 */
class EnglishDictionary {
  private val _words: HashSet[String] = new HashSet

  def create(words: File): EnglishDictionary = {
    val dict: EnglishDictionary = new EnglishDictionary
    try {
      for(line <- Source.fromFile(words).getLines()){
        dict._words.add(line.trim.toLowerCase)
      }
    }
    catch {
      case e: IOException => {
        throw new RuntimeException(e)
      }
    }
    return dict
  }

  def contains(word: String): Boolean = {
    return _words.contains(word)
  }

}

object EnglishDictionary{
  var _defaultWords:File = new File(getClass.getResource("/words.txt").getPath)
  def createDefault(_defaultWords:File): EnglishDictionary = {
    return (new EnglishDictionary).create(_defaultWords)
  }

  def setDefaultWordfile(f: File) {
    _defaultWords = f
  }
}
