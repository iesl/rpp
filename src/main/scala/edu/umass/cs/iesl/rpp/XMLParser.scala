package edu.umass.cs.iesl.rpp

/**
 * @author Kate Silverstein 
 *         created on 4/1/15
 */
import cc.factorie.app.nlp._
import edu.umass.cs.iesl.bibie._
import scala.collection.mutable.{ArrayBuffer, Stack}


class TreeNode(val tag: String, val value: String = null, val parent: TreeNode = null) {
  val children = new ArrayBuffer[TreeNode]()
  var discovered: Boolean = false
  def isLeaf: Boolean = children.isEmpty
  def addChild(n: TreeNode): Unit = children += n
  def repr: String = s"(<$tag>${if (value != null) value}</$tag>)"
  override def toString: String = if (value != null) value else ""
}

class XMLTree(root: TreeNode) {
  def search(tag: String): Option[TreeNode] = {
    val S = new Stack[TreeNode]()
    S.push(root)
    while (S.nonEmpty) {
      val v = S.pop()
      if (v.tag == tag) { resetAll(); return Some(v) }
      if (!v.discovered && (v.tag != tag)) {
        v.discovered = true
        S.pushAll(v.children)
      }
    }
    resetAll()
    None
  }

  def resetAll(): Unit = {
    val S = new Stack[TreeNode]()
    S.push(root)
    while (S.nonEmpty) {
      val v = S.pop()
      if (v.discovered) {
        v.discovered = false
        S.pushAll(v.children)
      }
    }
  }

  def addNodes(tagChunk: String, string: String): Unit = {
    // search tags specific --> general; add node to most specific non-leaf node
    val tags = tagChunk.split(":").reverse
    var done = false
    var i = 0
    while (!done && i < tags.length) {
      val curr = tags(i)
      search(curr) match {
        case Some(node) =>
          // if the node found is a leaf, attach the new node to its parent
          // otherwise, just add the new node to the found node's children
          val parent = if (node.isLeaf) node.parent else node
          parent.addChild(new TreeNode(tags.head, string, parent))
          done = true
        case None =>
      }
      i += 1
    }
    // if we didnt find anywhere to attach, add the necessary subtree
    if (!done) {
      // unreverse tags (want to add general --> specific in this case)
      val tagsUnrev = tags.reverse
      var prev = root
      i = 0
      while (i < tagsUnrev.length - 1) {
        val n = new TreeNode(tagsUnrev(i), null, prev)
        prev.addChild(n)
        prev = n
        i += 1
      }
      prev.addChild(new TreeNode(tagsUnrev.last, string, prev))
    }
  }

  def preorderTraversalString(n: TreeNode): String = {
    n.discovered = true
    if (n.isLeaf) s"<${n.tag}>${n.value}</${n.tag}>"
    else {
      var result = s"<${n.tag}>"
      for (c <- root.children if !c.discovered) {
        val subtree = new XMLTree(c)
        result += subtree.preorderTraversalString(c)
      }
      result += s"</${n.tag}>"
      result
    }
  }

  override def toString: String = preorderTraversalString(root)

}

object XMLParser {
  def getTags(token: Token): Seq[String] = token.attr[CitationLabel].categoryValue.split(":").map(t => t.drop(2))
  def getLastTag(token: Token): String = token.attr[CitationLabel].categoryValue.split(":").last
  def fromBibieReferenceDocument(doc: Document): String = {
    val tokens = doc.tokens.toIndexedSeq

    // pre-process by grouping tokens with the same "specific" tag into single strings
    // e.g. <authors><person><person-first>Emily</person-first><person-first> . </person-first> becomes
    // <authors><person><person-first>Emily . </person-first> ...
    val grouped = new ArrayBuffer[(String, String)]()
    var lastSeen = getLastTag(tokens.head)
    var lastSeenFull = getTags(tokens.head).mkString(":")
    var i = 1
    var currChunk = tokens.head.string
    while (i < tokens.length) {
      val curr = getLastTag(tokens(i))
      if ((curr.drop(2) == lastSeen.drop(2)) && curr.take(2) == "I-") {
        currChunk += " " + tokens(i).string
      } else {
        val pair: (String, String) = (lastSeenFull, currChunk)
        grouped += pair
        currChunk = tokens(i).string
        lastSeen = curr
        lastSeenFull = getTags(tokens(i)).mkString(":")
      }
      i += 1
    }

    val root = new TreeNode("reference")
    val tree = new XMLTree(root)

    grouped.foreach {
      case (tag, chunk) =>
        println(tag + " --> " + chunk)
        tree.addNodes(tag, chunk)
    }

    tree.toString
  }
}





