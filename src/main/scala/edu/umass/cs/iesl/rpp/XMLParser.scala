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
  def openTag: String = s"<$tag>"
  def closeTag: String = s"</$tag>"
  def isLeaf: Boolean = children.isEmpty
  def isRoot: Boolean = parent == null
  def addChild(n: TreeNode): Unit = children += n
  def repr: String = s"(<$tag>${if (value != null) value}</$tag>)"
  override def toString: String = if (value != null) value else "" //if (isLeaf && value != null) value else s"<$tag>" + children.map(c => s"<${c.tag}>${c.toString}</${c.tag}>").mkString("\n") + s"</$tag>"
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
    None
  }

  def addNode(node: TreeNode): Unit = {
    val parent: TreeNode = search(node.tag).getOrElse(root)
    parent.addChild(node)
    println(s"search for: ${node.repr}; got parent: ${parent.repr}")
  }

  def addNode(tagChunk: String, string: String): Unit = {
    val tags = tagChunk.split(":").reverse
    // search tags specific --> general; add node to most specific non-leaf node
    var done = false
    var i = 0
    while (!done && i < tags.length) {
      val curr = tags(i)
      search(curr) match {
        case Some(node) =>
          // if the node found is a leaf, attach the new node to its parent
          if (node.isLeaf) {
            val n = new TreeNode(tags.head, string, node.parent)
//            println(s"tag=$curr ; found node=${node.repr} ; attach ${n.repr} to parent ${node.parent.repr}")
            node.parent.addChild(n)
          } else { // otherwise, just add the new node to the found node's children
            val n = new TreeNode(tags.head, string, node)
//            println(s"tag=$curr ; found node=${node.repr} ; attach ${n.repr} to ${node.repr}")
            node.addChild(n)
          }
          done = true
        case None =>
      }
      i += 1
    }
    // if we didnt find anything, add the necessary subtree
    if (!done) {
      // unreverse tags (want to add general --> specific in this case)
      val tags2 = tags.reverse
      var prev = root
      i = 0
      while (i < tags2.length - 1) {
        val n = new TreeNode(tags2(i), null, prev)
//        println(s"add child: ${n.repr} to parent: ${prev.repr}")
        prev.addChild(n)
        prev = n
        i += 1
      }
      val l = new TreeNode(tags2.last, string, prev)
      prev.addChild(l)
//      println(s"(final) add child: ${l.repr} to parent: ${prev.repr}")

    }

  }

  def dfs(n: TreeNode): String = {
    n.discovered = true
    if (n.isLeaf) s"${n.openTag}${n.value}${n.closeTag}"
    else {
      var result = s"${n.openTag}"
      for (c <- root.children if !c.discovered) {
        val subtree = new XMLTree(c)
        result += subtree.dfs(c)
      }
      result += s"${n.closeTag}"
      result
    }
  }

  def printTree(): Unit = {
    println(dfs(root))
  }

  override def toString: String = dfs(root)

}

object XMLParser {
  def getTags(token: Token): Seq[String] = token.attr[CitationLabel].categoryValue.split(":").map(t => t.drop(2))
  def getLastTag(token: Token): String = token.attr[CitationLabel].categoryValue.split(":").last
  def getFirstTag(token: Token): String = token.attr[CitationLabel].categoryValue.split(":").head.drop(2)
  def getTagAtPos(token: Token, pos: Int): String = token.attr[CitationLabel].categoryValue.split(":")(pos).drop(2)
  def fromDocument(doc: Document): String = {
    val tokens = doc.tokens.toIndexedSeq
    for (token <- tokens) println(s"${token.string}\t${token.attr[CitationLabel].categoryValue}")
    println("")

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
        tree.addNode(tag, chunk)
    }

//    println(tree.toString)

    tree.toString

  }
}





