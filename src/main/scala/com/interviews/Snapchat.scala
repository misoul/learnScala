package com.interviews

import com.interviews.BuildTreeFromXMLFile.{ContentMarkup, CloseMarkup, OpenMarkup, XMLData}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.util.Random

object RandomlyShuffleArray {
  val random = Random

  def shuffle(a: Vector[Int]): Vector[Int] = {
    if (a.length <= 1) return a

    val l = a.length
    val i = random.nextInt(l)
    val iValue = a(i)
    val lValue = a(a.length - 1)

    shuffle(a.updated(i, lValue).take(l-1)) ++ Seq(iValue)
  }
}

object BuildTreeFromXMLFile {
  abstract class XMLData
  case class OpenMarkup(name: String)  extends XMLData // e.g. "<node>"
  case class CloseMarkup(name: String) extends XMLData // e.g. "</node>"
  case class CompleteMarkup(name: String, data: String) extends XMLData // e.g. "<node/>"
  case class ContentMarkup(data: String) extends XMLData // "abc"

  case class XMLNode(markup: String, var data: String, children: ArrayBuffer[XMLNode]) {

    def toStringShort = s"${markup}:${data}"

    def toStringShallow(tab: String): String = {
      s"${tab}${markup}:${data}: Children[${children.map(_.toStringShort).mkString(" ")}]"
    }

    def toStringDeep(tab: String): String = {
      toStringShallow(tab) + "\n" + children.map(_.toStringDeep(tab + "  ")).mkString("\n")
    }

    override def toString = toStringDeep("")
  }

  def buildTree(markups: Seq[XMLData]): Option[XMLNode] = {
    val stack = new Stack[XMLNode]()
    var currentNode: Option[XMLNode] = None

    if (markups.isInstanceOf[CompleteMarkup]) {
      throw new IllegalStateException("not implemented yet")
    }
    if (!markups(0).isInstanceOf[OpenMarkup])
      throw new IllegalArgumentException("First markup is invalid")

    val firstMarkup = markups(0).asInstanceOf[OpenMarkup]
    markups.zipWithIndex.foreach { entry =>
      val (markup, index) = entry
      markup match {
        case OpenMarkup(name) =>
          currentNode match {
            case None => currentNode = Some(XMLNode(name, "", ArrayBuffer.empty))
            case Some(node) =>
              val newNode = XMLNode(name, "", ArrayBuffer.empty)
              node.children ++= Seq(newNode)
              stack.push(node)
              currentNode = Some(newNode)
          }
        case CloseMarkup(name) =>
          currentNode match {
            case None => throw new IllegalArgumentException("Invalid input: " + name)
            case Some(node) =>
              if (node.markup != name) throw new IllegalArgumentException(s"Invalid input: ${node.markup} vs $name")
              if (stack.length == 0) {
                if (index != markups.length - 1) throw new IllegalArgumentException(s"Invalid input: ${node.markup}")
                // then leave currentNode as is, because it is the root!
              } else {
                currentNode = Some(stack.pop)
              }
          }
        case CompleteMarkup(name, data) => throw new IllegalStateException("Not implemented yet")
        case ContentMarkup(data) =>
          currentNode match {
            case None => throw new IllegalArgumentException(s"Invalid input: $data")
            case Some(node) => node.data = node.data + data
          }
        case _ => throw new IllegalStateException("Should not reach here")
      }
    }

    currentNode
  }
}

object Snapchat extends App {
//  (1 to 10) foreach { i => println(RandomlyShuffleArray.shuffle(Vector(1,2,3))) }

  val node = BuildTreeFromXMLFile.buildTree(Seq(
    OpenMarkup("root"),
    ContentMarkup("rootContent"),
      OpenMarkup("A"),
      ContentMarkup("aContent"),
      CloseMarkup("A"),

      OpenMarkup("B"),
        OpenMarkup("BB"),
        ContentMarkup("bbContent"),
        CloseMarkup("BB"),
      ContentMarkup("cContent"),
      CloseMarkup("B"),

      OpenMarkup("C"),
      ContentMarkup("cContent"),
      CloseMarkup("C"),

    CloseMarkup("root")
  ))

  if (node.isEmpty) println("FAILED") else println(node.get.toString)
}
