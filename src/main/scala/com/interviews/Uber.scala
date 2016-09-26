package com.interviews

import scala.collection.immutable.HashMap

/*
A kidnaper wishes to write a ransom note using letters from a magazine article. You are given with the ransom note and
magazine article find whether kidnaper can write a note using the article or not.
I was supposed to write a bug free code for thin in O(n+m) time complexity, O(1) space complexity.
 */
case class RansomNote(magazine: String, note: String) {
  def getSignature(str: String): Map[Char, Int] = {
    str.filter(_ != ' ').map(c => c.toLower -> 1).groupBy(_._1).map(e => e._1 -> e._2.size)
  }

  def isPossible(): Boolean = {
    val ms = getSignature(magazine)
    val ns = getSignature(note)

    ns.foreach { entry =>
      val (char, count) = entry
      val mCount = ms.getOrElse(char, 0)
      if (mCount < count) return false
    }

    true
  }
}

class FindNthElement {
  def fromUnsorted(numbers: Vector[Vector[Int]], k: Int): Int = {
    if (k < 0) throw new IllegalArgumentException("k is negative: " + k)
    if (numbers.length == 1) {
      if (numbers(0).length < k) throw new IllegalArgumentException("k is too big: " + k)
      else return numbers(0)(k-1)
    }

    def filter(f: Int => Boolean): (Vector[Vector[Int]], Int) = {
      val array = numbers.map(a => a.filter(f(_))).filter(_.nonEmpty)
      (array, array.map(_.length).sum)
    }

    val pivot = numbers(0)(0)
    val (smaller, smallerCount) = filter((x:Int) => x < pivot)
    val (equal, equalCount) = filter((x:Int) => x == pivot)
    val (larger, largerCount) = filter((x: Int) => x < pivot)

    if (smallerCount >= k) return fromUnsorted(smaller, k)
    if (smallerCount + equalCount >= k) return pivot
    fromUnsorted(larger, k - smallerCount - equalCount)
  }
}

/*
Given a string A and B, find the smallest substring of A that contains all the characters from B. (implement
  solution in O(n), keep in mind chars in B can repeat)
 */
class FindShortestBinA {
  import scala.collection.mutable.HashMap

  def find(a: String, b: String): Int = {
    val bs: HashMap[Char, Int] = HashMap(b.map(c => c -> 1).groupBy(_._1).map(e => (e._1,e._2.length)).toSeq: _*) //TODO
    val q: HashMap[Char, Array[Int]] = HashMap.empty
    var cCount = b.length
    var found = false
    var minDistance = -1

    (0 until a.length).foreach { i =>
      val curr = a(i)
      bs.get(curr) match {
        case Some(count) =>
          if (count > 0) {
            bs(curr) = count - 1
            cCount -= 1
            if (cCount == 0) found = true

            q(curr) = q.getOrElse(curr, Array.empty[Int]) ++ Seq(i)
          } else {
            q(curr) = q(curr).drop(1) ++ Seq(i)
          }

          if (found) {
            val minIndex = q.values.map(item => item(0)).min
            if (minDistance == -1 || minDistance > i - minIndex + 1) minDistance = i - minIndex + 1
          }
        case _ =>
      }
    }

    minDistance
  }
}

object TryRegex {
  def doTry() = {
    val datePattern = """([\d]{4})-([\d]{2})-(\d\d)""".r


    val datePattern(year, month, day) = "2016-09-20"
    println(year + " " + month + " " + day)

    datePattern.findAllMatchIn("2016-09-20 2017-09-2 3016-09-20 2016-019-20 4016-09-20").foreach(println(_))
    val matches = datePattern.findAllMatchIn("2016-09-20 2017-09-2 3016-09-20 2016-019-20 4016-09-20")
    println("matches: " + matches.mkString(" "))

  }
}


case class Node(data: Char, left: Option[Node], right: Option[Node]) {
  override def toString = Node.serialize(Some(this))
}
object Node {
  val delim = '#'

  def serialize(node: Option[Node]): String = {
    if (node.isEmpty) return "#"

    node.get.data + serialize(node.get.left) + serialize(node.get.right)
  }

  def deserialize(s: String): (Option[Node], String) = {
    if (s.isEmpty) return (None, "")

    val data = s(0)
    if (data == delim) return (None, s.drop(1))

    val (left, rightStr) = deserialize(s.drop(1))
    val (right, leftOver) = deserialize(rightStr)

    def oToStr(o: Option[Node]) = if (o.isEmpty) delim else o.get.data
    println(data + " " + oToStr(left) + " " + oToStr(right))

    (Some(Node(data, left, right)), leftOver)
  }
  def fromString(s: String): Option[Node] = {
    val (node, leftOver) = deserialize(s)

    if (leftOver.nonEmpty) throw new IllegalArgumentException("Invalid input: " + leftOver)
    node
  }
}

class Sudoku(matrix: Array[Array[Int]]) {
  type Loc = (Int,Int)
  val size = matrix.length
  val nSize = Math.sqrt(size).toInt
  val blank = 0
  var found = false

  override def toString = matrix.map(_.mkString("")).mkString("\n")

  def findMates(cell: Loc): (Seq[Loc], Seq[Loc], Seq[Loc]) = {
    val ix = cell._1 - cell._1%nSize
    val iy = cell._2 - cell._2%nSize

    val offsets = (for (i <- 0 until nSize; j <- 0 until nSize) yield (i,j)).map{o => (ix + o._1, iy + o._2)}.filter(_!=cell)
    val row = (0 until size).map(i => (cell._1, i)).filter(_!=cell)
    val col = (0 until size).map(i => (i, cell._2)).filter(_!=cell)

    (offsets, row, col)
  }

  def getValues(cells: Seq[Loc]): Set[Int] = cells.map(c => matrix(c._1)(c._2)).to[Set]

  def strBlanks(blanks: Seq[Loc]) = blanks.map(c => "("+c._1+","+c._2+")".mkString(" "))

  def explore(blanks: Seq[Loc]): Unit = {
    if (found) return
    if (blanks.isEmpty) {
      found = true
      println("FOUND!!\n" + toString)
      return
    }

    val (cell, next) = (blanks.head, blanks.tail)
    val (small, row, col) = findMates(cell)
    val used = getValues(small) ++ getValues(row) ++ getValues(col)

    (1 to size).filter(!used.contains(_)).foreach { i =>
      if (found) return
      matrix(cell._1)(cell._2) = i
      explore(next)
      matrix(cell._1)(cell._2) = 0
    }
  }

  def solve(): Unit = {
    import scala.collection.mutable.Queue
    val blanks = Queue.empty[Loc]

    for (i <- 0 until size; j <- 0 until size) {
      if (matrix(i)(j) == blank) blanks.enqueue((i,j))
    }

    val b = blanks.to[Seq]
    println("Blanks: " + blanks.length + " " + b.map(c => "("+c._1+","+c._2+")".mkString(" ")))
    println(toString)

    explore(b)
  }
}
object Sudoku {
  def apply(a: Seq[String]): Sudoku = {
    val array = a.map{row => row.map{c => if (c == '.') 0 else c.toString.toInt}.to[Array]}.to[Array]
    new Sudoku(array)
  }
}


object Uber extends App {
  val a1 = Seq(
    ".13.",
    "2...",
    "...3",
    ".21."
  )
  val a = Seq(
    "9.3.....8",
    ".6.....1.",
    ".5.2...9.",
    "...1645.2",
    "....3....",
    "6.1587...",
    ".4...6.8.",
    ".8.....2.",
    "2.....3.7"
  )
  val s = Sudoku(a)

  s.solve()
}

/*
  PROBLEMS

1. Constant time random access hash implementation

2. Efficient elevator API

3. Ransom note

4. Median of k unsorted arrays

5. Design of a task scheduler

6. Custom comparator
7. Given 2 trips, algorithm to see if they should be pooled
8. Sudoku
9. How could you make Uber better
10. Walk me through how you would troubleshoot an error you're encountering.
11. Implement data structure "Map" storing pairs of integers (key, value) and define following member functions in O(1)
  runtime: void insert(key, value), void delete(key), int get(key), int getRandomKey().
12. Given a string A and B, find the smallest substring of A that contains all the characters from B. (implement
  solution in O(n), keep in mind chars in B can repeat)
13. Given a picture of square with a bunch of horizontal and vertical lines in it (lines are not necessarily spanning
  the full square length, in other words think of a fine grid with many holes in it), design data structure(s)
  representing the data and a function that returns a number of squares pictured. (actual implementation expected)
14. How would you design Youtube (need for low latency, robustness against data loss, ...) (no implementation necessary)
15. Sketch design of a project you participated in. (on whiteboard)
16. Write a function that returns values randomly, according to their weight.
17. Binary tree serialization/deserialization
18. Uber Pool System design (walk from signup process to how to get the nearest drivers)
19. Find overlapping meeting times
20. Implement autocomplete system

 */