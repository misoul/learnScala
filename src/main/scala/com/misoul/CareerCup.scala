package com.misoul

import java.util.concurrent.locks.{Lock, ReentrantLock, Condition}
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Random, Failure, Success}
// import scala.collection.mutable.{Array}


object CareerCup {

  /*
  Given a array of integer and group size, reverse array by group size, example as follows:
[1, 2, 3, 4, 5, 6], 1 -> [1, 2, 3, 4, 5, 6]
[1, 2, 3, 4, 5, 6], 2 -> [2, 1, 4, 3, 6, 5]
[1, 2, 3, 4, 5, 6], 3 -> [3, 2, 1, 6, 5, 4]
[1, 2, 3, 4, 5, 6, 7, 8], 3 -> [3, 2, 1, 6, 5, 4, 8, 7]
  Design test cases for you API
  */
  def reverseArrayByGroup(v: Vector[Int], groupSize: Int): Vector[Int] = {
    if (groupSize < 1) throw new IllegalArgumentException("groupSize cannot be zero")

    val input = v.to[Array]
    val length = input.length
    (0 to length/groupSize) foreach { i =>
      var begin = Math.min(i * groupSize, length-1)
      var end   = Math.min(i * groupSize + groupSize - 1, length-1)

      while (begin < length && end < length && begin < end) {
        val temp = input(begin)
        input(begin) = input(end)
        input(end) = temp
        begin += 1
        end -= 1
      }
    }

    input.to[Vector]

    //TODO: do a Skiis version of this
  }

  // Unbalanced binary search tree
  class BinaryNode(val data: Int) {
    private var _left : Option[BinaryNode] = None
    private var _right: Option[BinaryNode] = None

    def left = _left
    def left_= (l: Option[BinaryNode]):Unit = _left = l // TODO: Setters??
    def right = _right
    def right_= (r: Option[BinaryNode]):Unit = _right = r


    def add(newNode: BinaryNode): Unit = {
      val newData = newNode.data

      def addChild(child: Option[BinaryNode], assign: Option[BinaryNode]=>Unit) = {
        if (child.isEmpty) assign(Option(newNode)) else child.get.add(newNode)
      }
      if (data > newData) addChild(left, left_=) else addChild(right, right_=)
    }
  }
  object BinaryNode {
    def apply(input: Vector[Int]): BinaryNode = {
      if (input.length < 1) throw new IllegalArgumentException("Good input please")

      val root = new BinaryNode(input(0))
      (1 until input.length) foreach { data =>
        root.add(new BinaryNode(input(data)))
      }

      root
    }
  }

  def spiralPrint2dArray(array: Array[Array[Int]]): String = {
    if (array.length == 0) throw new IllegalArgumentException("Give me a good array")

    val result = new StringBuilder
    val nRow = array.length
    val nCol = array(0).length
    val nRing = Math.min(nRow, nCol) / 2

    def process(marker: String, data: Int): String = { println(marker + " " + data); data.toString }
    (0 to nRing) foreach { ring =>
      val rRow = nRow - ring
      val rCol = nCol - ring
      val cRow = nRow - 2*ring
      val cCol = nCol - 2*ring
      if (cRow == 1) {
        (0 until cCol) foreach { i => result ++= array(ring)(ring + i).toString }
      } else if (cCol == 1) {
        (0 until cRow) foreach { i => result ++= array(ring + i)(ring).toString }
      } else if (cRow > 1 && cCol > 1){
        (ring until rCol - 1) foreach { i => result ++= process("A", array(ring)(i)); }
        (ring until rRow - 1) foreach { i => result ++= process("B", array(i)(rCol - 1)); }
        ((rCol - 1) until ring by -1) foreach { i => result ++= process("C", array(rRow - 1)(i)); }
        ((rRow - 1) until ring by -1) foreach { i => result ++= process("D", array(i)(ring)); }
      }
    }

    result.toString
  }

  /*
  Given an array of integers and the target as an input
    E.g. input = {5,2,1,4,3,6,7,8} .. target : 333 it should true as
    (5 +214 + 36 + 78) if the target does not match it should return false....
    E.g. of false input : {5,5} target:60 ... It should return false as the
    combinations possible are 5+5 = 10 and 55
   */
  def isSummable(a: Seq[Int], targetSum: Int): Boolean = {
    // Solution: exhaustive search with early termination
    // Search function: whether to insert a "number delimiter" between each pair or not.

    def explore(sum: Int, index: Int, rightMost: String): Boolean = {
      // Early termination
      if (sum > targetSum) return false

      if (index == a.length) {
        if (sum + rightMost.toInt == targetSum) return true else return false
      }

      explore(sum + rightMost.toInt, index+1, a(index).toString) ||   // insert delimiter: YES
        explore(sum, index+1, rightMost + a(index).toString)          // insert delimiter: NO
      }

    if (a.length == 1) return (a(0).toInt == targetSum)

    explore(0, 1, a(0).toString)
  }

  /*
    Given 2 strings, each of which contains exactly 1 '*' serving as a wild card
    to match the 2 strings. Return the (shortest) matched string.

    See tests for examples.
   */
  def resolveWildcards(a: String, b: String): String = {
    if (a.length == 0 || b.length == 0) throw new IllegalArgumentException("Corner case. Skipped for now")

    val NO_RESULT = "not-possible"

    var i = 0;
    while (i < a.length && i < b.length && a(i) != '*' && b(i) != '*' && a(i) == b(i)) i += 1

    if (i >= a.length || i>= b.length) return NO_RESULT
    if (a(i) != b(i)) {
      if (a(i) != '*' && b(i) != '*') return NO_RESULT
      else if (a(i) == '*') {
        leftTrimmed(a.drop(i), b.drop(i))
      } else leftTrimmed(b.drop(i), a.drop(i))
    } else {
      if (a(i) == '*') leftTrimmed(a.drop(i), b.drop(i))
      else throw new IllegalArgumentException("UnreachableState") //TODO: return an appropriate Exception
    }

    def leftTrimmed(c: String, d: String): String = {
      // This one is no fun to implement, too many 'ifs'
      println(c + " v.s. " + d)
      ""
    }

    var left = 0


    ""
  }

  /*
  The first thread prints 1 1 1 …, the second one prints 2 2 2 …, and the
  third one prints 3 3 3 … endlessly. How do you schedule these three threads
  in order to print 1 2 3 1 2 3
  */
  def doFairWorkThreads() = {
    val random = Random

    class WorkThread(name: String, lock: Lock, waitCondition: Condition, triggerCondition: Condition) extends Thread {
      override def run () {
        var i = 0
        while (true) {
          lock.lock()
          waitCondition.await()
          Thread.sleep(random.nextInt(500))
          println(name)
          triggerCondition.signal()
          lock.unlock()
        }
      }
    }

    val lock = new ReentrantLock();
    val condition1 = lock.newCondition();
    val condition2 = lock.newCondition();
    val condition3 = lock.newCondition();
    val thread1 = new WorkThread("Worker-1", lock, condition1, condition2)
    val thread2 = new WorkThread("Worker-2", lock, condition2, condition3)
    val thread3 = new WorkThread("Worker-3", lock, condition3, condition1)

    thread1.start()
    thread2.start()
    thread3.start()

    Thread.sleep(1000) // Make sure all works are ready, so the START signal is not lost
    lock.lock()
    println("START, GO!!!")
    condition1.signal()
    lock.unlock()
  }

  /*
    See problem description: http://stackoverflow.com/questions/746082/how-to-find-list-of-possible-words-from-a-letter-matrix-boggle-solver#746345
   */
  def findAllBoggles(dict: Set[String], matrix: Seq[Seq[Char]]): Set[String] = {
    import scala.collection.mutable.Map

    // This implementation def needs code cleanup

    val results = scala.collection.mutable.Set[String]()
    if (matrix.length == 0 || matrix(0).length == 0) throw new IllegalArgumentException("Bad Input")

    val nRow = matrix.length
    val nCol = matrix.length

    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      val offsets = Seq(-1, 0, 1)
      val neighbors: Seq[(Int, Int)] = for (i <- offsets; j <- offsets) yield (row+i, col+j)

      def isValid(location: (Int, Int)): Boolean = {
        val (i, j) = location
        i >= 0 && i < nRow && j >= 0 && j < nCol && (i != row || j != col)
      }
      neighbors.filter(isValid(_))
    }

    def explore(current: (Int,Int), locations: Seq[(Int, Int)], chars: String, node: TrieNode): Unit = {
      // println("Exploring: " + matrix(current._1)(current._2) + " with " + chars)
      if (dict.contains(chars)) {
        // println("FOUND: " + chars + " - " + current + " - " + locations)
        results.add(chars)
      }

      neighbors(current._1,current._2) foreach { location =>
        val (i,j) = location
        val nextChar = matrix(i)(j)
        val nextNode = node.getPath(nextChar)
        if (!locations.contains((i,j)) && nextNode.isDefined)
          explore((i,j), locations++Seq((i,j)), chars+matrix(i)(j), nextNode.get)
       }
    }

    case class TrieNode(char: Char, children: Map[Char,TrieNode] = Map()) {
      var _isWord = false

      def isWord = _isWord
      def isWord_= (isWord: Boolean) = _isWord = isWord

      def addPath(c: Char) = children.getOrElseUpdate(c, new TrieNode(c, Map[Char,TrieNode]()))
      def hasPath(c: Char) = children.get(c).isDefined
      def getPath(c: Char) = children.get(c)
      def addWord(c: Char) = { val child = addPath(c); child.isWord = true; child }
    }

    def buildTree(node: TrieNode, dict: Set[String]) = {
      dict.foreach { word =>
        val lastNode = word.to[Array].foldLeft(node)((currNode,char) => currNode.addPath(char))
        lastNode.isWord = true
      }
    }

    // Create TrieMap from dictionary
    val root = new TrieNode('-')
    buildTree(root, dict)

    val allCells = for (i <- 0 until nRow; j <- 0 until nCol) yield((i,j))
    allCells.foreach { cell =>
      val currChar = matrix(cell._1)(cell._2)
      root.getPath(currChar).foreach{ explore(cell, Seq.empty, currChar.toString, _) }
    }

    results.to[Seq].to[Set]
  }

  def countOccurances(base: String, pattern: String) = {
    def iter(s1: String, count: Int): Int = {
      if (s1.isEmpty) return count

      s1.indexOfSlice(pattern) match {
        case -1 => count
        case i  => iter(s1.drop(i + pattern.length), count+1)
      }
    }

    iter(base, 0)
  }

  def countSubstring(str1:String, str2:String):Int={
    @tailrec def count(pos:Int, c:Int):Int={
      val idx=str1 indexOf(str2, pos)
      if(idx == -1) c else count(idx+str2.size, c+1)
    }
    count(0,0)
  }

  def findPrime(n: Int): Array[Int] = {
    def isPrime(i: Int, primes: Array[Int]) = !primes.exists(i % _ == 0)

    def iter(curr: Int, primes: Array[Int]): Array[Int] = {
      if (curr > n) return primes

      if (isPrime(curr, primes)) iter(curr+2, primes++Array(curr)) else iter(curr+2, primes)
    }

    iter(3, Array(2))
  }

  def main(args: Array[String]) = {
    // println(reverseArrayByGroup(Vector(1,2,3,4,5,6,7,8), 3))

    println(countOccurances("ab 1ab 2ab 3ab a", "ab"))
  }
}
