package com.misoul

import java.util.concurrent.locks.{Lock, ReentrantLock, Condition}
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
      println("Ring=" + ring)
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

  def main(args: Array[String]) = {
    // println(reverseArrayByGroup(Vector(1,2,3,4,5,6,7,8), 3))
    val r = spiralPrint2dArray(Array[Array[Int]](Array(1,2,5),
                                         Array(3,4,6)))
    println(r)
  }
}
