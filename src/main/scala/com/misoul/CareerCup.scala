package com.misoul

import java.util.concurrent.locks.{Lock, ReentrantLock, Condition}
import scala.concurrent.duration._
import scala.util.{Random, Failure, Success}


object CareerCup {


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
    doFairWorkThreads()
  }
}
