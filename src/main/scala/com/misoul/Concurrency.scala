package com.misoul

import scala.util.{Random, Failure, Success}
import java.util.concurrent.{Executors, ExecutorService}
import scala.concurrent.{Future, Await, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

//This class should be rename 'Future', since this is not the only one way to do Concurrency
object Concurrency {

  def return10 = 10

  val INTERVAL = 1000
  val MAXSLEEP = 3000
  val random = Random

  private class Sleeper(name: String, sleepTime: Int) extends Runnable {
    override def run() {
      Thread.sleep(sleepTime)
      println("Complete: " + name + ", after " + sleepTime)
    }
  }

  // Demonstrate basic Runnable, Callable
  def doBasic() {
    val pool: ExecutorService = Executors.newFixedThreadPool(3)
    try {
      (1 to 10) foreach { i =>
        pool.execute(new Sleeper("Sleeper" + i, random.nextInt(MAXSLEEP/3)))
      }
    } finally {
      pool.shutdown()
    }
  }

  def doFuture() {
    val future = Future[String] {
      Thread.sleep(INTERVAL)
      "RESULTS_FROM_FUTURE"
    }

    future.onComplete {
      case Success(value) => println("Future is successful: " + value)
      case Failure(e) => e.printStackTrace
    }

    println("Before waiting on future..")
    Await.ready(future.map("" + _), 60.second)
    println("After waiting on future..")
  }

  def doChainingFuture() {
    val loadKernel = Future {
      Thread.sleep(INTERVAL)
      "KERNEL LOADED"
    }

    val loadModuleNetworking = loadKernel map { kernelMsg =>
      Thread.sleep(INTERVAL)
      println(kernelMsg)
      "MODULE NETWORKING LOADED"
    } recover {
      case e => e.printStackTrace
    }

    loadModuleNetworking.onComplete {
      case Success(value) => Thread.sleep(INTERVAL); println("SHOULD NOT BE PRINTED: " + value)
      case Failure(e) => e.printStackTrace
    }

    Await.ready(loadModuleNetworking, 60.second) //Note that this doesn't wait for onComplete
    println("DONE")
  }

  /*
  The first thread prints 1 1 1 …, the second one prints 2 2 2 …, and the
  third one prints 3 3 3 … endlessly. How do you schedule these three threads
  in order to print 1 2 3 1 2 3
  */
  def doFairWorkThreads() = {

  }
  //TODO try with Actor, Akka, Finagle, Skiis

  def main(args: Array[String]) = {
    doChainingFuture()
  }
}
