package com.interviews.radius

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTopReachableLeads extends Specification {

  import TopReachableLeads._

  val conf = new SparkConf().setMaster("local")
                            .setAppName("TopReachableLeads")
  val sc = new SparkContext(conf)

  "TopReachableLeads.compute" should {
    "simple test" in {
      val users = sc.parallelize(Array(
        User("uid-1", "name-1", Seq.empty, Seq("1111", "11111")),
        User("uid-2", "name-2", Seq.empty, Seq("2222")),
        User("uid-3", "name-3", Seq.empty, Seq("3333", "33333")),
        User("uid-4", "name-4", Seq.empty, Seq("4444")),
        User("uid-5", "name-5", Seq.empty, Seq("5555")),
        User("uid-6", "name-5", Seq.empty, Seq.empty),
        User("uid-7", "name-7", Seq.empty, Seq("7777", "77777"))
      ))
      val transactions = sc.parallelize(Array(
        Transaction("uid-1", 1.23, 2015),
        Transaction("uid-2", 1.24, 2015),
        Transaction("uid-3", 1.25, 2015),
        Transaction("uid-4", 1.26, 2015),
        Transaction("uid-5", 1.27, 2015),
        Transaction("uid-6", 1.28, 2015),
        Transaction("uid-7", 1.29, 2015),
        Transaction("uid-8", 1.22, 2015)
      ))
      val dnc = sc.parallelize(Array(
        DoNotCall("11111"),
        DoNotCall("22222"),
        DoNotCall("33333"),
        DoNotCall("4444"),
        DoNotCall("00000")
      ))

      compute(users, dnc, transactions, 1) === "blah"
    }
  }
}
