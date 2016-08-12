package com.interviews.radius

import scala.io.Source
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
    "return correctly for a simple test" in {
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
        Transaction("uid-1", 1.00, 2015),
        Transaction("uid-1", 2.21, 2015),
        Transaction("uid-1", 2.00, 2015),
        Transaction("uid-2", 5.22, 2015),
        Transaction("uid-3", 5.23, 2015),
        Transaction("uid-4", 5.24, 2015),
        Transaction("uid-5", 4.25, 2015),
        Transaction("uid-5", 1.00, 2015),
        Transaction("uid-6", 5.26, 2015),
        Transaction("uid-6", 0.00, 2015),
        Transaction("uid-7", 5.27, 2015),
        Transaction("uid-8", 3.28, 2015),
        Transaction("uid-8", 2.00, 2015)
      ))
      val dncs = sc.parallelize(Array(
        DoNotCall("11111"),
        DoNotCall("22222"),
        DoNotCall("33333"),
        DoNotCall("4444"),
        DoNotCall("00000")
      ))

      val results = compute(users, dncs, transactions, noTargetUsers=5, sc=sc)
      results foreach { println(_) }

      results === Array(
        ResultLine("uid-7","name-7",Vector("7777", "77777"),5.27),
        ResultLine("uid-5","name-5",Vector("5555"),5.25),
        ResultLine("uid-3","name-3",Vector("3333"),5.23),
        ResultLine("uid-2","name-2",Vector("2222"),5.22),
        ResultLine("uid-1","name-1",Vector("1111"),5.21)
      )
    }

    "process Radius-given files" in {
      val prefixPath = "./src/main/scala/com/interviews/radius/"
      val donotcallSrc = Source fromFile (prefixPath + "donotcall.txt")
      val usersSrc = Source fromFile (prefixPath + "users.txt")
      val transactionsSrc = Source fromFile (prefixPath + "transactions.txt")

      val users = sc.parallelize(usersSrc.getLines.map(User(_)).to[Seq])
      val dncs = sc.parallelize(donotcallSrc.getLines.map(DoNotCall(_)).to[Seq])
      val transactions = sc.parallelize(transactionsSrc.getLines.map(Transaction(_)).to[Seq])

      val result = compute(users, dncs, transactions, noTargetUsers=1000, sc=sc)

      donotcallSrc.close
      usersSrc.close
      transactionsSrc.close

      result.take(10).foreach { println(_) }
      result.length === 1000
    }
  }
}
