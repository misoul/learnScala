package com.interviews.radius

import scala.io.Source
import org.apache.spark.rdd.{RDD, PairRDDFunctions}
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf


object TopReachableLeads extends App {
  import scala.io.Source
  import org.apache.spark.rdd.{RDD, PairRDDFunctions}
  import org.apache.spark.SparkContext
  import org.apache.spark.SparkContext._
  import org.apache.spark.SparkConf

  case class DoNotCall(phone: String) {
    override def toString = "[ " + phone + " ]"
  }

  // For this campaign, we are only interested whether the year is 2015 or not.
  case class Transaction(userId: String, amount: Double, year: Int) { // TODO: fixme
    override def toString = "[ " + userId + " , " + amount + " , " + year + " ]"
  }

  object Transaction {
    def apply(str: String): Transaction = {
      val Array(userId, amountStr, dateStr) = str.split(";")
      Transaction(userId, amountStr.substring(1).toDouble, dateStr.substring(0,4).toInt)
    }
  }

  // Dropping user emails since we are not interested in email for this
  case class User(id: String, name: String, emails: Seq[String], phones: Seq[String]) {
    override def toString = "[ " + id + " , " + name + " , " + phones.mkString("~") + " ]"
  }

  object User {
    def apply(str: String): User = {
      val Array(id, name, emails, phones) = str.split(";", -1) // Use "-1" for no-phone, no-email cases
      User(id, name, emails.split(",").to[Seq], phones.split(",").to[Seq])
    }
  }

  val conf = new SparkConf().setAppName("TopReachableLeads")
  val sc = new SparkContext(conf)

  val prefixPath = "./src/main/scala/com/interviews/radius/"
  val donotcallFilename = prefixPath + "donotcall.txt"
  val usersFilename = prefixPath + "users.txt"
  val transactionsFilename = prefixPath + "transactions.txt"

  // val dncSrc = sc.parallelize(sc.textFile(donotcallFilename).take(10))
  // val usersSrc = sc.parallelize(sc.textFile(usersFilename).take(10))
  // val transactionsSrc = sc.parallelize(sc.textFile(transactionsFilename).take(10))
  val dncSrc = sc.textFile(donotcallFilename).take(10)
  val usersSrc = sc.textFile(usersFilename).take(10)
  val transactionsSrc = sc.textFile(transactionsFilename).take(10)

  val transactions = transactionsSrc.map(Transaction(_))
  val dnc = dncSrc map(DoNotCall(_))
  val users = usersSrc map(User(_))

  // Output: Customer ID, Customer name, reachable phone list , total transaction amount
  def compute(users: RDD[User], donotcall: RDD[DoNotCall], transactions: RDD[Transaction],
              nTop: Int = 1, targetYear: Int = 2015): String = {
    val heuristicMultiplier = 5
    val heuristicTop = nTop * heuristicMultiplier //TOOD: FIXMEs

    // Calculate top spenders by 'total of their transactions'
    val topSpenders = transactions
      .filter(_.year == targetYear)             // Filter by year
      .map { case t => (t.userId, t.amount) }
      .reduceByKey(_+_)                         // Calculate total spending by user
      .sortBy(_._2, false)

    val bl = new PairRDDFunctions(topSpenders)

    // Flatten & Filter out users without numbers
    val phoneToUser = users
      .flatMap { user => user.phones.map((user.id, user.name, _)) }
      .map { case (id, name, phone) => (phone, (id, name)) }

    // Drop donocall numbers from users (as well as users who have no phones left)
    // Assuming: donotcall.length << users.length
    val reachableUsers = donotcall.map { dnc => (dnc.phone, dnc.phone) }
      .cogroup(phoneToUser)
      .collect { case (phone, (dncs, idNames)) if dncs.isEmpty => (idNames.head, phone) }
      .groupBy(_._1)
      .map { case ((id, name), phones) => (id, (id, name, phones.map(_._2).to[Seq])) }

    val topSpendersWithInfo = topSpenders
      .cogroup(reachableUsers)
      .collect { case (id, (amounts, users)) if (!amounts.isEmpty && !users.isEmpty) => (users.head, amounts.head) }
      .map { case ((id, name, phones), amount) => (id, name, phones, amount)}

    topSpenders.foreach(println(_))
    topSpendersWithInfo.foreach { println(_) }

    "blah"
  }
}
