package com.interviews.radius

import scala.io.Source
import org.apache.spark.rdd.{RDD, PairRDDFunctions}
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

import java.io._


object TopReachableLeads {
  case class DoNotCall(phone: String) {
    override def toString = "[ " + phone + " ]"
  }

  // For this campaign, we are only interested whether the year is 2015 or not.
  case class Transaction(userId: String, amount: Double, year: Int) {
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

/*
    2.1. Users.txt contains malformed lines (<15users); some cross checks show
    that these users indeed have big transactions amount that might make them
    end up in the top 1000 (a bit unrealistic since usually typos in VIP account
    are quickly fixed). Since this is an exercise, these users are skipped and
    logged.
 */
  object User {
    def apply(str: String): User = {
      try {
        val Array(id, name, emails, phones) = str.split(";", -1) // Use "-1" for no-phone, no-email cases
        User(id, name, emails.split(",").to[Seq], phones.split(",").to[Seq])
      } catch {
        case e: Exception => {
          println("Bad input: userLine=" + str);
          User("FAILED", "FAILED", Seq.empty, Seq.empty) // This won't show up in the resuilt
        }
      }
    }
  }

  case class ResultLine(id: String, name: String, phones: Seq[String], totalAmount: Double) {
    override def toString = id + ";" + name + ";" + phones.mkString(",") + ";" + f"$totalAmount%1.2f"
  }

  // Output: (Customer ID, Customer name, reachable phone list , total transaction amount)
  def compute(users: RDD[User], donotcall: RDD[DoNotCall], transactions: RDD[Transaction],
              noTargetUsers: Int = 1, targetYear: Int = 2015,
              sc: SparkContext): Array[ResultLine] = {
    val heuristicMultiplier = 5
    val heuristicTop = noTargetUsers * heuristicMultiplier

    // Calculate top spenders by 'total of their transactions'
    val topSpendersArray = transactions
      .filter(_.year == targetYear)
      .map { case t => (t.userId, t.amount) }
      .reduceByKey(_+_)
      .sortBy(_._2, false)
      .take(heuristicTop)
    val topSpenders = sc.parallelize(topSpendersArray)

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
      .map { case ((id, name, phones), amount) => ResultLine(id, name, phones, amount)}

    topSpendersWithInfo.take(noTargetUsers).sortWith(_.totalAmount > _.totalAmount)
  }

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local")
                              .setAppName("TopReachableLeads")
    val sc = new SparkContext(conf)

    val prefixPath = "./src/main/scala/com/interviews/radius/"
    val donotcallFilename = prefixPath + "donotcall.txt"
    val usersFilename = prefixPath + "users.txt"
    val transactionsFilename = prefixPath + "transactions.txt"

    val dncSrc = sc.textFile(donotcallFilename)
    val usersSrc = sc.textFile(usersFilename)
    val transactionsSrc = sc.textFile(transactionsFilename)

    val transactions = transactionsSrc.map(Transaction(_))
    val dnc = dncSrc map(DoNotCall(_))
    val users = usersSrc map(User(_))

    val result = compute(users, dnc, transactions, sc=sc)
    result.foreach { println(_) }

    val pw = new PrintWriter(new File("output.txt" ))
    result.foreach { entry => pw.write(entry.toString + "\n") }
    pw.close

  }
}
