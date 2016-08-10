package com.interviews.radius

import scala.io.Source

case class DoNotCall(phone: String) {
  override def toString = "[ " + phone + " ]"
}

// For the sake of this assignment, we are only interested whether the year is 2015 or not.
case class Transaction(userId: String, amount: Float, year: Int) {
  override def toString = "[ " + userId + " , " + amount + " , " + year + " ]"
}

object Transaction {
  def apply(str: String): Transaction = {
    val Array(userId, amountStr, dateStr) = str.split(";")
    new Transaction(userId, amountStr.substring(1).toFloat, dateStr.substring(0,4).toInt)
  }
}

case class User(id: String, name: String, phones: Seq[String]) {
  override def toString = "[ " + id + " , " + name + " , " + phones.mkString("~") + " ]"
}

object User {
  def apply(str: String): User = {
    // val Array(id, name, emails, phones) = str.split(";")
    // new User(id, name, phones.split(",").toSeq)
    null
  }
}

object TopReachableLeads extends App {
  val donotcallFilename = "./src/main/scala/com/interviews/radius/donotcall.txt"
  val usersFilename = "./src/main/scala/com/interviews/radius/users.txt"
  val transactionsFilename = "./src/main/scala/com/interviews/radius/transactions.txt"


  val dncSrc = Source fromFile donotcallFilename
  dncSrc.getLines.take(10).foreach { str => println(DoNotCall(str)) }
  dncSrc.close()

  // val usersSrc = Source fromFile usersFilename
  // usersSrc.getLines.take(10).foreach { str => println(User(str)) }
  // usersSrc.close()

  val transactionsSrc = Source fromFile transactionsFilename
  transactionsSrc.getLines.take(10).foreach { str => println(Transaction(str)) }
  transactionsSrc.close()



  println("hey hey")

}
