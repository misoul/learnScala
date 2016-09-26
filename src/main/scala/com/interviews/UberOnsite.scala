package com.interviews




object UberOnsite extends App {

  def findMinimalDistanceBetween2Words() = {
    // Given a string of words and 2 words, find the minimal distance between 2 words
  }

  def findIndexRange() = {
    // Given a sorted array of integers and a number, find the index range of the number occurences.
  }

  def findN(a: Seq[Int], sum: Int, n: Int): Seq[Seq[Int]] = {
    if (n <= 0) return Seq.empty
    if (a.length == 1) {
      if (a(0) == sum) return Seq(a) else return Seq.empty
    }

    val (curr, next) =  (a.head, a.tail)

    if (curr != sum)
      findN(next, sum-curr, n-1).map(_ ++ Seq(curr)) ++ findN(next, sum, n)
    else
      findN(next, sum-curr, n-1).map(_ ++ Seq(curr)) ++ findN(next, sum, n) ++ Seq(Seq(curr))
  }

  def findAll(a: Seq[Int], sum: Int): Seq[Seq[Int]] = {
    (1 to a.length).map(findN(a,sum,_)).foldLeft(Seq.empty[Seq[Int]])((a,b) => a ++ b)
  }

  val input = Seq(1,2,0,-3,6,3)
  val sum = 3

  val result = findAll(input.sorted, sum)
  val u = result.map(i => i.sorted)

  println(result.map(_.mkString(",")).mkString("\n"))

}


/*
  def findAll(a: Seq[Int], sum: Int): Seq[Seq[Int]] = {
    if (a.length == 1) {
      if (a(0) == sum) return Seq(a) else return Seq.empty
    }

    val (curr, next) = (a.head, a.tail)


    if (sum != curr) findAll(next, sum-curr).map(_ ++ Seq(curr)) ++ findAll(next, sum)
        else findAll(next, sum-curr).map(_ ++ Seq(curr)) ++ findAll(next, sum) ++ Seq(Seq(curr))
  }

  val input = Seq(1,2,0,-3,3,6,3)
  val sum = 3

  val result = findAll(input, sum)
  val u = result.map(i => i.sorted)

  println(result.map(_.mkString(",")).mkString("\n"))

 */