package com.misoul

import scala.io.Source
import scala.io.StdIn
import scala.collection.Searching._
import scala.language.postfixOps

object HackerRank {

  /*
  Problem: for a given word w, output a  lexicographically bigger than  in a
    separate line. In case of multiple possible answers, print the
    lexicographically smallest one, and if no answer exists, print no answer.

  Solution:
  1- Sort the input string's chars and count, to figure out the chars in interest.
  2- Find the pivot P in input word w: subtring R on the right of P is the smallest.
  3- We need to swap P with the smallest char in R that is > P, fix up R...
   */
  private def findSmallestNextWord(input: String): String = {
    val NOANSWER = "no answer"
    if (input.length < 2) return NOANSWER

    val chars = input.to[Array]
    var pivot = chars.length-2
    while (pivot >= 0 && chars(pivot) >= chars(pivot+1)) pivot = pivot-1;
    if (pivot < 0) return NOANSWER

    // Find the smallest char on the right that is larger than pivot (Binary search would be better)
    var i = chars.length - 1
    while (chars(pivot) > chars(i)) i = i - 1;

    val temp = chars(pivot)
    chars(pivot) = chars(i)
    chars(i) = temp

    chars.slice(0,pivot).mkString("") + chars(pivot) + chars.slice(pivot+1, chars.length).sortBy(c=>c).mkString("")
  }

  /* https://www.hackerrank.com/challenges/new-year-chaos */
  private def guessSwap(a: Array[Int]): String = {
    val ERROR = "Too chaotic"
    var count = 0

    (1 to a.length) foreach { i =>
      val num = a(i-1)
      val off = num - i

      if (off > 0) {
        if (off > 2) return ERROR
        count = count + off
      } else if (off < 0) {
        count = count - off
      }
    }

    if (count % 2 == 1) return ERROR
    count/2 + ""

    // This solution doesn't always give correct answers
  }

  // Also demonstrate Generic
  def permutations[T](xs: List[T]): List[List[T]] = xs match {
    case List(_) => List(xs)
    case _ => for ( x <- xs
                  ; (l, r) = xs span { x!= }
                  ; ys <- permutations(l ++ r.tail)
                  ) yield x :: ys
  }


// 2D Arrays
  def createMultiDimentionalArray(nRows: Int, nCols: Int): Array[Array[Int]] = {
    return Array.ofDim[Int](nRows, nCols)
  }

  def main(args: Array[String]) = {
    val count = StdIn.readInt

    (1 to count).foreach { i =>
      val numbers = StdIn.readLine.split(" ").map(_.toInt).to[List]

      println(permutations[Int](numbers).mkString("\n"))
      println("------")
    }
  }
}
