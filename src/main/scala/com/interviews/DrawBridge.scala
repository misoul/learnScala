package com.interviews

import scala.annotation.tailrec

object DrawBridge extends App {
  def findAllPrimes(n: Int): Array[Int] = {

    // Assume: 'primes' contains all prime numbers up to n
    def isPrime(primes: Array[Int], num: Int) =
      !primes.takeWhile(_ <= Math.sqrt(num)).exists(num % _ == 0) // Early termination: Math.sqrt(n)


    @tailrec def iter(n: Int, i: Int, primes: Array[Int]): Array[Int] = {
      if (i > n) return primes

      if (isPrime(primes, i)) iter(n, i+2, primes++Array(i)) else iter(n, i + 2, primes)
    }

    iter(n, 3, Array(2))
  }

  println(findAllPrimes(50).mkString(" "))
}
