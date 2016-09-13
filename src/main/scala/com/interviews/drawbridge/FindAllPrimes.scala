package com.interviews.drawbridge

object FindAllPrimes extends App {
  def findPrimes(n: Int): Array[Int] = {

    // Assume: 'primes' contains all prime numbers up to n
    def isPrime(primes: Array[Int], n: Int) = !primes.exists(i => n%i == 0) // Early termination: Math.sqrt(n)


    def iter(n: Int, i: Int, primes: Array[Int]): Array[Int] = {
      if (i > n) return primes

      if (isPrime(primes, i)) iter(n, i+2, primes++Array(i)) else iter(n, i + 2, primes)
    }

    iter(n, 3, Array(2))
  }

  println(findPrimes(50).mkString(" "))
}
