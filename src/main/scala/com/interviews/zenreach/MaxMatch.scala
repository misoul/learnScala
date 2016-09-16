package com.interviews.zenreach

import HungarianAlgorithm.hgAlgorithm
import scala.io.Source

object MaxMatch extends App {
  def parseName(name: String): (Int, Int, Int) = { // (nVowels, nConsonants, nNumbers)
    val vowels = "aeiouy"
    val consonants = "bcdfghjklmnpqrstvwxz"
    val numbers = "" //0123456789" // This is here because I'm not sure "letters" include "numbers"
    val vowelSet = (vowels + vowels.toUpperCase).toSet
    val consonantSet = (consonants + consonants.toUpperCase).toSet
    val numberSet = numbers.toSet

    (name.count(vowelSet.contains(_)), name.count(consonantSet.contains(_)), name.count(numberSet.contains(_)))
  }

  def maxCommonDivisor(a: Int, b: Int): Int = if (a == 0) b else { if (b == 0) a else maxCommonDivisor(b, a%b) } // Assuming positive int for now

  def computeScore(person: String, product: String): Double = {
    def isEven(n: Int) = (n % 2 == 0)

    val (eVowels, eConsonants, eNumber) = parseName(person)   // person
    val (oVowels, oConsonants, oNumber) = parseName(product)  // product
    val eLen = eVowels + eConsonants + eNumber
    val oLen = oVowels + oConsonants + oNumber
    val multiplier = if (maxCommonDivisor(eLen, oLen) > 1) 1.5 else 1

    oLen match {
      case l if isEven(l) => eVowels * 1.5 * multiplier
      case _              => eConsonants * multiplier
    }
  }

  def parseLine(line: String): (Array[String], Array[String]) = { //Return: (people, products)
    val Array(personsStr,productsStr) = line.split(";")
    val persons = personsStr.split(",")
    val products = productsStr.split(",")

    (persons, products)
  }

  def scoreMatrix(persons: Array[String], products: Array[String]): Array[Array[Double]] = {
    persons.map { person =>
      products.map { product =>
        println(person + " " + parseName(person) + " ~~ " + product + " " + parseName(product) + " = " + computeScore(person, product))

        computeScore(person, product)
      }
    }
  }

  def computeMaxMatch(line: String): Double = {
    val (people, products) = parseLine(line)
    val matrix = scoreMatrix(people, products)

    println(people.mkString("\n"))
    println("==")
    println(products.mkString("\n"))
    println("==")
    printMatrix(matrix)

    hgAlgorithm(matrix, "max")
  }

  def computeMaxMatchStr(line: String): String = "%.2f".format(computeMaxMatch(line))

  def printMatrix(m: Array[Array[Double]]) = {
    m.foreach { row => println(row.mkString(",") + "\n") }
  }

  val line1 = "Jack Abraham,John Evans,Ted Dziuba;iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow"
  val line2 = "Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,Peter Gibbons,Michael Bolton,Samir Nagheenanajar;Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,Printer paper,Vibe Magazine Subscriptions - 40 pack"
  val line3 = "Jareau Wade,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage"
  val line4 = "Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage"
  println(computeMaxMatchStr(line4))

}

/*
Jack Abraham,John Evans,Ted Dziuba;iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow
>> 21.00

Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,Peter Gibbons,Michael Bolton,Samir Nagheenanajar;Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,Printer paper,Vibe Magazine Subscriptions - 40 pack
>> 83.50

Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage
>> 71.25
 */
