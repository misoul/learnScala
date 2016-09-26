package com.interviews

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestUber extends Specification {

  "RansomNote" should {
    "functions function properly" in {
      true === RansomNote("This is from a magazine.", "is This from Mag").isPossible()
      false === RansomNote("This is from a magazine.", "is This from Magg").isPossible()
      true === RansomNote("This is from a magazine.", "is This from Mag.").isPossible()
      false === RansomNote("This is from a magazine.", "is This from Magg..").isPossible()
    }
  }

  "FindNthElement" should {
    "fromUnsorted" in {
      val f = new FindNthElement()

      val a = Vector(Vector(1,3,5,7,9))
      f.fromUnsorted(a, 3) === 5
      f.fromUnsorted(a, 1) === 1
      f.fromUnsorted(a, 5) === 9

      val b = Vector(
        Vector(1,3,5,7,9),
        Vector(2,4,6,8,10)
      )
      f.fromUnsorted(b, 3) === 3
      f.fromUnsorted(b, 1) === 1
      f.fromUnsorted(b, 5) === 5

      val c = Vector(
        Vector(2,4,6,8,10),
        Vector(1,3,5,7,9)
      )
      f.fromUnsorted(c, 3) === 3
      f.fromUnsorted(c, 1) === 1
      f.fromUnsorted(c, 5) === 5

      val d = Vector(
        Vector(2,4,6,8,10),
        Vector(1,3,5,7,9),
        Vector(0,11,12,13,14,15)
      )
      f.fromUnsorted(d, 3) === 2
      f.fromUnsorted(d, 1) === 0
      f.fromUnsorted(d, 15) === 14
    }
  }

  "FindShortestBinA" should {
    "basic tests" in {
      val f = new FindShortestBinA()

      f.find("abcd", "ac") === 3
      f.find("abcdbcdcab", "dabb") === 4
      f.find("abcdbcdcabbd", "dabb") === 5
    }
  }

  "BinaryTree Serialization" should {
    val str = "a#b#c#d##" // "abcd#####" //"abd#f##eg###ch##i##"//"ab##c##"
    val n = Node.fromString(str)
    println(n.get)
  }
}