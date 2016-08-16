package com.misoul

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestCareerCup extends Specification {

  import CareerCup._

  "reverseArrayByGroup" should {
    "function properly for valid input" in {
      reverseArrayByGroup(Vector.empty, 1) === Vector.empty
      reverseArrayByGroup(Vector(1,2,3,4,5,6), 1) === Vector(1,2,3,4,5,6)
      reverseArrayByGroup(Vector(1,2,3,4,5,6), 2) === Vector(2, 1, 4, 3, 6, 5)
      reverseArrayByGroup(Vector(1,2,3,4,5,6), 3) === Vector(3, 2, 1, 6, 5, 4)
      reverseArrayByGroup(Vector(1,2,3,4,5,6,7,8), 3) === Vector(3, 2, 1, 6, 5, 4, 8, 7)
    }

    "throws a proper exception upon invalid input" in {
      reverseArrayByGroup(Vector(1,2,3,4,5,6), 0) must throwA[IllegalArgumentException]
    }
  }

  "spiralPrint2dArray" should {
    "function properly for basic inputs" in {
      spiralPrint2dArray(Array[Array[Int]](Array(1))) === "1"
      spiralPrint2dArray(Array[Array[Int]](Array(1,2))) === "12"
      spiralPrint2dArray(Array[Array[Int]](Array(1,2,3))) === "123"
      spiralPrint2dArray(Array[Array[Int]](Array(1),Array(2))) === "12"
      spiralPrint2dArray(Array[Array[Int]](Array(1),Array(2),Array(3))) === "123"
      spiralPrint2dArray(Array[Array[Int]](Array(1,2),
                                           Array(3,4))) === "1243"
      spiralPrint2dArray(Array[Array[Int]](Array(1,2,5),
                                           Array(3,4,6))) === "125643"
      spiralPrint2dArray(Array[Array[Int]](Array(1,2,5),
                                           Array(7,8,9),
                                           Array(3,4,6))) === "125964378"
      //TODO: size 3x4 4x3
      spiralPrint2dArray(Array[Array[Int]](Array(1,2,5,10),
                                           Array(7,8,9,11),
                                           Array(3,4,6,12),
                                           Array(13,14,15,16))) === "12510111216151413378964"
    }
  }

  "resolveWildcards" should {
    "tests" in {
      val NO_RESULT = "not-possible"
      resolveWildcards("SOCIA*TWIST", "SOCIALTWI*T" ) // Expect: "SOCIALTWIST"
      resolveWildcards("PROFESS*", "*PROFESS" ) // Expect: "PROFESS"
      resolveWildcards("*EXAMPLETEST", "THIRDEXAMPLE*" ) // Expect:  "THIRDEXAMPLETEST"
      resolveWildcards("*", "B*")  // Returns: "B"
      resolveWildcards("*C", "D*") // Returns: "DC"
      // resolveWildcards("*TELL", "*AFRIEND") === NO_RESULT
      resolveWildcards("HELLO", "HI*") === NO_RESULT
    }
  }

  "isSummable" should {
    "tests" in {
      isSummable(Seq(5), 0) === false
      isSummable(Seq(5), 5) === true
      isSummable(Seq(5,5), 55) === true
      isSummable(Seq(5,5), 10) === true
      isSummable(Seq(5,5), 1) === false
      isSummable(Seq(5,5), 5) === false
      isSummable(Seq(5,5), 50) === false
      isSummable(Seq(5,2,1,4,3,6,7,8), 333) === true
    }
  }


}
