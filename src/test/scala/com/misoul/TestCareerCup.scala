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

}
