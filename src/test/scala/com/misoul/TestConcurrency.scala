package com.misoul

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestConcurrency extends Specification {

  import Concurrency._

  "return10.1" should {
    "Always return 10" in {
      return10 === 10
    }

    "Always return 10, 2nd time" in {
      return10 === 10
    }
  }

}
