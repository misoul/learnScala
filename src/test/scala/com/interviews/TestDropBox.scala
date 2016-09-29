package com.interviews

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestDropBox extends Specification {
  import DropBox._

  "URL.countHits" should {
    "functions for corner cases" in {
      val url = new URL(3)
      url.countHits() === 0

      (0 to 5). foreach { i => url.logHit() }
      Thread.sleep(6 * 1000 + 100)
      url.countHits() === 0
    }

    "functions for basic cases" in {
      val url = new URL(3)
      url.logHit()
      url.countHits() === 1
      url.logHit()
      url.countHits() === 2
      url.logHit()
      url.countHits() === 3
      url.logHit()
      url.countHits() === 4

      Thread.sleep(1000)

      url.logHit()
      url.countHits() === 5
      url.logHit()
      url.countHits() === 6
      url.logHit()
      url.countHits() === 7
      url.logHit()
      url.countHits() === 8

      Thread.sleep(3001)
      url.logHit()
      url.countHits() === 1
    }

    "functions for more complicated cases" in {
      val url = new URL(5)

      (1 to 100).foreach { i =>
        Thread.sleep(30)
        (1 to i).foreach { j => url.logHit() }
      }
      url.countHits() === 100 * 101 / 2


      (1 to 100).foreach { i =>
        Thread.sleep(30)
        (1 to i).foreach { j => url.logHit() }
      }
      url.countHits() < 100 * 101
    }
  }
}