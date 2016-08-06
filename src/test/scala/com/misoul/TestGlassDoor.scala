package com.misoul

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestGlassDoor extends Specification {

  import GlassDoor._

  "TwoStackQueue" should {
    "handle edge cases" in {
      val tsq = new TwoStackQueue[Int]()

      tsq.length === 0
      tsq.dequeue() === None
      tsq.dequeue() === None
      tsq.dequeue() === None
      tsq.length === 0
    }

    "handle regular cases" in {
      val tsq = new TwoStackQueue[Int]()

      tsq.enqueue(1) === tsq
      tsq.length === 1
      tsq.dequeue() === Some(1)
      tsq.dequeue() === None
      tsq.length === 0
      tsq.enqueue(2) === tsq
      tsq.enqueue(3) === tsq
      tsq.enqueue(4) === tsq
      tsq.length === 3
      tsq.dequeue() === Some(2)
      tsq.enqueue(5) === tsq
      tsq.enqueue(6) === tsq
      tsq.length === 4
      tsq.dequeue() === Some(3)
      tsq.dequeue() === Some(4)
      tsq.dequeue() === Some(5)
      tsq.dequeue() === Some(6)
      tsq.length == 0
    }

  }



}
