package com.misoul

import java.util.concurrent.locks.{Lock, ReentrantLock, Condition}
import scala.concurrent.duration._
import scala.util.{Random, Failure, Success}
import scala.collection.mutable.{Stack}


object GlassDoor {

  class TwoStackQueue[T] { //TODO: make it thread-safe
    private val _stack1 = new Stack[T]()
    private val _stack2 = new Stack[T]()
    private var _length: Int = 0

    def length(): Int = _length

    def enqueue(data: T): TwoStackQueue[T] = {
      _stack1.push(data)
      _length += 1
      this
    }

    def dequeue(): Option[T] = {
      if (_stack2.length == 0) {
        if (_stack1.length == 0) return None

        (1 to _stack1.length) foreach { i => val data = _stack1.pop(); _stack2.push(data); }
      }

      _length -= 1
      return Some(_stack2.pop())
    }
  }

  //TODO: thread safe singleton class in Java
}
