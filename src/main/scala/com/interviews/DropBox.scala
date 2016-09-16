package com.interviews

case class Bucket(start: Long, count: Long)

case class URL(nWindows: Int) { // Each window is of 1s length each
  private val _buffer: Array[Option[Bucket]] = (0 until nWindows).map(i => None).toArray
  private var _index = -1

  private val _windowSize = 1000 // miliseconds

  private var _currentWindowStart = System.currentTimeMillis
  private var _currentCount = 0L

  def logHit(): Unit = {
    checkCurrentBucket()
    _currentCount += 1
  }

  def countHits(): Long = {
    checkCurrentBucket()
    getCount(System.currentTimeMillis - _windowSize*nWindows) + _currentCount
  }

  private def checkCurrentBucket() = { // This is called to correct current bucket after sleeping for a long time
    val now = System.currentTimeMillis

    if (now - _currentWindowStart > _windowSize) {
      add(Bucket(_currentWindowStart, _currentCount))
      _currentWindowStart = now
      _currentCount = 0
    }
  }

  private def add(bucket: Bucket): Unit = { // Not returning overwritten bucket for now
    _index = (_index + 1) % nWindows
    _buffer(_index) = Some(bucket)
  }

  private def getCount(time: Long): Long = {
    _buffer.filter(item => item.nonEmpty && item.get.start > time) //TODO: improve with binary search
           .map(_.get.count).sum
  }

  override def toString = _buffer.filter(_.nonEmpty).map(_.get).mkString(" ") + " # " + _index
}


object DropBox extends App {
  val url = new URL(3)

  (0 to 5). foreach { i => Thread.sleep(1000); url.logHit() }

  println(url.countHits())
}
