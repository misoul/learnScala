package com.misoul.main

import scala.io.Source

object DigestLogfile {
  val fileName = "input/server.20160726.log"

  def main(args: Array[String]) = {
    val src = Source fromFile fileName
    val lines = src.getLines map (_ split " ") // Iterator[Array[String]]

    val output = lines filter(_.length > 3) map { fields => fields(0) + "/" + fields(2) }

    println(output.mkString("\n"))

    src.close()

    /*
      Note:
      - Iterator: Instead of attempting to load the entire file into memory, lines
      will be parsed on demand. This is great for performance and memory pressure,
      but also means that you must be careful to only close the source after you’ve
      finished processing it. Also these iterators can only be used once.
     */
  }

}
