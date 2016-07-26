package com.misoul.main

object Sort {
  trait Sorter[T] {
    def sort(a: Seq[T]): Seq[T]
  }

  class BubbleSort[T <% Ordered[T]] extends Sorter[T] {
    override def sort(a: Seq[T]): Seq[T] = {
      // Do-able, but a bit silly
      return Seq.empty
    }
  }

  class QuickSort[T <% Ordered[T]] extends Sorter[T] {
    def sort(a: Seq[T]): Seq[T] = {
      if (a.length < 2) a
      else {
        val pivot = a(0)
        sort(a.filter(_ < pivot)) ++ (a filter (_ == pivot)) ++ sort(a filter (pivot < _))
      }
    }
  }

  class MergeSort[T <% Ordered[T]] extends Sorter[T] {
    private def merge(a: Seq[T], b: Seq[T]): Seq[T] = {
      (a ++ b) sorted // CHEATING but less important
                      // TODO: improve, see `scalac -feature src/com/misoul/main/*`
    }

    def sort(a: Seq[T]): Seq[T] = {
      a.size match {
      case 0 => Seq.empty[T] // Should not happen
      case 1 => a
      case 2 => merge(sort(a.slice(0, 1)), sort(a.slice(1, 2)))
      case _ =>
        val mid = a.size / 2
        merge(sort(a.slice(0, mid)), sort(a.slice(mid, a.size)))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val qSort = new QuickSort[String]
    val mSort = new MergeSort[String]
    val argsInt = args map(new String(_)) toSeq

    println("Inputs of length[" + args.length + "]: " + args.toSeq.mkString(","))

    println("Quick Sorted: " + qSort.sort(argsInt))
    println("Merge Sorted: " + mSort.sort(argsInt))

  }
}
