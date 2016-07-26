package com.misoul.main

object Sort {
  trait Sorter {
    def sort(a: Seq[Integer]): Seq[Integer]
  }

  class QuickSort extends Sorter {
    def sort(a: Seq[Integer]): Seq[Integer] = {
      if (a.length < 2) a
      else {
        val pivot = a(0)
        sort(a.filter(_ < pivot)) ++ (a filter (_ == pivot)) ++ sort(a filter (pivot < _))
      }
    }
  }

  class MergeSort extends Sorter {
    private def merge(a: Seq[Integer], b: Seq[Integer]): Seq[Integer] = {
      (a ++ b) sorted // CHEATING but less important
                      // TODO: improve, see `scalac -feature src/com/misoul/main/*`
    }

    def sort(a: Seq[Integer]): Seq[Integer] = {
      a.size match {
      case 0 => Seq.empty[Integer] // Should not happen
      case 1 => a
      case 2 => merge(sort(a.slice(0, 1)), sort(a.slice(1, 2)))
      case _ =>
        val mid = a.size / 2
        merge(sort(a.slice(0, mid)), sort(a.slice(mid, a.size)))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val qSort = new QuickSort
    val mSort = new MergeSort
    val argsInt = args map(new Integer(_)) toSeq

    println("Inputs of length[" + args.length + "]: " + args.toSeq.mkString(","))

    println("Quick Sorted: " + qSort.sort(argsInt))
    println("Merge Sorted: " + mSort.sort(argsInt))

  }
}
