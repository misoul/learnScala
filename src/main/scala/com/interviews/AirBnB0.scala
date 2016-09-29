package com.interviews

// Display Unique Hosts on Pages
object AirBnB0 extends App {
  import scala.collection.mutable.{Queue, Set}


  // for (i <- 0 until 5) println("Hello, world!")
  case class Entry(hostId: Int, listId: Int, score: Double, city: String)

  case class Bucket(entries: Queue[Entry], hosts: Set[Int], maxSize: Int) {
    def contains(hostId: Int) = hosts.contains(hostId)
    def add(entry: Entry) = {
      entries += entry
      hosts += entry.hostId
    }
    def hasRoom = entries.length < maxSize
    def isFull = entries.length == maxSize
  }


  def getPages(input: Seq[Entry], pageSize: Int): Seq[Bucket] = {
    def calBuckets(count: Int, size: Int): Int = {
      if (count % size == 0)
        return count / size
      else
        return (count/size + 1)
    }

    def createBuckets(size: Int, bucketSize: Int): Seq[Bucket] = {
      val buckets = (0 until size) map { i => Bucket(Queue[Entry](), Set[Int](), bucketSize) }
      buckets.to[Seq]
    }

    val nBucket = calBuckets(input.length, pageSize)
    val buckets = createBuckets(nBucket, pageSize)

    input.foreach { entry =>
      var i = 0

      while (i < nBucket && (buckets(i).isFull || buckets(i).contains(entry.hostId))) i += 1
      if (i >= nBucket) i = nBucket-1;

      buckets(i).add(entry)
    }

    buckets
  }

  println("Starting...")

  val input = Seq(Entry(1,28,300.1,"San Francisco"),
                  Entry(1,28,300.1,"San Francisco"),
                  Entry(3,28,300.1,"San Francisco"),
                  Entry(4,28,300.1,"San Francisco"))

  val pages = getPages(input, 2)
  println(pages)
  println("Done")


}

/*
Host Crowding

You are given an array of csv strings indicating search results.  Each has a host_id, listing_id, score, and city.  Initially, results are sorted by highest score.

We’d like to display these search results on a web page.  Write a function that returns groups of listings to be displayed on each page.  However, note that a given host may have several listings that show up in the results.  Reorder the list so that a host shows up at most once on a page if possible, but otherwise preserves the ordering.

Your program should return the new array and print out the results in blocks representing the pages.

Input:
*  An array of csv strings, with sort score
*  number of results per page

int PER_PAGE = 12;

ArrayList<String> input = new ArrayList<String>();
input.add("host_id,listing_id,score,city");
input.add("1,28,300.1,San Francisco");
input.add("4,5,209.1,San Francisco");
input.add("20,7,208.1,San Francisco");
input.add("23,8,207.1,San Francisco");
input.add("16,10,206.1,Oakland");
input.add("1,16,205.1,San Francisco");
input.add("6,29,204.1,San Francisco");
input.add("7,20,203.1,San Francisco");
input.add("8,21,202.1,San Francisco");
input.add("2,18,201.1,San Francisco");
input.add("2,30,200.1,San Francisco");
input.add("15,27,109.1,Oakland");
input.add("10,13,108.1,Oakland");
input.add("11,26,107.1,Oakland");
input.add("12,9,106.1,Oakland");
input.add("13,1,105.1,Oakland");
input.add("22,17,104.1,Oakland");
input.add("1,2,103.1,Oakland");
input.add("28,24,102.1,Oakland");
input.add("18,14,11.1,San Jose");
input.add("6,25,10.1,Oakland");
input.add("19,15,9.1,San Jose");
input.add("3,19,8.1,San Jose");
input.add("3,11,7.1,Oakland");
input.add("27,12,6.1,Oakland");
input.add("1,3,5.1,Oakland");
input.add("25,4,4.1,San Jose");
input.add("5,6,3.1,San Jose");
input.add("29,22,2.1,San Jose");
input.add("30,23,1.1,San Jose");
*/
