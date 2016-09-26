package com.misoul


// abstract class BinaryTree[T <% Ordered[T]] (data: T) {
//   type BinaryNode = BinaryTree[T] //TODO: use type alias
//
//   val parent: Option[BinaryTree[T]] = None
//   val left: Option[BinaryTree[T]] = None
//   val right: Option[BinaryTree[T]] = None
//
//   // Recursively pass T down until encoutering a location for adding.
//   // Return: direct parent of T
//   def add(data: T): BinaryTree[T]
//
//   // Recursively pass T down until encoutering a location for adding.
//   // Return: direct parent of T
//   def add(node: BinaryTree[T]): BinaryTree[T]
// }


/////////////////////
//TODO: see http://codereview.stackexchange.com/questions/102470/binary-tree-implementation-in-scala
/////////////////////

//TODO: implement: isValid(), isBalanced()


// BST: BinarySearchTree
class BST[T <% Ordered[T]] (val data: T)  { // TODO: extends BinaryTree
  type Tree = BST[T]

  var parent: Option[Tree] = None //TODO: expose getter, but not setter
  var left: Option[Tree] = None
  var right: Option[Tree] = None
  // Alternative: instead of having parent/left/right as var, a new node has to
  //  be created for every change in the tree. This could breaks clients who keeps
  //  references to a node.

  def add(data: T): Tree = {
    add(new BST(data))
  }

  def add(node: BST[T]): BST[T] = {
    if (data > node.data) { // Go left
      if (left.isEmpty) {
        node.parent = Some(this)
        left = Some(node)
        this
      } else
        left.get.add(node)
    } else { // Go right
      if (right.isEmpty) {
        node.parent = Some(this)
        right = Some(node)
        this
      } else
        right.get.add(node)
    }
  }

  def printTree(): String = {
    import scala.collection.mutable.Queue
    import scala.collection.mutable.StringBuilder

    val emptyNodeStr = " * "
    val result = StringBuilder.newBuilder
    var queue = Queue[Option[BST[T]]](Some(this))
    var nextQueue = Queue[Option[BST[T]]]()
    var shouldContinue = true

    while ((queue.nonEmpty || shouldContinue) && queue.size < 15) {
      if (queue.isEmpty && shouldContinue) {
        shouldContinue = false

        val temp = queue
        queue = nextQueue
        nextQueue = temp

        result.append("\n")
      }

      val item = queue.dequeue()

      if (item.isEmpty) {
        result.append(emptyNodeStr)

        nextQueue.enqueue(item)
        nextQueue.enqueue(item)
      } else {
        val node = item.get
        result.append(node.data)

        if (node.left.nonEmpty || node.right.nonEmpty) shouldContinue = true
        nextQueue.enqueue(node.left)
        nextQueue.enqueue(node.right)
      }
    }

    result.toString()
  }
}

object BST extends App {
  def apply(input: Seq[String]): BST[String] = {
    if (input.isEmpty)
      throw new IllegalAccessError("Empty input")

    val root = new BST(input(0))

    input.drop(1).foreach { root.add(_) }

    root
  }

  val root = BST(Seq("1", "2", "3", "4"))

  println("======")
  println(root.printTree())
}
