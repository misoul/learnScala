package com.interviews.airbnb

import com.interviews.airbnb.ShuffleTile.Tiles

import scala.collection.immutable.Queue

//TODO: Board should be collapsed into State
object ShuffleTile extends App {
  type Pair = (Int, Int)
  val emptySlot = 0
  val nRows = 3
  val nCols = 3

  case class Board(tiles: Vector[Vector[Int]]) {
    override def toString = tiles.map { _.mkString("") }.mkString("##")

    def swap(a: Location, b: Location): Board = {
      val newTiles = tiles.map(_.to[Array]) //TODO: is there a better way?
      val temp = newTiles(a.row)(a.col)
      newTiles(a.row)(a.col) = newTiles(b.row)(b.col)
      newTiles(b.row)(b.col) = temp
      Board(newTiles.map(_.to[Vector]))
    }

    def swap(a: Pair, b: Pair): Board = swap(Location(a), Location(b))
  }

  object Board {
    val targetTiles = (0 until nRows).map { i => (0 until nCols).map(_ + i * nCols).to[Vector] }.to[Vector]
    val target = Board(targetTiles)
  }

  case class Location(row: Int, col: Int) {
    def isValid(): Boolean = (0 <= row && row < nRows && 0 <= col && col < nCols)

    def applyOffSet(offset: Pair): Location = Location(row + offset._1, col + offset._2)

    override def toString = row + "," + col
  }

  object Location {
    def apply(p: Pair): Location = Location(p._1, p._2)
  }

  case class State(board: Board, empty: Location) {
    // empty: coordinate of the "0"
    override def toString = board.toString + " ~ " + empty.toString

    def neighbors(): Set[State] = {
      val offsets = Set((0, -1), (0, 1), (-1, 0), (1, 0))
      offsets.map { empty.applyOffSet(_) }
        .filter(_.isValid())
        .map { l => State(board.swap(empty, l), l) }
    }

    override def hashCode = toString.hashCode

    override def equals(o: Any) = (o.isInstanceOf[State] && toString.equals(o.toString))
  }

  object State {
    val target = State(Board.target, Location(0, 0))
  }

  def explore(queue: Queue[State], visited: Set[State]): Boolean = {
    if (queue.isEmpty) return false

    val (state, left) = queue.dequeue

    state match {
      case state if state == State.target => true
      case state => {
        val nextStates = state.neighbors.filter(!visited.contains(_))
        explore(left.enqueue(nextStates), visited ++ Set(state))
      }
    }
  }

//  def walk(tiles: Tiles): Boolean = {
//    explore(Queue(State))
//  }

  type Tiles = Vector[Vector[Int]]

//  val initialBoard = Board.target.swap((0,0),(0,1)) //true
//                                 .swap((1,0),(1,1)) //false
//  val initialState = State(initialBoard, Location(0,1))

//  val tiles1 = Vector(Vector(0,1,2),Vector(3,4,5),Vector(6,7,8)) Location(2,2) //true
  val tiles1 = Vector(Vector(1,2,5),Vector(3,4,8),Vector(6,7,0))
  val state1 = State(Board(tiles1), Location(2,2))
  println("Initial: " + state1)
  println("Target : " + State.target)
  println("REACHABLE: " + explore(Queue(state1), Set.empty))
}
