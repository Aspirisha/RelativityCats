package models

/**
  * Created by andy on 12/19/16.
  */
import scala.util.Random

object MazeGen {
  type Pos = (Int, Int)

  sealed trait Cell {
    def empty = false
  }

  case object Wall extends Cell {
    override def toString = "W"
  }
  case object Empty extends Cell {
    override def toString = " "
    override def empty = true
  }
  case object Cat extends Cell {
    override def toString = "C"
  }
  case object Mouse extends Cell {
    override def toString = "m"
  }
  case object MouseHole extends Cell {
    override def toString = "h"
  }

  case class Maze(maze: Array[Array[Cell]]) {

    def size = maze.length

    override def toString = {
      maze map(_ map {case x => x.toString} mkString "") mkString "\n"
    }

    def apply(p: Pos) = {
      if (   p._1 <= 0
        || p._1 >= size - 1
        || p._2 <= 0
        || p._2 >= size - 1)
        Empty
      else
        maze(p._2)(p._1)
    }

    def update(p: Pos, v: Cell) = maze(p._2)(p._1) = v
  }

  def up(p: Pos): Pos = (p._1, p._2 + 1)
  def down(p: Pos): Pos = (p._1, p._2 - 1)
  def left(p: Pos): Pos = (p._1 - 1, p._2)
  def right(p: Pos): Pos = (p._1 + 1, p._2)

  def apply(size: Int): Maze = {
    val goodSize = if (size < 3) 3 else {
      // makes a nicer looking maze
      if (size % 2 == 0) {
        println(s"Need an odd number, using size = ${size + 1}")
        size + 1
      }
      else size
    }

    val maze = Maze(Array.tabulate(goodSize, goodSize)((_,_) => Wall))

    def doubleStep(p: Pos, f: Pos => Pos): Boolean = {
      if (!maze(f(p)).empty && !maze(f(f(p))).empty) {
        maze(f(p)) = Empty
        maze(f(f(p))) = Empty
        true
      }
      else
        false
    }

    val possibleMoves = List(up(_: Pos), down(_: Pos), left(_: Pos), right(_: Pos))
    def computePath(p: Pos) {
      for (direction <- Random.shuffle(possibleMoves)) {
        if (doubleStep(p, direction))
          computePath(direction(direction(p)))
      }
    }

    val start = (1, 1)
    val exit  = (goodSize - 2, goodSize - 1)

    computePath(start)

    maze
  }

}