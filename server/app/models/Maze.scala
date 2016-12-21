package models

/**
  * Created by andy on 12/19/16.
  */
import scala.util.Random

object Maze {
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

  def apply(size: Int, actorsNumber: Int): Maze = {
    val goodSize = if (size < 3) 3
    else {
      // makes a nicer looking maze
      if (size % 2 == 0) size + 1 else size
    }

    new Maze(goodSize, actorsNumber)
  }

}

class Maze(var size: Int, actorsNumber: Int) {
  import Maze._
  val roadmap: Array[Array[Cell]] = Array.tabulate(size, size)((_, _) => Wall)

  override def toString = {
    roadmap map (_ map { case x => x.toString } mkString "") mkString "\n"
  }

  def apply(p: Pos) = {
    if (p._1 <= 0
      || p._1 >= size - 1
      || p._2 <= 0
      || p._2 >= size - 1)
      Empty
    else
      roadmap(p._2)(p._1)
  }

  def update(p: Pos, v: Cell) = roadmap(p._2)(p._1) = v


  def up(p: Pos): Pos = (p._1, p._2 + 1)

  def down(p: Pos): Pos = (p._1, p._2 - 1)

  def left(p: Pos): Pos = (p._1 - 1, p._2)

  def right(p: Pos): Pos = (p._1 + 1, p._2)

  private def doubleStep(p: Pos, f: Pos => Pos): Boolean = {
    if (!apply(f(p)).empty && !apply(f(f(p))).empty) {
      update(f(p), Empty)
      update(f(f(p)), Empty)
      true
    }
    else
      false
  }

  val possibleMoves = List(up(_: Pos), down(_: Pos), left(_: Pos), right(_: Pos))
  private def computePath(p: Pos) {
    for (direction <- Random.shuffle(possibleMoves)) {
      if (doubleStep(p, direction))
        computePath(direction(direction(p)))
    }
  }

  val exit = {
    // we don't want even coordinates since then maze has fat walls
    def validator(v: Int) = if (v % 2 == 1) v else v - 1
    val rawExit = {
      val res = (Random.shuffle(List(0, size - 1)).head, validator(1 + Random.nextInt(size - 3)))
      if (Random.nextBoolean()) res.swap else res
    }
    rawExit
  }

  computePath(exit)
  update(exit, Empty)
  def findFreeCell = {

  }
}

class MazeView {

}