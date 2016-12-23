package models

/**
  * Created by andy on 12/19/16.
  */
import scala.annotation.tailrec
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
  case object CatCell extends Cell {
    override def toString = "C"
  }
  case object MouseCell extends Cell {
    override def toString = "m"
  }
  case object MouseHole extends Cell {
    override def toString = "h"
  }
  case object Invisible extends Cell {
    override def toString = "?"
  }

  def apply(size: Int, actorsNumber: Int): Maze = {
    val goodSize = if (size < 3) 3
    else {
      // makes a nicer looking maze
      if (size % 2 == 0) size + 1 else size
    }

    new Maze(goodSize, actorsNumber)
  }

  case class RoadMap(raw : Array[Array[Cell]]) {
    val size = raw.length

    def apply(p: Pos) = {
      if (p._1 <= 0
        || p._1 >= size - 1
        || p._2 <= 0
        || p._2 >= size - 1)
        Empty
      else
        raw(p._2)(p._1)
    }

    def update(p: Pos, v: Cell) = raw(p._2)(p._1) = v

    override def toString = {
      raw map (_ map { case x => x.toString } mkString "") mkString "\n"
    }
  }


}

class Maze(var size: Int, actors: Int) {
  import Maze._
  import scala.collection.mutable.{Map => MutableMap}
  val roadmap = RoadMap(Array.tabulate(size, size)((_, _) => Wall))

  // to make everything more mind boggling, let's assume universe was just born
  // and so no previous state exist for any point
  val stateAtMomentTMinusN: Array[Array[List[Cell]]] = Array.tabulate(size, size)((_, _) =>
    List.fill(size)(Invisible))

  override def toString = {
    roadmap toString
  }

  def up(p: Pos): Pos = (p._1, p._2 + 1)

  def down(p: Pos): Pos = (p._1, p._2 - 1)

  def left(p: Pos): Pos = (p._1 - 1, p._2)

  def right(p: Pos): Pos = (p._1 + 1, p._2)

  private def doubleStep(p: Pos, f: Pos => Pos): Boolean = {
    if (!roadmap(f(p)).empty && !roadmap(f(f(p))).empty) {
      roadmap update(f(p), Empty)
      roadmap update(f(f(p)), Empty)
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
      val res = (Random.shuffle(List(0, size)).head, validator(1 + Random.nextInt(size - 2)))
      if (Random.nextBoolean()) res.swap else res
    }
    rawExit
  }

  computePath(exit)
  roadmap.update(exit, Empty)

  @tailrec
  private def addCharacter(character: Cell): MazeView = {
    val p = (Random.nextInt(size), Random.nextInt(size))
    if (roadmap(p) empty) {
      roadmap.update(p, character)
      MazeView(size, p, character)
    } else addCharacter(character)
  }

  def addCharacters(players: List[GameCharacter]): List[MazeView] = {
    players map {
      case Mouse(_) => addCharacter(MouseCell)
      case Cat(_) => addCharacter(CatCell)
    }
  }

  def renderTimeStep(views: List[MazeView]) = {

  }

  private def isValidPos(p: Pos): Boolean = {
    p._1 >= 0 && p._2 >= 0 && p._1 < size && p._2 < size
  }

  private def validateStep(from: Pos, to: Pos): Boolean = {
    if (!isValidPos(to))
      return false
    val diff = (to._1 - from._1, to._2 - from._2)
    val sum = Math.abs(diff._1) + Math.abs(diff._2)
    if (sum != 1)
      return false

    roadmap(to) == Wall
  }

  def renderCharacterStep(view: List[MazeView], from: Pos, to: Pos): Boolean = {
    if (!validateStep(from, to))
      return false

    roadmap(to) match {
      case Empty =>
        roadmap update(to, roadmap(from))
        roadmap update(from, Empty)
        true
      case Wall => false
      case Maze.CatCell if roadmap(from) == Maze.CatCell => false
      case Maze.CatCell => true
    }


  }

  case class MazeView(size: Int, var position: Pos, character: Cell) {
    import Maze._

    val visibleRoadMap = RoadMap(Array.tabulate(size, size)((_, _) => Invisible))
    val staticRoadMap = RoadMap(Array.tabulate(size, size)((_, _) => Invisible))

    visibleRoadMap.update(position, character)
    staticRoadMap.update(position, Empty)
  }
}

