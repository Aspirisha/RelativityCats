package shared.models

/**
  * Created by andy on 12/19/16.
  */
import com.uniformlyrandom.jello.JelloValue.{JelloArray, JelloNumber, JelloObject}
import com.uniformlyrandom.jello.{JelloFormat, JelloReader, JelloValue, JelloWriter}
import shared.models.Maze.RoadMap
import upickle.Js

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Maze {
  object Cell {
    val empty = Cell(" ")
    val wall = Cell("W")
    val cat = Cell("C")
    val mouse = Cell("M")
    val exit = Cell("E")
    val unknown = Cell("?")

    implicit val fmt: JelloFormat[Cell] = JelloFormat.format[Cell]
  }

  case class Cell(name: String) {
    def empty = name == "W" || name == "E"
  }

  def isValidPos(p: Vector2, size: Int): Boolean = {
    p.x >= 0 && p.y >= 0 && p.x < size && p.y < size
  }

  sealed trait Event

  case object InvalidStep extends Event
  case object MouseEscape extends Event
  case object Escape extends Event
  case object MouseSteppedIntoCat extends Event
  case object CatCaughtMouse extends Event
  case object JustStep extends Event

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

    def apply(p: Vector2) = {
      if (p.x <= 0
        || p.x >= size - 1
        || p.y <= 0
        || p.y >= size - 1)
        Cell.empty
      else
        raw(p.y)(p.x)
    }

    def update(p: Vector2, v: Cell) = raw(p.y)(p.x) = v

    override def toString = {
      raw map (_ map { case x => x.toString } mkString "") mkString "\n"
    }
  }

  object RoadMap {
    implicit val reader: JelloReader[RoadMap] = {
      new JelloReader[RoadMap] {
        override def read(jelloValue: JelloValue): Try[RoadMap] = {
          jelloValue match {
            case p: JelloArray =>
              Try(RoadMap(p.seq.map{case row: JelloArray =>
                row.seq.map {
                  case x: JelloValue => Cell.fmt.read(x).get
                }.toArray[Cell]
              }.toArray[Array[Cell]]))
          }
        }
      }
    }

    implicit val writer: JelloWriter[RoadMap] = {
      new JelloWriter[RoadMap] {
        override def write(v: RoadMap): JelloValue = {
          JelloArray(v.raw.map{ case row => JelloArray(row.map{ case v => Cell.fmt.write(v)})})
        }
      }
    }
  }

}

class Maze(var size: Int, actors: Int) {
  import Maze._

  import scala.collection.mutable.{Map => MutableMap}
  val roadmap = RoadMap(Array.tabulate(size, size)((_, _) => Cell.wall))

  // to make everything more mind boggling, let's assume universe was just born
  // and so no previous state exist for any point
  var visibilityMap: List[RoadMap] = List.fill[RoadMap](size)(
    RoadMap(Array.tabulate(size, size)((_, _) => Cell.unknown)))

  override def toString = {
    roadmap toString
  }

  def up(p: Vector2): Vector2 = p + Vector2.unitY

  def down(p: Vector2): Vector2 = p - Vector2.unitY

  def left(p: Vector2): Vector2 = p - Vector2.unitX

  def right(p: Vector2): Vector2 = p + Vector2.unitX

  private def doubleStep(p: Vector2, f: Vector2 => Vector2): Boolean = {
    if (!roadmap(f(p)).empty && !roadmap(f(f(p))).empty) {
      roadmap update(f(p), Cell.empty)
      roadmap update(f(f(p)), Cell.empty)
      true
    }
    else
      false
  }

  val possibleMoves = List(up(_: Vector2), down(_: Vector2), left(_: Vector2), right(_: Vector2))
  private def computePath(p: Vector2) {
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
  roadmap.update(exit, Cell.exit)

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
      case Mouse(_) => addCharacter(Cell.mouse)
      case Cat(_) => addCharacter(Cell.cat)
    }
  }

  def renderTimeStep(views: List[MazeView]) = {
    for (i <- size - 2 to 0 by -1) {
      visibilityMap = visibilityMap.last :: visibilityMap.takeRight(size - 1)
      for {i <- 0 until size
           j <- 0 until size} {
        visibilityMap.head.update((i, j), roadmap(i, j))
      }
    }

    views foreach { case v => v.render(visibilityMap) }
  }

  private def validateStep(from: Vector2, to: Vector2): Boolean = {
    isValidPos(to, size) && (to - from).magnitude == 1
  }

  def renderCharacterStep(view: MazeView, to: Vector2): Event = {
    if (!validateStep(view.position, to))
      return InvalidStep

    (roadmap(view.position), roadmap(to)) match {
      case (_, Cell.empty) =>
        roadmap update(to, roadmap(view.position))
        roadmap update(view.position, Cell.empty)
        view.performStep(to, visibilityMap)
        JustStep
      case (Cell.mouse, Cell.cat) =>
        roadmap update(view.position, Cell.empty)
        MouseSteppedIntoCat
      case (Cell.cat, Cell.mouse) =>
        roadmap update(to, roadmap(view.position))
        roadmap update(view.position, Cell.empty)
        view.performStep(to, visibilityMap)
        CatCaughtMouse
      case (_, Cell.exit) =>
        roadmap update(view.position, Cell.empty)
        Escape
      case (_, _) => InvalidStep
    }
  }
}


object MazeView {
  import Maze._
  def apply(size: Int, position: Vector2, character: Maze.Cell): MazeView = {
    val visibleRoadMap = RoadMap(Array.tabulate(size, size)((_, _) => Cell.unknown))
    val staticRoadMap = RoadMap(Array.tabulate(size, size)((_, _) => Cell.unknown))
    visibleRoadMap.update(position, character)
    staticRoadMap.update(position, Cell.empty)
    MazeView(size, position, visibleRoadMap, staticRoadMap)
  }

  implicit val reader: JelloReader[MazeView] = {
    new JelloReader[MazeView] {
      override def read(jelloValue: JelloValue): Try[MazeView] = {
        jelloValue match {
          case p: JelloObject =>
            for {size <- Try(p.map("size").asInstanceOf[JelloNumber].v.toInt)
                 pos <- Vector2.reader.read(p.map("position"))
                 visibleMap <- RoadMap.reader.read(p.map("visible_map"))
                 staticMap <- RoadMap.reader.read(p.map("visible_map"))}
              yield MazeView(size, pos, visibleMap, staticMap)
        }
      }
    }
  }

  implicit val writer: JelloWriter[MazeView] = {
    new JelloWriter[MazeView] {
      override def write(v: MazeView): JelloValue = {
        JelloObject (Map[String, JelloValue](
          "size" -> JelloNumber(v.size),
          "position" -> Vector2.writer.write(v.position),
          "visible_map" -> RoadMap.writer.write(v.visibleRoadMap),
          "static_map" -> RoadMap.writer.write(v.staticRoadMap)))
      }
    }
  }
}

case class MazeView(size: Int, var position: Vector2,
                    visibleRoadMap: Maze.RoadMap, staticRoadMap: Maze.RoadMap) {
  import Maze._
  val character = visibleRoadMap(position)

  def performStep(to: Vector2, visibilityMap: List[RoadMap]): Unit = {
    visibleRoadMap update (position, Cell.empty)
    position = to
    visibleRoadMap update (position, character)
    render(visibilityMap)
  }

  def render(visibilityMap: List[RoadMap]): Unit = {
    @tailrec
    def bfsUpdate(dist: Int, step: Vector2): Unit = {
      val pos = position + dist * step
      if (isValidPos(pos, size)) {
        visibleRoadMap update(pos, visibilityMap(dist)(pos))
        staticRoadMap(pos) match {
          case Cell.unknown =>
            val newStaticValue = visibleRoadMap(pos) match {
              case Cell.wall => Cell.wall
              case Cell.exit => Cell.exit
              case _ => Cell.empty
            }
            staticRoadMap update(pos, newStaticValue)
        }

        if (visibleRoadMap(pos).empty) {
          bfsUpdate(dist + 1, step)
        }
      }
    }
    List((0, 1), (0, -1), (-1, 0), (1, 0)) foreach { case step => bfsUpdate(1, step) }
  }
}