package example

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorSystem, Props}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLImageElement
import shared._
import shared.models.Maze.Cell
import shared.models.{GameCharacter, Maze, MazeView, Vector2}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import scala.scalajs.js.annotation.JSExport
case class Point(x: Int, y: Int){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Int) = Point(x / d, y / d)
}

@JSExport
object RoomFrontend extends js.JSApp {
  var cellSize: Vector2 = Vector2.zero
  val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]

  object Sprite {
    def getImageUri(document: Document, imgName: String): String = {
      val accesedImage = s"${dom.document.location.protocol}//${dom.document.location.host}/assets/images/$imgName"
      g.console.debug(accesedImage)
      accesedImage
    }

    val imagesNumberGCD = 2

    def apply(img: String): Sprite = {
      Sprite(List(img))
    }
  }

  case class Sprite(imgnames: List[String]) {
    var loaded = new AtomicInteger(0)
    val images = imgnames map { case imgname =>
      val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = Sprite.getImageUri(dom.document, imgname)
      image.onload = (x: dom.Event) => {
        loaded.incrementAndGet()
      }
      image
    }

    def draw(pos: Vector2, ctx: dom.CanvasRenderingContext2D, timer: Int): Unit = {
      val image = images(timer % images.size)
      ctx.drawImage(
        image,
        0,
        0,
        image.width,
        image.height,
        pos.x * cellSize.x,
        pos.y * cellSize.y,
        cellSize.x,
        cellSize.y)

      ctx.lineWidth = 1
      ctx.strokeStyle = "#A57D00"
      ctx.rect(pos.x * cellSize.x, pos.y * cellSize.y, cellSize.x, cellSize.y)
      ctx.stroke()
    }
  }

  val catSprite = Sprite(List("cat1.png", "cat2.png"))
  val mouseSprite = Sprite(List("mouse1.png", "mouse2.png"))
  val unknownSprite = Sprite("unknown1.png")
  val floorSprite = Sprite("floor.png")
  val wallSprite = Sprite("wall.png")

  object Render {
    def props(ctx: dom.CanvasRenderingContext2D, username: String): Props = Props(new Render(ctx, username))

    case object RedrawMessage
  }

  class Render(ctx: dom.CanvasRenderingContext2D, username: String) extends Actor {
    var maze: Option[MazeView] = None
    var currentPlayer = ""
    var players = List.empty[GameCharacter]
    var timer = 0

    def drawCell(pos: Vector2, cell: Cell): Unit = {
      cell match {
        case Cell.unknown =>
          unknownSprite.draw(pos, ctx, timer)
        case Cell.cat =>
          floorSprite.draw(pos, ctx, timer)
          catSprite.draw(pos, ctx, timer)
        case Cell.mouse =>
          floorSprite.draw(pos, ctx, timer)
          mouseSprite.draw(pos, ctx, timer)
        case Cell.wall => wallSprite.draw(pos, ctx, timer)
        case Cell.empty => floorSprite.draw(pos, ctx, timer)
      }
    }

    def redraw = {
      maze match {
        case Some(m) =>
          for {i <- 0 until m.size
               j <- 0 until m.size} {
            drawCell((i, j), m.visibleRoadMap((i, j)))
          }
        case None =>
      }
    }

    override def receive: Receive = {
      case Render.RedrawMessage =>
        if (currentPlayer == username) {
          for (m <- maze) {
            drawCell(m.position, m.visibleRoadMap(m.position))
          }
          timer += 1
          if (timer == Sprite.imagesNumberGCD)
            timer = 0
        }
      case msg: NotifyGameStart =>
        players = msg.players
      case msg: UserView =>
        maze = Some(msg.data)
        g.console.debug(s"Got the maze ${maze}")
        cellSize = Vector2(canvas.width / msg.data.size, canvas.height / msg.data.size)
        redraw
      case msg: NotifyActiveUser =>
        currentPlayer = msg.activeUser
        g.console.debug(s"Now active user is $currentPlayer")
      case TryMoveResult(event, delta, _) =>
        maze match {
          case None =>
          case Some(m) =>
            event match {
              case Maze.Event.justStep =>
                m.visibleRoadMap update (m.position, Cell.empty)
                m.position += delta
                m.visibleRoadMap update (m.position, m.character)
                redraw
              case _ => g.console.debug("Implement me!")
            }
        }
      case msg: NotifyTimeStep =>
        maze = Some(msg.mazeView)
        redraw
    }
  }

  object SocketActor {
    def props(name: String): Props = Props(new SocketActor(name))
  }

  class SocketActor(name: String) extends Actor {
    var players = List.empty[GameCharacter]
    val render = context.actorSelection(s"/user/render-$name")
    val inputController = context.actorSelection(s"/user/input-$name")

    def getWebsocketUri(document: Document): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

      g.console.debug(s"host is ${dom.document.location.host}")
      s"$wsProtocol://${dom.document.location.host}/game/socket?name=$name"
    }

    val socket = new WebSocket(getWebsocketUri(dom.document))

    socket.onmessage = { (event: MessageEvent) =>
      val msg = Message.deserialize(event.data.toString)
      g.console.debug(s"received ${msg.messageType} message from server")
      msg match {
        case msg: RoomStatMessage =>
          val userList = dom.document.getElementById("users").asInstanceOf[html.UList]
          val len = userList.childNodes.length
          for (i <- 1 to len)
            userList.removeChild(userList.childNodes.item(0))
          msg.data.users foreach {
            case name =>
              val li = dom.document.createElement("li")
              val element = li.appendChild(dom.document.createTextNode(name))
              userList.appendChild(li)
          }
        case msg: NotifyGameStart =>
          players = msg.players
          render ! msg
        case msg: UserView =>
          render ! msg
        case msg: NotifyActiveUser =>
          inputController ! msg
          render ! msg
        case msg: TryMoveResult =>
          g.console.debug(s"try move result: ${msg.result}")
          render ! msg
        case msg: NotifyTimeStep =>
          render ! msg
        case _ => g.console.debug(s"Received some unknown message from server: $msg")
      }
    }

    override def receive: Receive = {
      case x:TryMove =>
        socket.send(Message.serialize(x))
        g.console.debug(s"sent try move message ${x.delta}")
      case _ => //sender()
    }
  }

  object InputController {
    def props(canvas: html.Canvas, username: String): Props = Props(new InputController(canvas, username))
  }

  class InputController(canvas: html.Canvas, username: String) extends Actor {
    var activeUser: String = ""
    val socket = context.actorSelection(s"/user/socket-$username")

    override def receive: Receive = {
      case msg: NotifyActiveUser =>
        activeUser = msg.activeUser
        g.console.debug(s"Got active player: $activeUser")
      case _ =>
    }

    document.onkeydown = (e: dom.KeyboardEvent) => {
      g.console.debug("user pressed some key")
      e.key match {
        case dom.ext.KeyValue.ArrowDown => socket ! TryMove((0, 1))
        case dom.ext.KeyValue.ArrowLeft => socket ! TryMove((-1, 0))
        case dom.ext.KeyValue.ArrowUp => socket ! TryMove((0, -1))
        case dom.ext.KeyValue.ArrowRight => socket ! TryMove((1, 0))
        case _ => g.console.debug(s"pressed key ${e.key} and arrow is ${dom.ext.KeyValue.ArrowDown}")
      }
    }
  }

  @JSExport
  def main(): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val username: String = dom.document.cookie
    val system = ActorSystem(s"someSystem-$username")
    val render = system.actorOf(Render.props(ctx, username), s"render-$username")
    val socket = system.actorOf(SocketActor.props(username), s"socket-$username")
    val inputController = system.actorOf(InputController.props(canvas, username), s"input-$username")
    var down = false

    dom.window.setInterval(() => render ! Render.RedrawMessage, 1000)
  }
}