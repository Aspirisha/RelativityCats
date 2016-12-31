package example

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
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
      ctx.rect(pos.x * cellSize.x, pos.y * cellSize.y,
        (pos.x + 1) * cellSize.x - 1, (pos.y + 1) * cellSize.y - 1)
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
    g.console.debug(s"Render creation")
    def drawCell(pos: Vector2, cell: Cell): Unit = {
      cell match {
        case Cell.unknown =>
          g.console.debug("Drawing unknown")
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
        maze = Some(msg.data)
        players = msg.players
        g.console.debug(s"Got the maze ${maze}")
        cellSize = Vector2(canvas.width / msg.data.size, canvas.height / msg.data.size)
        redraw
      case msg: NotifyActiveUser =>
        currentPlayer = msg.activeUser
      case TryMoveResult(event, delta, _) =>
        maze match {
          case None =>
          case Some(m) =>
            event match {
              case Maze.Event.justStep =>
                m.position += delta
                redraw
              case _ => g.console.debug("Implement me!")
            }
        }
    }
  }

  object SocketActor {
    def props(name: String): Props = Props(new SocketActor(name))
  }

  class SocketActor(name: String) extends Actor {
    var players = List.empty[GameCharacter]
    val render = context.actorSelection(s"/user/render-$name")
    val inputController = context.actorSelection(s"/user/input-$name")

    def getWebsocketUri(document: Document, nameOfChatParticipant: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

      g.console.debug(s"host is ${dom.document.location.host}")
      s"$wsProtocol://${dom.document.location.host}/game/socket?name=$nameOfChatParticipant"
    }

    val chat = new WebSocket(getWebsocketUri(dom.document, name))

    chat.onmessage = { (event: MessageEvent) =>
      g.console.debug(s"received message from server ${event.data.toString}")
      val msg = Message.deserialize(event.data.toString)
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
          g.console.debug(s"received room state ${msg.data}")
        case msg: NotifyGameStart =>
          players = msg.players
          render ! msg
          g.console.debug(s"received game start ${msg.data}")
        case msg: NotifyActiveUser =>
          inputController ! msg
          render ! msg
        case msg: TryMoveResult =>
          g.console.debug(s"received try move result from server: ${msg.result}")
          render ! msg
        case _ => g.console.debug(s"Received some message from server: $msg")
      }
    }

    override def receive: Receive = {
      case x:TryMove =>
        chat.send(Message.serialize(x))
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
        case dom.ext.KeyValue.ArrowDown => socket ! TryMove((0, -1))
        case dom.ext.KeyValue.ArrowLeft => socket ! TryMove((-1, 0))
        case dom.ext.KeyValue.ArrowUp => socket ! TryMove((0, 1))
        case dom.ext.KeyValue.ArrowRight => socket ! TryMove((1, 0))
        case _ => g.console.debug(s"pressed key ${e.key} and arrow is ${dom.ext.KeyValue.ArrowDown}")
      }
    }
  }

  @JSExport
  def main(): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val username: String = dom.document.getElementById("scalajsShoutOut").textContent
    val system = ActorSystem(s"someSystem-$username")
    val render = system.actorOf(Render.props(ctx, username), s"render-$username")
    val socket = system.actorOf(SocketActor.props(username), s"socket-$username")
    val inputController = system.actorOf(InputController.props(canvas, username), s"input-$username")
    var down = false

    g.console.log(s"name was: ${dom.document.getElementById("scalajsShoutOut").textContent}")
    dom.document.getElementById("scalajsShoutOut").textContent = "dddd"
    g.console.log(s"elements: ${dom.document.getElementsByName("div")}")


    //canvas.width = canvas.parentElement.clientWidth
    //canvas.height = canvas.parentElement.clientHeight
    ctx.fillStyle = "#f8f8f8"
    ctx.fillRect(0, 0, canvas.width, canvas.height)

    /*code*/
    ctx.fillStyle = "black"

    canvas.onmousedown =
      (e: dom.MouseEvent) => down = true

    canvas.onmouseup =
      (e: dom.MouseEvent) => down = false

    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        val rect =
          canvas.getBoundingClientRect()
        if (down) ctx.fillRect(
          e.clientX - rect.left,
          e.clientY - rect.top,
          10, 10
        )
    }

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, canvas.width, canvas.height)
    }

    g.console.debug("HERE!")
    dom.window.setInterval(() => render ! Render.RedrawMessage, 1000)
  }


}