package example

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLImageElement
import shared._
import shared.models.Maze.Cell
import shared.models.{MazeView, Vector2}

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
  }

  case class Sprite(imgname: String) {
    val image = {
      val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = Sprite.getImageUri(dom.document, imgname)
      image
    }

    def draw(pos: Vector2, ctx: dom.CanvasRenderingContext2D): Unit = {
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
      ctx.stroke()
    }
  }

  lazy val catSprite = Sprite("cat1.png")
  lazy val mouseSprite = Sprite("mouse1.png")
  lazy val unknownSprite = Sprite("unknown1.png")

  object Render {
    def props(ctx: dom.CanvasRenderingContext2D): Props = Props(new Render(ctx))

    case object RedrawMessage
  }

  class Render(ctx: dom.CanvasRenderingContext2D) extends Actor {
    var maze: Option[MazeView] = None
    g.console.debug(s"Render creation")
    def drawCell(pos: Vector2, cell: Cell): Unit = {
      cell match {
        case Cell.unknown => unknownSprite.draw(pos, ctx)
        case Cell.cat => catSprite.draw(pos, ctx)
        case Cell.mouse => mouseSprite.draw(pos, ctx)
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

      /*if (count % 3000 == 0) clear()
    count += 1
    p = (p + corners(Random.nextInt(3))) / 2

    val height = 512.0 / (255 + p.y)
    val r = (p.x * height).toInt
    val g = ((255 - p.x) * height).toInt
    val b = p.y
    ctx.fillStyle = s"rgb($g, $r, $b)"

    ctx.fillRect(p.x, p.y, 1, 1)*/
    }

    override def receive: Receive = {
      case Render.RedrawMessage =>
        redraw
      case msg: NotifyGameStart =>
        maze = Some(msg.data)
        g.console.debug(s"Got the maze ${maze}")
        cellSize = Vector2(canvas.width / msg.data.size, canvas.height / msg.data.size)
        redraw
    }
  }

  object SocketActor {
    def props(name: String): Props = Props(new SocketActor(name))
  }


  class SocketActor(name: String) extends Actor {

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
          context.actorSelection(s"/user/render-$name") ! msg
          g.console.debug(s"received game start ${msg.data}")
      }
    }

    override def receive: Receive = {
      case _ => //sender()
    }
  }

  @JSExport
  def main(): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val username: String = dom.document.getElementById("scalajsShoutOut").textContent
    val system = ActorSystem(s"someSystem-$username")
    val render = system.actorOf(Render.props(ctx), s"render-$username")
    val socket = system.actorOf(SocketActor.props(username), s"socket-$username")
    var down = false
    var count = 0
    var p = Point(0, 0)
    val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))

    g.console.log(s"name was: ${dom.document.getElementById("scalajsShoutOut").textContent}")
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
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

    dom.window.setInterval(() => render ! Render.RedrawMessage, 50)
  }


}