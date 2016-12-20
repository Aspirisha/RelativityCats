package example

import be.doeraene.spickling.jsany._
import akka.actor.{ActorRef, ActorSystem}
import be.doeraene.spickling.PicklerRegistry
import org.scalajs.dom
import org.scalajs.dom._
import play.api.libs.json.{JsValue, Json}
import shared.{ClientMessage, Message, RoomStatMessage, SharedMessages}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Random
import js.Dynamic.{global => g}
case class Point(x: Int, y: Int){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Int) = Point(x / d, y / d)
}


@JSExport
object RoomFrontend extends js.JSApp {
  var username: Option[String] = None

  @JSExport
  def setName(name: String): Unit = {
    username = Some(name)
    g.console.debug(s"Got user name: $name")
    joinGame(name)
  }

  @JSExport
  def main(): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
    g.console.log(s"elements: ${dom.document.getElementsByName("div")}")

    val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight
    ctx.fillStyle = "#f8f8f8"
    ctx.fillRect(0, 0, canvas.width, canvas.height)

    /*code*/
    ctx.fillStyle = "black"
    var down = false
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

    var count = 0
    var p = Point(0, 0)
    val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, 255, 255)
    }

    def run = for (i <- 0 until 10) {
      if (count % 3000 == 0) clear()
      count += 1
      p = (p + corners(Random.nextInt(3))) / 2

      val height = 512.0 / (255 + p.y)
      val r = (p.x * height).toInt
      val g = ((255 - p.x) * height).toInt
      val b = p.y
      ctx.fillStyle = s"rgb($g, $r, $b)"

      ctx.fillRect(p.x, p.y, 1, 1)
    }

    dom.window.setInterval(() => run, 50)
  }

  def joinGame(name: String): Unit = {
    def getWebsocketUri(document: Document, nameOfChatParticipant: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

      g.console.debug(s"host is ${dom.document.location.host}")
      s"$wsProtocol://${dom.document.location.host}/game/socket?name=$nameOfChatParticipant"
    }

    val chat = new WebSocket(getWebsocketUri(dom.document, name))

    chat.onmessage = { (event: MessageEvent) ⇒
      val msg = Message.deserialize(event.data.toString)
      g.console.debug(s"received room state ${msg}")
      msg match {
        case msg:RoomStatMessage => g.console.debug(s"received room state ${msg.data}")
      }

    }


  }
}