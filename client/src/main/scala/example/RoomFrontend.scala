package example

import akka.actor.{ActorRef, ActorSystem}
import org.scalajs.dom
import org.scalajs.dom.{Document, WebSocket, html}
import play.api.libs.json.{JsValue, Json}
import shared.{ClientMessage, MessageA, SharedMessages}

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

    def run = for (i <- 0 until 10){
      if (count % 3000 == 0) clear()
      count += 1
      p = (p + corners(Random.nextInt(3))) / 2

      val height = 512.0 / (255 + p.y)
      val r = (p.x * height).toInt
      val g = ((255-p.x) * height).toInt
      val b = p.y
      ctx.fillStyle = s"rgb($g, $r, $b)"

      ctx.fillRect(p.x, p.y, 1, 1)
    }

    dom.window.setInterval(() => run, 50)
  }

  def joinGame(name: String): Unit = {
    val chat = new WebSocket(getWebsocketUri(dom.document, name))
  }

  def getWebsocketUri(document: Document, nameOfChatParticipant: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

    g.console.debug(s"host is ${dom.document.location.host}")
    s"$wsProtocol://${dom.document.location.host}/game/socket?name=$nameOfChatParticipant"
  }
}

object Connection {
  import akka.{ Done, NotUsed }
  import akka.stream.ActorMaterializer
  import akka.stream.scaladsl._
  import akka.http.scaladsl.Http
  import akka.stream.ActorMaterializer
  import akka.stream.scaladsl._
  import akka.http.scaladsl.model._
  import akka.http.scaladsl.model.ws._

  import scala.concurrent.Future

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  // print each incoming strict text message
  val printSink: Sink[Message, Future[Done]] =
    Sink.foreach {
      case message: MessageA =>
        println(message.data)
    }

  val helloSource: Source[Message, NotUsed] =
    Source.single(TextMessage(Json.toJson(MessageA(0, "hello world!")).toString()))

  // the Future[Done] is the materialized value of Sink.foreach
  // and it is completed when the stream completes
  val flow: Flow[Message, Message, Future[Done]] =
    Flow.fromSinkAndSourceMat(printSink, helloSource)(Keep.left)

  // upgradeResponse is a Future[WebSocketUpgradeResponse] that
  // completes or fails when the connection succeeds or fails
  // and closed is a Future[Done] representing the stream completion from above
  val (upgradeResponse, closed) =
    Http().singleWebSocketRequest(WebSocketRequest("ws://echo.websocket.org"), flow)

  val connected = upgradeResponse.map { upgrade =>
    g.console.debug("Connected!")
    // just like a regular http request we can get 404 NotFound,
    // with a response body, that will be available from upgrade.response
    if (upgrade.response.status == StatusCodes.OK) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }

  // in a real application you would not side effect here
  // and handle errors more carefully
  connected.onComplete(println)
  closed.foreach(_ => println("closed"))
}