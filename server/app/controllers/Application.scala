package controllers

import java.util.Calendar

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.cluster.pubsub.{DistributedPubSub, DistributedPubSubMediator}
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import com.google.inject.Inject
import models.GameManager.NewUser
import models.{GameManager, LoginForm}
import play.api.data.Form
import play.api.mvc._
import shared._
import play.api.Play._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.data._
import play.api.data.Forms._
import play.Logger
import play.api.i18n.MessagesApi
import play.api.libs.streams.ActorFlow
import play.api.libs.json._

import scala.language.postfixOps

case class User(name: String)

class Application @Inject()(val messagesApi: MessagesApi,  materializer: Materializer) extends Controller {
  import Application._
  import scala.collection.mutable.{Map => MutableMap}
  val userToSocket = MutableMap.empty[String, ActorRef]
  implicit val timeout = Timeout(20 second)

  def game = Action {
    Ok(views.html.game(SharedMessages.itWorks)).withNewSession
  }


  def login = Action { implicit request =>
    implicit val messages = messagesApi.preferred(request)
    Ok(views.html.login(LoginForm.form))
  }

  def loginSubmit = Action.async { implicit request =>
    implicit val messages = messagesApi.preferred(request)
    Logger.info("loginSubmit")
    LoginForm.form.bindFromRequest.fold(
      errors => {
        Logger.info("Errors in form")
        Future.successful(
          Ok(views.html.login(errors)))
      },

      loginForm => {
        Logger.info("Form is fine")
        Logger.debug(s"User=${loginForm.username}")

        gameManager ? NewUser(loginForm.username) map {
          case true => Ok(views.html.game(loginForm.username))
          case false => Ok(views.html.login(LoginForm.form))
        }
      })
  }

  def socket = WebSocket.accept[JsValue, JsValue] { request =>
    val name = request.queryString.get("name").flatMap(_.headOption)

    name match {
      case Some(x) =>
        Logger.debug(s"name got from socket is $x")
        ActorFlow.actorRef(out => MyWebSocketActor.props(out, x))(system, materializer)
      case _ => throw new Exception("Expected user name")
    }

  }

  object MyWebSocketActor {
    def props(out: ActorRef, user: String) = Props(new MyWebSocketActor(out, user))
  }

  class MyWebSocketActor(out: ActorRef, name: String) extends Actor {
    import DistributedPubSubMediator.{ Subscribe, SubscribeAck }
    val mediator = DistributedPubSub(context.system).mediator
    mediator ! Subscribe("websocket_messages", self)

    def receive = {
      case request: JsValue =>
        val response = handleMessage(Json.stringify(request))
        out ! response
      case SubscribeAck(Subscribe("websocket_messages", None, `self`)) ⇒
        Logger.info("subscribing");
    }

    def handleMessage(msg: ClientMessage): String = {
      lazy val responseTimestamp = Calendar.getInstance().getTime.getTime
      msg match {
        case msg: RoomStatMessage => msg // just pass it
        case _ => ErrorMessage(responseTimestamp, "Unsupported message type")
      }
    }
  }


}

object Application {
  implicit val system = ActorSystem("mySystem")
  val gameManager = system.actorOf(Props[GameManager], "game_manager")
}