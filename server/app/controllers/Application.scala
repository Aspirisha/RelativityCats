package controllers

import java.util.Calendar

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.cluster.pubsub.{DistributedPubSub, DistributedPubSubMediator}
import akka.pattern.ask
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.util.Timeout
import com.google.inject.Inject
import models.GameRoom.{NewUser, ValidateLoggedIn}
import models.{GameRoom, LoginForm}
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
import shared.Message._

case class User(name: String)

class Application @Inject()(val messagesApi: MessagesApi,  materializer: Materializer) extends Controller {
  import Application._
  import scala.collection.mutable.{Map => MutableMap}

  implicit val timeout = Timeout(20 second)

  def game(name: String) = Action.async {
    gameManager ? ValidateLoggedIn(name) map {
      case true => Ok(views.html.game(name))
      case false => Redirect(routes.Application.login())
    }
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
          case true => Redirect(routes.Application.game(loginForm.username))
          case false => Redirect(routes.Application.login())
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
    gameManager ! (name -> self)

    def receive = {
      case request: JsValue =>
        deserialize(Json.stringify(request)) match {
          case x: ClientMessage => out ! handleMessage(x)
        }
      case x: ServerMessage =>
        Logger.debug(s"ServerMessage: $x")
        out ! Json.parse(serialize(x))
      case x: TryMove =>
        gameManager ! (x, name)
    }

    def handleMessage(msg: ClientMessage): JsValue = {
      msg match {
        case _ => Json.parse(ClientErrorMessage("Unsupported message type"))
      }
    }
  }


}

object Application {
  implicit val system = ActorSystem("mySystem")
  val gameManager = system.actorOf(Props[GameRoom], "game_manager")
}