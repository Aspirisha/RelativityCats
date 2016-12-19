package controllers

import java.util.Calendar

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import com.google.inject.Inject
import models.GameManager.NewUser
import models.{GameManager, LoginForm}
import play.api.data.Form
import play.api.mvc._
import shared._

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
    name foreach {x => Logger.debug(s"name got from socket is $name")}
    ActorFlow.actorRef(out => MyWebSocketActor.props(out, name))(system, materializer)
  }

  object MyWebSocketActor {
    def props(out: ActorRef, name: Option[String]) = Props(new MyWebSocketActor(out, name))
  }

  class MyWebSocketActor(out: ActorRef, name: Option[String]) extends Actor {
    var user: Option[String] = None

    def receive = {
      case request: JsValue =>
        val response = handleMessage(request)
        out ! response
    }

    def isAuthenticated = user.isDefined

    def handleMessage(msg: ClientMessage): JsValue = {
      lazy val responseTimestamp = Calendar.getInstance().getTime.getTime
      msg match {
        case msg: LoginMessage if !isAuthenticated => handleLoginMessage(msg)
        case msg: ClientMessage if !isAuthenticated => ErrorMessage(responseTimestamp, "Client not logged in")
        case msg: MessageA => handleMessageA(msg)
        case msg: MessageB => handleMessageB(msg)
        case _ => ErrorMessage(responseTimestamp, "Unsupported message type")
      }
    }

    def handleLoginMessage(msg: LoginMessage): JsValue = {
      user = Some(msg.login)

      LoginResponseMessage(Calendar.getInstance().getTime.getTime, LoginResponseMessage.resultOk)
    }

    def handleMessageA(msg: MessageA): JsValue = {
      // Message handling…
      MessageA(Calendar.getInstance().getTime.getTime, "some data A")
    }
    def handleMessageB(msg: MessageB): JsValue = {
      // Message handling…
      MessageB(Calendar.getInstance().getTime.getTime, "some data B")
    }
  }


}

object Application {
  implicit val system = ActorSystem("mySystem")
  val gameManager = system.actorOf(Props[GameManager], "game_manager")
}