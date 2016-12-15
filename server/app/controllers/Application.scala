package controllers

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.Inject
import models.{GameManager, LoginForm}
import play.api.data.Form
import play.api.mvc._
import shared.SharedMessages

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.data._
import play.api.data.Forms._
import play.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json.JsObject
import play.mvc.BodyParser.Json

import scala.language.postfixOps

case class User(name: String)

class Application @Inject()(val messagesApi: MessagesApi) extends Controller {
  import Application._

  implicit val timeout = Timeout(1 second)

  def index = Action {
    Ok(views.html.index(SharedMessages.itWorks)).withNewSession
  }

  def login = Action { implicit request =>
    implicit val messages = messagesApi.preferred(request)
    Ok(views.html.login(LoginForm.form))
  }

  def welcome = Action.async { implicit request =>
    request.session.get("user") match {
      case Some(user) => (gameManager ? GameManager.NewUser(user)).map(f => Ok(views.html.welcome(user)))
      case None => Future[Result](Redirect(routes.Application.index()))
    }
  }

  def showLoginPage = Action { request =>
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
        val tempUser = loginForm.username // TODO
        Logger.debug(s"User=$tempUser")
        Future.successful(Ok(views.html.index("blah")))
      })
  }

}

object Application {
  val system = ActorSystem("mySystem")
  val gameManager = system.actorOf(Props[GameManager], "Game Manager")
}