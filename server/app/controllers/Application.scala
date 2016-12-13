package controllers

import play.api.mvc._
import shared.SharedMessages

class Application extends Controller {

  def index = Action {
    Ok(views.html.index(SharedMessages.itWorks))
  }

  def login = Action {
    Ok(views.html.login())
  }

}
