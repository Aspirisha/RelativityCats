package models

/**
  * Created by andy on 12/15/16.
  */

import play.api.data.Form
import play.api.data.Forms._

/**
  * @author adrian
  */
object LoginForm {

  case class LoginData(username: String)

  val form = Form(
    mapping("username" -> nonEmptyText) (LoginData.apply) (LoginData.unapply)
  )

}