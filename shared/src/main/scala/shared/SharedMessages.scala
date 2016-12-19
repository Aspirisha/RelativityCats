package shared

import java.util.Calendar

import play.api.libs.json.{JsValue, Json}

object SharedMessages {
  def itWorks = "It works!"
}

abstract class MessageObjectTypeAware(val MSG_TYPE: String)
object MessageA extends MessageObjectTypeAware("message_a") {
  implicit val format = Json.format[MessageA]
}
object MessageB extends MessageObjectTypeAware("message_b") {
  implicit val format = Json.format[MessageB]
}
object ErrorMessage extends MessageObjectTypeAware("ERROR") {
  implicit val format = Json.format[ErrorMessage]
}
object LoginMessage extends MessageObjectTypeAware("message_login") {
  implicit val format = Json.format[LoginMessage]
}

object LoginResponseMessage extends MessageObjectTypeAware("message_login_reponse") {
  implicit val format = Json.format[LoginResponseMessage]

  val resultOk = "OK"
  val alreadyLoggedIn = "Already logged in"
}

object ErrorMessageType {
  def ERR_UNKNOWN_MSG = "Unknow message type"
}

trait ClientMessage
case class MessageA(timestamp: Long, data: String, messageType: String = MessageA.MSG_TYPE) extends ClientMessage
case class LoginMessage(timestamp: Long, login: String, messageType: String = LoginMessage.MSG_TYPE) extends ClientMessage
case class LoginResponseMessage(timestamp: Long, result: String, messageType: String = LoginResponseMessage.MSG_TYPE) extends ClientMessage
case class MessageB(timestamp: Long, data: String, messageType: String = MessageB.MSG_TYPE) extends ClientMessage
case class ErrorMessage(timestamp: Long, data: String, messageType: String = ErrorMessage.MSG_TYPE) extends ClientMessage

object ClientMessage {

  implicit def jsValue2ClientMessage(jsValue: JsValue): ClientMessage = {
    (jsValue \ "messageType").as[String] match {
      case ErrorMessage.MSG_TYPE => jsValue.as[ErrorMessage]
      case MessageA.MSG_TYPE => jsValue.as[MessageA]
      case MessageB.MSG_TYPE => jsValue.as[MessageB]
      case LoginMessage.MSG_TYPE => jsValue.as[LoginMessage]
      case messageType => ErrorMessage(Calendar.getInstance().getTime.getTime, ErrorMessageType.ERR_UNKNOWN_MSG)
    }
  }

  implicit def clientMessage2jsValue(clientMessage: ClientMessage): JsValue = {
    clientMessage match {
      case error: ErrorMessage => Json.toJson(error)
      case msgA: MessageA => Json.toJson(msgA)
      case msgB: MessageB => Json.toJson(msgB)
    }
  }

}