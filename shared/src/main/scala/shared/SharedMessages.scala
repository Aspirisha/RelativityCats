package shared

import java.util.Calendar

object SharedMessages {
  def itWorks = "It works!"
}

abstract class MessageObjectTypeAware(val MSG_TYPE: String)


object RoomStatMessage extends MessageObjectTypeAware("message_room_statistics") {
  //implicit val format = Json.format[RoomStatMessage]
}
object MessageB extends MessageObjectTypeAware("message_b") {
 // implicit val format = Json.format[MessageB]
}
object ErrorMessage extends MessageObjectTypeAware("ERROR") {
 // implicit val format = Json.format[ErrorMessage]
}
object LoginMessage extends MessageObjectTypeAware("message_login") {
 // implicit val format = Json.format[LoginMessage]
}

object LoginResponseMessage extends MessageObjectTypeAware("message_login_reponse") {
 // implicit val format = Json.format[LoginResponseMessage]

  val resultOk = "OK"
  val alreadyLoggedIn = "Already logged in"
}

object ErrorMessageType {
  def ERR_UNKNOWN_MSG = "Unknow message type"
}

case class RoomState(users: List[String])

sealed trait ClientMessage
case class RoomStatMessage(timestamp: Long, data: RoomState, messageType: String = RoomStatMessage.MSG_TYPE) extends ClientMessage
case class LoginMessage(timestamp: Long, login: String, messageType: String = LoginMessage.MSG_TYPE) extends ClientMessage
case class LoginResponseMessage(timestamp: Long, result: String, messageType: String = LoginResponseMessage.MSG_TYPE) extends ClientMessage
case class MessageB(timestamp: Long, data: String, messageType: String = MessageB.MSG_TYPE) extends ClientMessage
case class ErrorMessage(timestamp: Long, data: String, messageType: String = ErrorMessage.MSG_TYPE) extends ClientMessage

object ClientMessage {
  implicit def jsValue2ClientMessage(jsValue: String): ClientMessage = {
    upickle.default.read[ClientMessage](jsValue)
  }

  implicit def clientMessage2jsValue(clientMessage: ClientMessage): String = {
    upickle.default.write(clientMessage)
  }

  def clientSideDeserializer(jsString: String) = {
    upickle.default.read[ClientMessage](jsString)
  }

}