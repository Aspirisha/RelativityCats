package shared

import java.util.Calendar

object SharedMessages {
  def itWorks = "It works!"
}

object LoginResponseMessage {
 // implicit val format = Json.format[LoginResponseMessage]

  val resultOk = "OK"
  val alreadyLoggedIn = "Already logged in"
}

object ErrorMessageType {
  def ERR_UNKNOWN_MSG = "Unknow message type"
}

case class RoomState(users: List[String])

sealed trait Message
sealed trait ClientMessage extends Message
sealed trait ServerMessage extends Message
// server to client messages
case class RoomStatMessage(data: RoomState, messageType: String = "room_stat") extends ServerMessage

//client to server messages
case class ClientErrorMessage(data: String, messageType: String = "error") extends ClientMessage
case class ServerErrorMessage(data: String, messageType: String = "error") extends ServerMessage

object Message {
  implicit def deserialize(jsValue: String): Message = {
    upickle.default.read[Message](jsValue)
  }

  implicit def serialize(msg: Message): String = {
    upickle.default.write(msg)
  }
}