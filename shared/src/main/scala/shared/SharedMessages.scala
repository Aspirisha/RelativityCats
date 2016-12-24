package shared

import java.util.Calendar

import com.uniformlyrandom.jello.JelloValue.{JelloObject, JelloString}
import com.uniformlyrandom.jello.{JelloFormat, JelloJson, JelloValue}

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
object RoomState {
  implicit val fmt: JelloFormat[RoomState] = JelloFormat.format[RoomState]
}

sealed trait Message
sealed trait ClientMessage extends Message
sealed trait ServerMessage extends Message
// server to client messages
case class RoomStatMessage(data: RoomState, messageType: String = "room_stat") extends ServerMessage
object RoomStatMessage {
  implicit val fmt: JelloFormat[RoomStatMessage] = JelloFormat.format[RoomStatMessage]
}

case class NotifyGameStart(data: models.MazeView, messageType: String = "start_game") extends ServerMessage
object NotifyGameStart {
  implicit val fmt: JelloFormat[NotifyGameStart] = JelloFormat.format[NotifyGameStart]
}

case class ServerErrorMessage(data: String, messageType: String = "error") extends ServerMessage

//client to server messages
case class ClientErrorMessage(data: String, messageType: String = "error") extends ClientMessage

object Message {
  implicit def deserialize(jsValue: String): Message = {
    //upickle.default.read[Message](jsValue)
    val ajJV: JelloValue = JelloJson.parse(jsValue)

    ajJV match {
      case x:JelloObject =>
        x.map("messageType").asInstanceOf[JelloString].v match {
          case "room_stat" =>  JelloJson.fromJson[RoomStatMessage](ajJV).get
          case "start_game" =>  JelloJson.fromJson[NotifyGameStart](ajJV).get
        }
    }
  }

  implicit def serialize(msg: Message): String = {
    //upickle.default.write(msg)
    val j = {
      msg match { // this is crap, I don't know wtd
        case x:RoomStatMessage => JelloJson.toJson[RoomStatMessage](x)
        case x:NotifyGameStart => JelloJson.toJson[NotifyGameStart](x)
      }

    }
    JelloJson.toJsonString(j)
  }
}