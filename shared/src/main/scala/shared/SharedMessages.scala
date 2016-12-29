package shared

import java.util.Calendar

import com.uniformlyrandom.jello.JelloValue.{JelloObject, JelloString}
import com.uniformlyrandom.jello.{JelloFormat, JelloJson, JelloValue}
import shared.models.{Maze, Vector2}

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

object ServerMessage {
  implicit val fmt: JelloFormat[RoomStatMessage] = JelloFormat.format[RoomStatMessage]
}

// server to client messages
case class RoomStatMessage(data: RoomState, messageType: String = "room_stat") extends ServerMessage

case class NotifyGameStart(data: models.MazeView, messageType: String = "start_game") extends ServerMessage
object NotifyGameStart {
  implicit val fmt: JelloFormat[NotifyGameStart] = JelloFormat.format[NotifyGameStart]
}

case class TryMoveResult(result: Maze.Event, delta: Vector2, messageType: String = "try_move_result") extends ServerMessage
object TryMoveResult {
  implicit val fmt: JelloFormat[TryMoveResult] = JelloFormat.format[TryMoveResult]
}
case class StartNextTurn(activeUser: String, messageType: String = "start_turn") extends ServerMessage
object StartNextTurn {
  implicit val fmt: JelloFormat[StartNextTurn] = JelloFormat.format[StartNextTurn]
}

case class ServerErrorMessage(data: String, messageType: String = "error") extends ServerMessage

//client to server messages

case class TryMove(delta: Vector2, messageType: String = "try_move") extends ClientMessage
object TryMove {
  implicit val fmt: JelloFormat[TryMove] = JelloFormat.format[TryMove]
}
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
          case "try_move" => JelloJson.fromJson[TryMove](ajJV).get
          case "try_move_result" => JelloJson.fromJson[TryMoveResult](ajJV).get
        }
    }
  }

  implicit def serialize(msg: Message): String = {
    //upickle.default.write(msg)
    val j = {
      msg match { // this is crap, I don't know wtd
        case x:RoomStatMessage => JelloJson.toJson[RoomStatMessage](x)
        case x:NotifyGameStart => JelloJson.toJson[NotifyGameStart](x)
        case x:TryMove => JelloJson.toJson[TryMove](x)
        case x:TryMoveResult => JelloJson.toJson[TryMoveResult](x)
      }

    }
    JelloJson.toJsonString(j)
  }
}