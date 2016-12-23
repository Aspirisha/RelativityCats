package models

import akka.actor.ActorRef
import akka.persistence.PersistentActor
import akka.util.Timeout
import play.Logger
import play.api.libs.iteratee._
import play.api.libs.json._
import shared.{ClientMessage, RoomStatMessage, RoomState}

import scala.concurrent.duration._
import scala.util.Random

object GameRoom {
  case class NewUser(username: String)
  case class ValidateLoggedIn(username: String)
  val rand = new Random()
  val usersToStartGame = 2 // this is very dummy for now
}

class GameRoom extends PersistentActor {
  import GameRoom._

  import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

  implicit val timeout = Timeout(1 second)

  val users = MutableSet.empty[String]
  val userToRef = MutableMap.empty[String, ActorRef]
  var isGameStarted = false

  lazy val players: Map[String, GameCharacter] = users.toList zip GameCharacter.genRoles(users.toList) toMap

  override def receive = {
    case NewUser(username) =>
      Logger.debug("Received new user message")
      if (!users.contains(username)) {
        users += username
        sender ! true
      } else
        sender ! false
    case ValidateLoggedIn(username) =>
      sender ! users.contains(username)
    case (x:String, y:ActorRef) =>
      Logger.debug("Received actorRef")
      userToRef.put(x, y)
      userToRef foreach {case (_, actor) =>
        Logger.debug("Sending broadcast")
        actor ! RoomStatMessage(RoomState(users.toList))
      }

      if (userToRef.size == usersToStartGame && !isGameStarted) {
        startGame()
      }
  }

  def startGame() = {
    isGameStarted = true
  }

  override def receiveRecover: Receive = {
    case x: Any => Logger.debug("GameManager: receiveRecover")
  }

  override def receiveCommand: Receive = {
    case x: Any => Logger.debug("GameManager: receiveCommand")
  }

  override def persistenceId: String = "1"
}



case class Join(username: String)
case class Quit(user: GameCharacter)
case class Talk(user: GameCharacter, text: String)
case class NotifyJoin(user: GameCharacter)

case class Connected(user: GameCharacter, enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)