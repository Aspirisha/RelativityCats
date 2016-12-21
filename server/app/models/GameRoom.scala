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
  val possibleRoles : Set[PlayerRole] = Set(Cat, Mouse)
}

class GameRoom extends PersistentActor {
  import GameRoom._

  import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

  implicit val timeout = Timeout(1 second)

  val users = MutableSet.empty[String]
  val roles = {
    MutableMap(possibleRoles map (t => t -> 0) toList: _*)
  }
  val userToRef = MutableMap.empty[String, ActorRef]
  var isGameStarted = false

  lazy val players: Map[String, Player] = users map {case x => x -> randomRole()(x)} toMap

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

  def existingRoles(): Set[PlayerRole] = {
    players.map{case (x:String, y:PlayerRole) => y}.toSet
  }

  def startGame() = {
    isGameStarted = true
  }

  def randomRole(): PlayerRole = {
    possibleRoles.toList(rand.nextInt(possibleRoles.size))
  }

  def almostRandomRole: PlayerRole = {
    val tentativeRole = randomRole()
    val freeRoles = possibleRoles -- existingRoles()
    if (freeRoles.contains(tentativeRole)) {
      tentativeRole
    } else {
      freeRoles.toList(rand.nextInt(freeRoles.size))
    }
  }

  override def receiveRecover: Receive = {
    case x: Any => Logger.debug("GameManager: receiveRecover")
  }

  override def receiveCommand: Receive = {
    case x: Any => Logger.debug("GameManager: receiveCommand")
  }

  override def persistenceId: String = "1"
}

sealed trait PlayerRole {
  val name: String
  def apply(username: String): Player
}

case object Cat extends PlayerRole {
  val name = "Cat"
  def apply(username: String): Player = CatPlayer(username)
}
case object Mouse extends PlayerRole {
  val name = "Mouse"
  def apply(username: String): Player = MousePlayer(username)
}

abstract class Player(username: String) {
  var hp: Int
  val role: PlayerRole
}

case class MousePlayer(username: String) extends Player(username) {
  override var hp: Int = 1
  override val role: PlayerRole = Mouse
}

case class CatPlayer(username: String) extends Player(username) {
  override var hp: Int = 9
  override val role: PlayerRole = Cat
}



case class Join(username: String)
case class Quit(user: Player)
case class Talk(user: Player, text: String)
case class NotifyJoin(user: Player)

case class Connected(user: Player, enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)