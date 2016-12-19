package models

import akka.persistence.PersistentActor
import akka.util.Timeout
import play.Logger
import play.api.libs.iteratee._
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.Random

/**
  * Stores state of the room
  */
trait RoomState {

  def getPlayer(username: String): Option[Player]

  def list(): List[String]

  def addUser(user: String): Boolean

  def remove(username: String): Boolean

  def existingRoles(): Set[PlayerRole]
}

object GameManagerState {
  val possibleRoles : Set[PlayerRole] = Set(Cat, Mouse)
}

class GameManagerState extends RoomState {
  import GameManagerState._

  import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
  val users = MutableSet.empty[String]
  val players = MutableMap.empty[String, Player]
  val roles = {
    MutableMap(possibleRoles map (t => t -> 0) toList: _*)
  }

  override def getPlayer(username: String): Option[Player] = {
    players.get(username)
  }

  override def list(): List[String] = {
    users.toList
  }

  override def remove(username: String): Boolean = {
    players.remove(username)
    users.remove(username)
  }

  override def existingRoles(): Set[PlayerRole] = {
    players.map{case (x:String, y:PlayerRole) => y}.toSet
  }

  override def addUser(username: String): Boolean = {
    users.add(username)
  }
}

object GameManager {
  case class NewUser(username: String)
  val rand = new Random()
}

class GameManager extends PersistentActor {
  import GameManager._

  import scala.collection.mutable.{Map => MutableMap}

  implicit val timeout = Timeout(1 second)
  val state = new GameManagerState

  override def receive = {
    case NewUser(username) =>
      Logger.debug("Received new user message")
      if (state.users.contains(username)) {
        sender ! false
      }
      else {
        state.users += username
        sender ! true
      }
  }

  def randomRole(): PlayerRole = {
    GameManagerState.possibleRoles.toList(rand.nextInt(GameManagerState.possibleRoles.size))
  }

  def almostRandomRole(): PlayerRole = {
    (GameManagerState.possibleRoles -- state.existingRoles()).find(x => true) match {
      case Some(role) => role // there are some roles that are not present in room
      case None => randomRole()
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



case object Cat extends PlayerRole { val name = "Cat" }
case object Mouse extends PlayerRole { val name = "Mouse" }

sealed trait PlayerRole { def name: String }
abstract class Player(username: String, role: PlayerRole) {
  var hp: Int
}



case class Join(username: String)
case class Quit(user: Player)
case class Talk(user: Player, text: String)
case class NotifyJoin(user: Player)

case class Connected(user: Player, enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)