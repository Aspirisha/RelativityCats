package models

import akka.actor.ActorRef
import akka.persistence.PersistentActor
import akka.util.Timeout
import play.Logger
import play.api.libs.iteratee._
import play.api.libs.json._
import shared.models.{GameCharacter, Maze}
import shared._

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

  val worldSize = 20
  val users = MutableSet.empty[String]
  val userToRef = MutableMap.empty[String, ActorRef]
  var isGameStarted = false
  lazy val world: Maze = Maze(worldSize, users.size)
  lazy val players: List[GameCharacter] = Random.shuffle(GameCharacter.genRoles(users.toList))
  lazy val playerViews = world.addCharacters(players)
  var currentPlayer = 0

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
    case (msg: TryMove, username: String) =>
      if (players(currentPlayer).username != username)
        sender ! TryMoveResult(Maze.Event.invalidStep, msg.delta)
      else {
        val event = world.renderCharacterStep(playerViews(currentPlayer), msg.delta)
        sender ! TryMoveResult(event, msg.delta)
      }
    case _ =>
  }

  def startGame() = {
    isGameStarted = true
    Logger.debug(s"players are ${players}")
    Logger.debug(s"playerviews are ${playerViews}")
    world.renderTimeStep(playerViews)
    players zip playerViews foreach {
      case (p, v) => userToRef(p.username) ! NotifyGameStart(v)
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
