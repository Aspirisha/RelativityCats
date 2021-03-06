package models

import akka.actor.{Actor, ActorRef}
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

class GameRoom extends Actor {
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

      if (userToRef.size == usersToStartGame) {
        if (!isGameStarted)
          startGame()
        else {
          val userActor = userToRef(x)
          userActor ! NotifyActiveUser(players(currentPlayer).username)
          val res = (players zip playerViews).filter { case (char,view) => char.username == x }
          for ((p, v) <- res)
            userActor ! UserView(v)
        }

      }
    case (msg: TryMove, username: String) =>
      Logger.debug("Received try move message")
      if (players(currentPlayer).username != username) {
        Logger.debug("user is not active")
        sender ! TryMoveResult(Maze.Event.invalidStep, msg.delta)
      } else {
        val event = world.renderCharacterStep(playerViews(currentPlayer), msg.delta)
        sender ! TryMoveResult(event, msg.delta)
        event match {
          case Maze.Event.justStep =>
            currentPlayer += 1
            if (currentPlayer == players.size) {
              currentPlayer = 0
              world.renderTimeStep(playerViews)
              players zip playerViews foreach {
                case (p, v) => userToRef(p.username) ! NotifyTimeStep(v)
              }
            }
            players foreach {
              case p => userToRef(p.username) ! NotifyActiveUser(players(currentPlayer).username)
            }
          case _ => Logger.debug("Unimplemented")
        }
      }
    case _ =>
  }

  def startGame() = {
    isGameStarted = true
    Logger.debug(s"players are ${players}")
    Logger.debug(s"playerviews are ${playerViews}")
    world.renderTimeStep(playerViews)
    players zip playerViews foreach {
      case (p, v) =>
        val ref = userToRef(p.username)
        ref ! NotifyGameStart(players)
        ref ! UserView(v)
        ref ! NotifyActiveUser(players(currentPlayer).username)
    }
  }
}
