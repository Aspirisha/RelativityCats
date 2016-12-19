package models

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.persistence.PersistentActor
import akka.util.Timeout
import com.google.common.html.HtmlEscapers
import models.GameManager.NewUser
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee._
import play.api.libs.json._
import play.{Logger}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import play.api.Play.current

/**
  * Stores state of the room
  */
trait RoomState {

  def get(username: String): Option[User]

  def list(): List[User]

  def save(user: User): Boolean

  def remove(username: String): Boolean

  def existingRoles(): Set[UserRole]
}

object RoomStateImpl {
  val possibleRoles : Set[UserRole] = Set(Cat, Mouse)
}

class RoomStateImpl extends RoomState {
  import RoomStateImpl._
  import scala.collection.mutable.{Map=>MutableMap}

  val users = MutableMap.empty[String, UserRole] //Map of username, avatar
  val roles = {
    MutableMap(possibleRoles map (t => t -> 0) toList: _*)
  }

  override def get(username: String): Option[User] = {
    users.get(username).map{User(username, _)}
  }

  override def list(): List[User] = {
    users.toList.map{e => User(e._1, e._2)}
  }

  override def save(user: User): Boolean = {
    users.put(user.username, user.role) match {
      case Some(role: UserRole) =>
        roles.put(role, roles.getOrElse(role, 0) + 1)
        true
      case None => false
    }
  }

  override def remove(username: String): Boolean = {
    users.remove(username).exists { s => true }
  }

  override def existingRoles(): Set[UserRole] = {
    users.map{case (x:String,y:UserRole) => y}.toSet
  }
}

class GameManagerState {
  import scala.collection.mutable.{Map=>MutableMap, Set=>MutableSet}
  val rooms = MutableMap[Int,ActorRef]()
  val users = MutableSet[String]()
}

object GameManager {
  case class NewUser(username: String)
}

class GameManager extends PersistentActor {
  import scala.collection.mutable.{Map=>MutableMap}

  implicit val timeout = Timeout(1 second)
  val state = new GameManagerState

  @tailrec
  final def createRoom(name: String): (Int, ActorRef) = {
    val id = new Random().nextInt
    if (state.rooms.contains(id)) createRoom(name) else {
      val room = context.actorOf(Props(new GameRoom(name, id)))
      state.rooms.put(id, room)
      (id, room)
    }
  }

  def join(username: String, roomId: Int): Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    state.rooms.get(roomId) match {
      case Some(room) => (room ? Join(username)).map {

        case Connected(user, enumerator) =>
          // Create an Iteratee to consume the feed
          val iteratee = Iteratee.foreach[JsValue] { event =>
            println(event)
          }.map { _ =>
            room ! Quit(user)
          }
          (iteratee, enumerator)

        case CannotConnect(error) =>
          // Connection error
          // A finished Iteratee sending EOF
          val iteratee = Done[JsValue, Unit]((), Input.EOF)
          // Send an error and close the socket
          val enumerator = Enumerator[JsValue](JsObject(Seq("error" -> JsString(error))))
            .andThen(Enumerator.enumInput(Input.EOF))
          (iteratee, enumerator)
      }
    }
  }


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

  class GameRoom(name: String, id: Int) extends PersistentActor {
    import GameRoom._

    val state = new RoomStateImpl
    val (chatEnumerator, chatChannel) = Concurrent.broadcast[JsValue]

    def randomRole(): UserRole = {
      RoomStateImpl.possibleRoles.toList(rand.nextInt(RoomStateImpl.possibleRoles.size))
    }

    def almostRandomRole(): UserRole = {
      (RoomStateImpl.possibleRoles -- state.existingRoles()).find(x => true) match {
        case Some(role) => role // there are some roles that are not present in room
        case None => randomRole()
      }
    }

    override def receive = {

      case Join(username) => {
        state.get(username).map{ user =>
          println(state.list())
          sender ! CannotConnect("This username is already used")
        }.getOrElse{
          val user = User(username, randomRole())
          state.save(user)
          sender ! Connected(user, chatEnumerator)
          self ! NotifyJoin(user)
        }
      }

      case NotifyJoin(user) =>
        notifyAll("join", user, "has entered the room")

      case Quit(user) =>
        state.remove(user.username)
        notifyAll("quit", user, "has left the room")

      case Talk(user, text) =>
        println(s"$user says: $text")

    }

    def notifyAll(kind: String, user: User, text: String) {
      def userToJson(user: User) = Json.obj("name"->user.username, "role"->user.role.name)
      val msg = JsObject(
        Seq(
          "kind" -> JsString(kind),
          "user" -> userToJson(user),
          "message" -> JsString(text),
          "members" -> JsArray(state.list.map(userToJson(_)))
        )
      )
      chatChannel.push(msg)
    }

    override def receiveRecover: Receive = {
      case x: Any => Logger.debug("receiveRecover")
    }

    override def receiveCommand: Receive = {
      case x: Any => Logger.debug("receiveCommand")
    }

    override def persistenceId: String = s"Room$id"
  }

  object GameRoom {
    val rand = new Random()
  }

  override def receiveRecover: Receive = {
    case x: Any => Logger.debug("GameManager: receiveRecover")
  }

  override def receiveCommand: Receive = {
    case x: Any => Logger.debug("GameManager: receiveCommand")
  }

  override def persistenceId: String = "1"
}



case object Cat extends UserRole { val name = "Cat" }
case object Mouse extends UserRole { val name = "Mouse" }

sealed trait UserRole { def name: String }
case class User(username: String, role: UserRole)

case class Join(username: String)
case class Quit(user: User)
case class Talk(user: User, text: String)
case class NotifyJoin(user: User)

case class Connected(user: User, enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)