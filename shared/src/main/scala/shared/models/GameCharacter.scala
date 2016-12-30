package shared.models

import com.uniformlyrandom.jello.JelloValue.{JelloNumber, JelloObject, JelloString}
import com.uniformlyrandom.jello.{JelloReader, JelloValue, JelloWriter}

import scala.util.{Random, Try}

/**
  * Created by andy on 12/23/16.
  */

abstract sealed class GameCharacter {
  var hp: Int
  val username: String

  def charString: String
}

object GameCharacter {
  def genRoles(usernames: List[String]): List[GameCharacter] = {
    usernames match {
      case l if l.size < 2 => throw new Exception("Expect at least two cahracters")
      case x :: y :: tail => Random.shuffle(List(Mouse(x), Cat(y)) ++ tail.map(generateRole))
    }
  }

  private def generateRole(username: String): GameCharacter = {
    Random.nextInt(Mouse.probability + Cat.probability) match {
      case i if i >= 0 && i < Mouse.probability => Mouse(username)
      case _ => Cat(username)
    }
  }

  implicit val reader: JelloReader[GameCharacter] = {
    new JelloReader[GameCharacter] {
      override def read(jelloValue: JelloValue): Try[GameCharacter] = {
        jelloValue match {
          case p: JelloObject =>
            for {hp <- Try(p.map("hp").asInstanceOf[JelloNumber].v.toInt)
                 name <- Try(p.map("name").asInstanceOf[JelloString].v.toString)
                 char <- Try(p.map("char").asInstanceOf[JelloString].v.toString)}
              yield char match {
                case "cat" => Cat(name, hp)
                case "mouse" => Mouse(name, hp)
              }
        }
      }
    }
  }

  implicit val writer: JelloWriter[GameCharacter] = {
    new JelloWriter[GameCharacter] {
      override def write(v: GameCharacter): JelloValue = {
        JelloObject(Map("hp" -> JelloNumber(v.hp), "name" -> JelloString(v.username),
          "char" -> JelloString(v.charString)))
      }
    }
  }
}

object Mouse {
  val probability = 1
}

object Cat {
  val probability = 1
}


case class Mouse(username: String, var hp: Int = 1) extends GameCharacter {
  def charString = "mouse"
}

case class Cat(username: String, var hp: Int = 9) extends GameCharacter {
  def charString = "cat"
}
