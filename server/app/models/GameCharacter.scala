package models

import scala.util.Random

/**
  * Created by andy on 12/23/16.
  */

abstract class GameCharacter(username: String) {
  var hp: Int
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
}

object Mouse {
  val probability = 1
}

object Cat {
  val probability = 1
}


case class Mouse(username: String) extends GameCharacter(username) {
  override var hp: Int = 1
}

case class Cat(username: String) extends GameCharacter(username) {
  override var hp: Int = 9
}
