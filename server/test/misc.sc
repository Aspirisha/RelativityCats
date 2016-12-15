import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import models._
import play.api.libs.iteratee.{Enumerator, Iteratee}

import scala.concurrent.duration.Duration

val f = Future {
  2 / 0
}

f onFailure {
  case npe: NullPointerException =>
    println("I'd be amazed if this printed out.")
}

f onComplete {
  case x => println("I'd be amazed if this printed out.")
}

println(12)



def printChunks: Iteratee[Int, Unit] =
  Iteratee.foreach[Int]( s => println(s) )
val enum = Enumerator(1,2,3,5)
Await.result(enum.run(printChunks), Duration(10, "seconds"))