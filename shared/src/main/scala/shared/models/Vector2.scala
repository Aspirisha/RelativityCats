package shared.models

import com.uniformlyrandom.jello.JelloValue.{JelloNumber, JelloObject}
import com.uniformlyrandom.jello.{JelloFormat, JelloReader, JelloValue, JelloWriter}

import scala.util.Try

/**
  * Created by andy on 12/24/16.
  */

class Vector2(private var _x: Int, private var _y: Int) {
  def x = _x
  def y = _y

  def +(v: Vector2) = {var c = Vector2(x, y); c += v; c}

  def +=(v: Vector2) = {
    this._x += v.x
    this._y += v.y
    ()
  }

  def -(v: Vector2) = {var c = Vector2(x, y); c -= v; c}

  def -=(v: Vector2) = {
    this._x -= v.x
    this._y -= v.y
    ()
  }

  def *(factor: Int) = {
    var c = Vector2(x, y)
    c *= factor
    c
  }

  def *=(factor: Int) = {
    this._x *= factor
    this._y *= factor
    ()
  }

  def unary_- : Vector2 = Vector2(-x, -y)

  def magnitude = math.abs(x) + math.abs(y)
}

object Vector2
{
  implicit class FloatPlusAExtender (val x: Int) extends AnyVal {
    def * (v: Vector2) = v * x
  }

  def zero = new Vector2(0, 0)

  def unitX = new Vector2(1, 0)

  def unitY = new Vector2(0, 1)

  implicit def Tuple2IntToVector2(v: (Int, Int)): Vector2 = {
    new Vector2(v._1, v._2)
  }

  def apply(): Vector2 = {
    new Vector2(0, 0)
  }

  def apply(x: Int, y: Int): Vector2 = {
    new Vector2(x, y)
  }

  implicit val reader: JelloReader[Vector2] = {
    new JelloReader[Vector2] {
      override def read(jelloValue: JelloValue): Try[Vector2] = {
        jelloValue match {
          case p: JelloObject =>
            for {x <- Try(p.map("x").asInstanceOf[JelloNumber].v.toInt)
                y <- Try(p.map("y").asInstanceOf[JelloNumber].v.toInt)}
              yield Vector2(x, y)
        }
      }
    }
  }

  implicit val writer: JelloWriter[Vector2] = {
    new JelloWriter[Vector2] {
      override def write(v: Vector2): JelloValue = {
        JelloObject(Map("x" -> JelloNumber(v.x), "y" -> JelloNumber(v.y)))
      }
    }
  }
}

