
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._
import cats.Monoid
import cats.implicits._
import Point._
import PathElement._

import scala.collection.mutable

/**
  * Created by am_dev on 5/4/17.
  */
object ch9 {
  // this shape method does not work, do not know why
  def shape(radius: Int, n: Int, initialAngle: Angle): Image={
    def loop (count: Int, rotation: Angle): List[PathElement] = {
      n match {
        case 0 => Nil
        case n => LineTo(polar(radius, rotation * n + initialAngle)) :: loop(n - 1, rotation)
      }
    }
    closedPath(MoveTo(polar(radius, 0.degrees)) :: loop(n, 360.degrees / n))
  }

  def ones(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case n => 1 :: ones(n - 1)
    }
  }

  def ascending(n: Int): List[Int] = {
    def loop(n: Int, count: Int): List[Int] = {
      n match {
        case 0 => Nil
        case n => count :: loop(n - 1, count + 1)
      }
    }
    loop(n, 1)
  }

  def doubleList(list: List[Int]): List[Int]={
    list.map(_ * 2)
  }

  def betterShape(side: Int, size: Int, initialRotation: Angle): Image ={
    val step = (Angle.one / side).toDegrees
    val path = (0.0 to 360.0 by step).toList.map(i => LineTo(polar(size, initialRotation + i.degrees)))
    closedPath(MoveTo(polar(size, initialRotation)) :: path)
  }
}


