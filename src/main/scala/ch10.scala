
import doodle.core.Image._
import doodle.core.PathElement._
import doodle.core.Point._
import doodle.core._
import doodle.syntax._
import doodle.turtle.Instruction._
import doodle.turtle._
import sun.security.util.Length

/**
  * Created by am_dev on 5/4/17.
  */
object ch10 {
  val stepSize = 10

  def rule (i: Instruction): List[Instruction] =
  i match {
    case Forward(_) => List(forward(stepSize),forward(stepSize))
    case NoOp =>
      List (branch(turn(45.degrees), forward(stepSize), noop),
        branch(turn(-45.degrees), forward(stepSize), noop))
    case other => List(other)
  }

  def double[A] (i: List[A]): List[A] ={
    i.flatMap(i => List(i ,i))
  }

  def nothing[A] (i : List[A]): List[A]={
    i.flatMap(i => List())
  }

  def rewrite(in: List[Instruction], rule: Instruction => List[Instruction]) : List[Instruction] ={
    in.flatMap(i =>
      i match{
        case Branch(i) => List(branch(rewrite(i, rule):_*))
        case other => rule(other)
      }
    )
  }

  def lSystem(step: Int, start: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] ={
    step match  {
      case 0 => start
      case n => lSystem(n-1, rewrite(start, rule), rule)
    }
  }

  def flatPolygon(side: Int, length: Double): Image ={
    val step = Angle.one / side
    Turtle.draw(
      (1 to side).toList.flatMap(
        i => List(turn(step), forward(length))
      )
    )
  }

  def flatSpiral(step: Int, len: Double, angle: Angle, in: Double): Image={
    Turtle.draw(
      (1 to step).toList.flatMap(
        i => List(forward(len + i * in), turn(angle + 0.5.degrees))
      )
    )
  }

  def start: List[Instruction] = {
    List(forward(10),
      branch(turn(45.degrees),forward(10)),
      branch(turn(-45.degrees),forward(10))
    )
  }

  val lMap = Turtle.draw(lSystem(20, start, rule))
}


