import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._

/**
  * Created by am_dev on 4/6/17.
  */
object Exercise {

  object c4Ex1 {
    val coloredTarget =
      Image.circle(10).fillColor(Color.red) on
        Image.circle(20).fillColor(Color.white) on
        Image.circle(30).fillColor(Color.red)

    val stand =
      Image.rectangle(6, 20) above
        Image.Rectangle(20, 6).fillColor(Color.brown)

    val ground = Image.rectangle(100, 20).lineWidth(0).fillColor(Color.green)

    val targetWithStand = coloredTarget above stand above ground
  }

  object c4Ex2 {
    val roof = Image.triangle(50, 30).fillColor(Color.brown)
    val wall = Image.rectangle(50, 50).fillColor(Color.red)
    val door = Image.rectangle(10, 20).fillColor(Color.red)  above Image.rectangle(10, 30).fillColor(Color.black)
    val house = roof above (door on wall)

    val tree = Image.circle(20).fillColor(Color.green) above
      Image.rectangle(10, 60).fillColor(Color.brown)

    val groundBlock = Image.rectangle(20, 5).fillColor(Color.yellow) beside
      Image.rectangle(10, 5).fillColor(Color.black) above
      Image.rectangle(30, 10).fillColor(Color.black)
    val street = groundBlock beside groundBlock beside groundBlock

    val houseOnStreet = house beside tree above street

    val finalStreet = houseOnStreet beside houseOnStreet beside houseOnStreet

    var intStreet = finalStreet
    for (i  <- 1 to 10) {
      intStreet = intStreet beside houseOnStreet
    }
  }

  object c5Ex1 {

    val a = (2+2)
    println(a)
    val a2 = a + a

    val b = print(a)
  }

  object c5Ex2 {

    {println("1"); 1} + {println("2"); 2} + {println("3"); 3}

  }

  object c6Ex{
    def square(x : Int): Int = {
      println("test1")
      x * x
    }

    def halve (x: Double): Double = x / 2
  }

  object c7Ex{

    def stackBox(count: Int) : Image ={
      count match {
        case 0 => Image.rectangle(20, 20).fillColor(Color.red)
        case _ => Image.rectangle(20, 20).fillColor(Color.red) beside stackBox(count - 1)
      }
    }

    def cross (count: Int) : Image ={
      val unit = Image.circle(10)
      def loop (count : Int) : Image ={
        count match {
          case 0 => unit
          case _ => ((unit beside loop(count - 1) beside unit) above unit) below unit
        }
      }
      loop(count)
    }

    def sierTri (count: Int) : Image ={
      val unit = Image.triangle(10, 15)
      def loop (count: Int) : Image = {
        count match {
          case 0 => unit
          case _ => loop (count  - 1) above (loop (count  - 1) beside loop (count  - 1))
        }
      }
      loop(count)
    }

    def exDouble (n : Int) : Int ={
      n match {
        case 0 => 0
        case _ => 2 * exDouble(n - 1)
      }
    }

    def gradientBox (count: Int, color: Color, spinDegree: Int) : Image ={
      val unit = Image.rectangle(30,30)
      val spinAngle = Angle.degrees(spinDegree)
      def loop (count: Int, color: Color) : Image ={
        count match {
          case 0 => unit.fillColor(color)
          case _ => unit.fillColor(color) beside loop(count - 1, color.spin(spinAngle))
        }
      }
      loop(count, color)
    }

    def gradientCircles(count: Int, size: Int, color: Color, spinDegree: Int) : Image ={
      val spinAngle = Angle.degrees(spinDegree)

      def loop (count: Int, size: Int, color: Color) : Image = {
        count match {
          case 0 => Image.empty
          case _ => Image.circle(size).lineColor(color) on loop (count - 1, size + 7, color.spin(spinAngle))
        }
      }
      loop(count, size, color)
    }

  }
}
