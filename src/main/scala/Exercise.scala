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

  }

  object c5Ex1 {

    val a = (2+2)
    println(a)
    a + a
    val b = print(a)
  }

  object c5Ex2 {

    {println("1"); 1} + {println("2"); 2} + {println("3"); 3}

  }

}